from flask import Flask, render_template, request
import ply.lex as lex
import ply.yacc as yacc
import pandas as pd

# Initialize Flask application
app = Flask(__name__)

# Token definitions for lexical analysis
tokens = (
	'ID', 'NUMBER', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE', 'SEMI', 'COMMA', 'ASSIGN', 'PRINT',
	'INT', 'RETURN'
)

# Regular expressions for tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMI = r';'
t_COMMA = r','
t_ASSIGN = r'='
t_ignore = ' \t'

def t_ID(t):
	r'[a-zA-Z_][a-zA-Z_0-9]*'
	if t.value == 'int':
		t.type = 'INT'
	elif t.value == 'printf':
		t.type = 'PRINT'
	elif t.value == 'return':
		t.type = 'RETURN'
	return t

def t_NUMBER(t):
	r'\d+'
	t.value = int(t.value)
	return t

def t_newline(t):
	r'\n+'
	t.lexer.lineno += len(t.value)

def t_error(t):
	print(f"Illegal character '{t.value[0]}'")
	t.lexer.skip(1)

lexer = lex.lex()

precedence = (
	('left', 'PLUS', 'MINUS'),
	('left', 'TIMES', 'DIVIDE'),
)

temp_count = 0
label_count = 1

def new_temp():
	global temp_count
	temp_name = f"t{temp_count}"
	temp_count += 1
	return temp_name

def new_label():
	global label_count
	label_name = f"L{label_count}"
	label_count += 1
	return label_name

# parsing rules and TAC generation
tac_code = {}

def add_to_matrix(operation, arg1=None, arg2=None, result=None):
	if not tac_code:
		tac_code['operation'] = []
		tac_code['arg1'] = []
		tac_code['arg2'] = []
		tac_code['result'] = []
	
	tac_code['operation'].append(operation)
	tac_code['arg1'].append(arg1)
	tac_code['arg2'].append(arg2)
	tac_code['result'].append(result)

def p_program(p):
	'''program : function_list'''
	p[0] = p[1]


def p_function_list(p):
	'''function_list : function_list function
		function_list : function'''
	if len(p) == 3:
		p[0] = p[1] + p[2]
	else:
		p[0] = p[1]


def p_function(p):
	'''function : INT ID LPAREN params RPAREN LBRACE declarations statements RBRACE'''
	function_label = new_label()
	add_to_matrix(operation="label", result=function_label)
	p[0] = [f"{function_label}:"] + p[4] + p[8]  

def p_params(p):
	'''params : param_list
			  | empty'''
	p[0] = p[1] if p[1] else []

def p_param_list(p):
	'''param_list : param_list COMMA param
				  | param'''
	if len(p) == 2:
		p[0] = [p[1]]
	else:
		p[0] = p[1] + [p[3]]

def p_param(p):
	'''param : INT ID'''
	p[0] = f"param {p[2]}"

def p_declarations(p):
	'''declarations : declarations declaration
					| empty'''
	p[0] = p[1] if p[1] else []

def p_declaration(p):
	'''declaration : INT ID ASSIGN expression SEMI
				   | INT ID SEMI'''
	if len(p) == 6:
		add_to_matrix(operation='=', arg1=p[4], result=p[2])
	else:
		add_to_matrix(operation='=', arg1='0', result=p[2])

def p_statements(p):
	'''statements : statements statement
				  | empty'''
	p[0] = p[1] if p[1] else []

def p_statement_expr(p):
	'''statement : ID ASSIGN expression SEMI
				| PRINT LPAREN expression RPAREN SEMI'''
	if p[1] == 'printf':
		add_to_matrix("param", p[3])
		
		# this basically means that there is only 1 argument ot this printf function below
		add_to_matrix("call", arg1="printf", arg2="1")
	else:
		add_to_matrix("=", result=p[1], arg1=p[3])


def p_statement_return(p):
	'''statement : RETURN expression SEMI'''
	add_to_matrix("return", arg1=p[2])

def p_expression_binop(p):
	'''expression : expression PLUS expression
				  | expression MINUS expression
				  | expression TIMES expression
				  | expression DIVIDE expression'''
	temp_left = p[1]
	temp_right = p[3]
	temp = new_temp()
	
	add_to_matrix(operation=p[2], arg1=p[1], arg2=p[3])
	p[0] = temp

def p_expression_group(p):
	'''expression : LPAREN expression RPAREN'''
	p[0] = p[2]

def p_expression_number(p):
	'''expression : NUMBER'''
	temp = new_temp()
	add_to_matrix("=", result=temp, arg1=p[1])
	p[0] = temp

def p_expression_id(p):
	'''expression : ID'''
	p[0] = p[1]

def p_expression_call(p):
	'''expression : ID LPAREN arguments RPAREN'''
	temp = new_temp()
	for arg in p[3]:
		# this line basically adds all the arguments to the argument expression call list
		add_to_matrix("param", arg1=arg)
		
	# the 2nd arg here is nothing but the no. of arguments in the call, which is in p[3]
	add_to_matrix("call", arg1=p[0], arg2=len(p[3]), result=temp)
	p[0] = temp

def p_arguments(p):
	'''arguments : arguments COMMA expression
				| expression'''
	if len(p) == 2:
		p[0] = [p[1]]
	else:
		p[0] = p[1] + [p[3]]

def p_empty(p):
	'''empty :'''
	pass

def p_error(p):
	if p:
		print(f"Syntax error at '{p.value}'")
	else:
		print("Syntax error at EOF")

parser = yacc.yacc()

def is_temp_var(val):
	if (type(val) == type('nice')) and (len(val) > 1) and (val[0] == 't') and (val[1].isdigit()):
		return val[1]

def get_rows(tac, code_type):
	quad_celldata = []
	values = list(tac.values())
	m, n = len(values), len(values[0])

	for j in range(n):
		row = []
		for i in range(m):
			val = values[i][j] if values[i][j] != None else ""
			row.append(val)
		quad_celldata.append(row)
	
	tri_celldata = []
	if code_type == 'tri':
		for j in range(n): 
			row = []	
			# this time we have to leave out the results heading
			for i in range(m-1):
				val = values[i][j] if values[i][j] != None else ""
				
				if (i > 0) and (num := is_temp_var(val)):
					print('here')
					val = f'({num})'
				row.append(val)
			tri_celldata.append(row)

		return tri_celldata

	else: # return 
		return quad_celldata


@app.route('/')
def home():
    return render_template('index.html')


@app.route('/generate', methods=['POST'])
def generate_code():
	if request.method == 'POST':
		code = request.form['code']
		code_type = request.form['code_type'] 
        
		tac_code.clear()
		
		parser.parse(code)
		
		print("TAC Code:", tac_code)
		celldata = get_rows(tac_code, code_type) 

		print(f"{code_type.upper()} Data:", celldata)
		
		headings = list(tac_code.keys())
		if code_type == 'tri':
			headings = headings[:-1]
		
		return render_template('result.html', headings=headings, tac_code=celldata, format_type='Triplet' if code_type == 'tri' else 'Quadruple')


if __name__ == '__main__':
    app.run(debug=True)

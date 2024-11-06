import ply.lex as lex
import ply.yacc as yacc

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

# Token rules for identifiers, numbers, keywords
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

# Precedence and associativity of operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS')
)

# TAC generation helpers
temp_count = 0

def new_temp():
    global temp_count
    temp_name = f"t{temp_count}"
    temp_count += 1
    return temp_name

# Parsing rules and TAC generation
tac_code = []

def p_program(p):
    '''program : INT ID LPAREN RPAREN block'''
    p[0] = p[5]

def p_block(p):
    '''block : LBRACE declarations statements RBRACE'''
    p[0] = p[2] + p[3]

def p_declarations(p):
    '''declarations : declarations declaration
                    | empty'''
    p[0] = p[1] if p[1] else []
    if len(p) > 2:
        p[0].append(p[2])

def p_declaration(p):
    '''declaration : INT ID ASSIGN expression SEMI
                   | INT ID SEMI'''
    if len(p) == 6:
        var_name = p[2]
        result = p[4]
        tac_code.append(f"{var_name} = {result}")
    else:
        tac_code.append(f"{p[2]} = 0")  # Default initialization to 0

def p_statements(p):
    '''statements : statements statement
                  | empty'''
    p[0] = p[1] if p[1] else []
    if len(p) > 2:
        p[0].append(p[2])

def p_statement_expr(p):
    '''statement : ID ASSIGN expression SEMI'''
    var_name = p[1]
    result = p[3]
    tac_code.append(f"{var_name} = {result}")

def p_statement_print(p):
    '''statement : PRINT LPAREN ID RPAREN SEMI'''
    tac_code.append(f"PRINT {p[3]}")

def p_statement_return(p):
    '''statement : RETURN expression SEMI'''
    result = p[2]
    tac_code.append(f"RETURN {result}")

def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    temp_left = p[1]
    temp_right = p[3]

    if not isinstance(p[1], str):  # Not already a temp variable
        temp_left = new_temp()
        tac_code.append(f"{temp_left} = {p[1]}")

    if not isinstance(p[3], str):  # Not already a temp variable
        temp_right = new_temp()
        tac_code.append(f"{temp_right} = {p[3]}")

    temp = new_temp()
    tac_code.append(f"{temp} = {temp_left} {p[2]} {temp_right}")
    p[0] = temp

def p_expression_group(p):
    '''expression : LPAREN expression RPAREN'''
    p[0] = p[2]

def p_expression_number(p):
    '''expression : NUMBER'''
    temp = new_temp()
    tac_code.append(f"{temp} = {p[1]}")
    p[0] = temp

def p_expression_id(p):
    '''expression : ID'''
    p[0] = p[1]

def p_expression_uminus(p):
    '''expression : MINUS expression %prec UMINUS'''
    temp_expr = p[2]

    if not isinstance(p[2], str):  # Not already a temp variable
        temp_expr = new_temp()
        tac_code.append(f"{temp_expr} = {p[2]}")

    temp = new_temp()
    tac_code.append(f"{temp} = - {temp_expr}")
    p[0] = temp

def p_empty(p):
    '''empty :'''
    pass

def p_error(p):
    print("Syntax error in input!")

parser = yacc.yacc()

# Collect input from the user
print("Enter C-like code (end input with 'EOF' on a new line):")
user_code = []
while True:
    line = input()
    if line == 'EOF':
        break
    user_code.append(line)
c_code = "\n".join(user_code)

# Parse the input code
parser.parse(c_code)

# Print the generated three-address code
print("\nGenerated Three-Address Code:")
for line in tac_code:
    print(line)

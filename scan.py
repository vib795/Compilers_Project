## This file has been created using the documentation from http://www.dabeaz.com/ply/ply.html
## Methods are available on the PLY website's documentation and usage page.

import ply.lex as lex
import sys

## taken from ply documentation 
reserved = {
    'if' : 'T_If',
    'else' : 'T_Else',
    'while' : 'T_While',
    'void' : 'T_Void',
    'int' : 'T_Int',
    'bool' : 'T_Bool',
    'string' : 'T_String',
    'double' : 'T_Double',
    'null' : 'T_Null',
    'for' : 'T_For',
    'return' : 'T_Return',
    'break' : 'T_Break',
    'Print' : 'T_Print',
    'ReadInteger' : 'T_ReadInt',
    'ReadLine' : 'T_ReadLine'
}

## taken from ply documentation 
tokens = ['OP', 'TAB' ,'UNTERMINATED_STRING', 'HASH', 'SINGLELINE_COMMENT' ,
'T_IntConstant', 'MULTILINE_COMMENT', 'T_StringConstant', 'T_BoolConstant' ,
'T_DoubleConstant','T_Identifier', 'SPACE', 'T_LessEqual', 'T_GreaterEqual', 
'T_Equal', 'T_NotEqual', 'T_And', 'T_Or'] + list(reserved.values())

# From lex.py in ply
# -----------------------------------------------------------------------------
# def _statetoken(s,names)
#
# Given a declaration name s of the form "t_" and a dictionary whose keys are
# state names, this function returns a tuple (states,tokenname) where states
# is a tuple of state names and tokenname is the name of the token.  For example,
# calling this with s = "t_foo_bar_SPAM" might return (('foo','bar'),'SPAM')
# -----------------------------------------------------------------------------
def t_T_Or(t):
    r'\|\|'
    t.type = reserved.get(t.value,'T_Or')    # Check for reserved words
    return t

def t_T_And(t):
    r'&&'
    t.type = reserved.get(t.value,'T_And')    # Check for reserved words
    return t

def t_T_NotEqual(t):
    r'!='
    t.type = reserved.get(t.value,'T_NotEqual')    # Check for reserved words
    return t

def t_T_LessEqual(t):
    r'<='
    t.type = reserved.get(t.value,'T_LessEqual')    # Check for reserved words
    return t

def t_T_Equal(t):
    r'=='
    t.type = reserved.get(t.value,'T_Equal')    # Check for reserved words
    return t

def t_T_GreaterEqual(t):
    r'>='
    t.type = reserved.get(t.value,'T_GreaterEqual')    # Check for reserved words
    return t

def t_SINGLELINE_COMMENT(t):
    r'\/\/.*\n' 
    t.lexer.lineno += 1

def t_MULTILINE_COMMENT(t):
    r'(/\*(.|\n)*?\*/)|(//.*)'
    for i in range(len(t.value)):
        if t.value[i] == '\n':
            t.lexer.lineno += 1
    
def t_T_StringConstant(t):
    r'\"[^\"\n]*\"'
    t.type = reserved.get(t.value,'T_StringConstant')    # Check for reserved words
    return t

def t_UNTERMINATED_STRING(t):
    r'(\"[^\"\n]*)'
    print("\n*** Error line {} \n*** Unterminated string constant:{} \n".format(t.lexer.lineno, t.value[0]))
    t.type = reserved.get(t.value,'UNTERMINATED_STRING')    # Check for reserved words
    return t


def t_HASH(t):
    r'\#.*'
    print("\n*** Error line {} \n*** Invalid # directive\n".format(t.lexer.lineno))
    t.type = reserved.get(t.value,'HASH')    # Check for reserved words
    return t

def t_T_BoolConstant(t):
    r'true|false'
    t.type = reserved.get(t.value,'T_BoolConstant')    # Check for reserved words
    t.value = t.value
    return t


def t_T_Identifier(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'T_Identifier')    # Check for reserved words
    return t

def t_T_DoubleConstant(t):
    r'[0-9]+\.[0-9]*((E|e)(\+|\-)?[0-9]+)?'
    val = float(t.value)
    t.value = val  
    return t

def t_T_IntConstant(t):
    r'[0-9]+|0[xX][0-9a-fA-F]+'
    t.value = t.value  
    return t

def t_OP(t):
    r'[!;,\.[\]{}()\-\+*/%<>=]'
    t.type = reserved.get(t.value,'OP')    # Check for reserved words
    t.value = t.value
    return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_SPACE(t):
    r'[ ]'
    t.type = reserved.get(t.value,'SPACE')
    t.value = t.value
    return t
    
def t_TAB(t):
    r'[\t]'
    t.type = reserved.get(t.value,'TAB')
    t.value = t.value
    return t


# Error handling rule
def t_error(t):
    print("t: ", t)
    print("\n*** Error line {}.\n*** Unrecognized char: '{}'\n".format(t.lexer.lineno, t.value[0]))
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

def process_input(raw_input):
    lexer.input(raw_input)
    return lexer

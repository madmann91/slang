#ifndef SLANG_TOK
#define SLANG_TOK(tok, str)
#endif

SLANG_TOK(DOT, ".")

SLANG_TOK(MUL, "*")
SLANG_TOK(DIV, "/")
SLANG_TOK(MOD, "%")
SLANG_TOK(ADD, "+")
SLANG_TOK(SUB, "-")

SLANG_TOK(LSHIFT, "<<")
SLANG_TOK(RSHIFT, ">>")

SLANG_TOK(LT,  "<")
SLANG_TOK(LEQ, "<=")
SLANG_TOK(GT,  ">")
SLANG_TOK(GEQ, ">=")
SLANG_TOK(EQ,  "==")
SLANG_TOK(NEQ, "!=")

SLANG_TOK(AND, "&")
SLANG_TOK(XOR, "^")
SLANG_TOK(OR,  "|")

SLANG_TOK(ANDAND, "&&")
SLANG_TOK(OROR,   "||")

SLANG_TOK(INC, "++")
SLANG_TOK(DEC, "--")
SLANG_TOK(NOT, "!")
SLANG_TOK(NEG, "~")

SLANG_TOK(QMARK, "?")
SLANG_TOK(COLON, ":")
SLANG_TOK(SEMICOLON, ";")
SLANG_TOK(COMMA, ",")

SLANG_TOK(ASSIGN,     "=")
SLANG_TOK(ASSIGN_ADD, "+=")
SLANG_TOK(ASSIGN_SUB, "-=")
SLANG_TOK(ASSIGN_MUL, "*=")
SLANG_TOK(ASSIGN_DIV, "/=")
SLANG_TOK(ASSIGN_MOD, "%=")

SLANG_TOK(ASSIGN_LSHIFT, "<<=")
SLANG_TOK(ASSIGN_RSHIFT, ">>=")

SLANG_TOK(ASSIGN_AND, "&=")
SLANG_TOK(ASSIGN_XOR, "^=")
SLANG_TOK(ASSIGN_OR,  "|=")

SLANG_TOK(IDENT,   "<ident>")
SLANG_TOK(LIT,     "<lit>")
SLANG_TOK(UNKNOWN, "<unknown>")

SLANG_TOK(LBRACE, "{")
SLANG_TOK(RBRACE, "}")

SLANG_TOK(LPAREN, "(")
SLANG_TOK(RPAREN, ")")

SLANG_TOK(LBRACKET, "[")
SLANG_TOK(RBRACKET, "]")

SLANG_TOK(EOF, "eof")

#undef SLANG_TOK

type token =
  | BOOL of (bool)
  | INT of (int)
  | FLOAT of (float)
  | NOT
  | MINUS
  | PLUS
  | MINUS_DOT
  | PLUS_DOT
  | AST_DOT
  | SLASH_DOT
  | EQUAL
  | LESS_GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | LESS
  | GREATER
  | IF
  | THEN
  | ELSE
  | IDENT of (Id.t)
  | LET
  | IN
  | REC
  | COMMA
  | ARRAY_CREATE
  | DOT
  | LESS_MINUS
  | SEMICOLON
  | LPAREN
  | RPAREN
  | EOF
  | AND
  | OR
  | XOR
  | FISZERO
  | FLESS
  | FISPOS
  | FISNEG
  | FNEG
  | FABS
  | FHALF
  | FSQR
  | FLOOR
  | FLOATOFINT
  | INTOFFLOAT
  | SQRT
  | COS
  | SIN
  | TAN
  | ATAN
  | READINT
  | READFLOAT
  | PRINTCHAR

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.t

{
(* lexerが利用する変数、関数、型などの定義 *)
open Parser
open Type
}

(* 正規表現の略記 *)
let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| "\n"
    { Lexing.new_line lexbuf; token lexbuf }
| "(*"
    { comment lexbuf; (* ネストしたコメントのためのトリック *)
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| digit+ (* 整数を字句解析するルール (caml2html: lexer_int) *)
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '-' (* -.より後回しにしなくても良い? 最長一致? *)
    { MINUS }
| '+' (* +.より後回しにしなくても良い? 最長一致? *)
    { PLUS }
| '*'
    { MUL }
| '/'
    { DIV }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "xor"
    { XOR }
| "&&"
    { AND }
| "||"
    { OR }
| "fiszero"
    { FISZERO }
| "fequal"
    { FEQUAL }
| "fless"
    { FLESS }
| "fispos"
    { FISPOS }
| "fisneg"
    { FISNEG }
| "fneg"
    { FNEG }
| "fabs"
    { FABS }
| "fhalf"
    { FHALF }
| "fsqr"
    { FSQR }
| "floor"
    { FLOOR }
| "float_of_int"
    { FLOATOFINT }
| "int_of_float"
    { INTOFFLOAT }
| "sqrt"
    { SQRT }
| "read_int"
    { READINT }
| "read_float"
    { READFLOAT }
| "print_int"
    { PRINTINT }
| "print_char"
    { PRINTCHAR }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| "Array.create" | "Array.make" | "create_array" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| "open"
    { OPEN }
| ";;" 
    { SEMISEMI }
| eof
    { EOF }
| lower (digit|lower|upper|'_')* (* 他の「予約語」より後でないといけない *)
    { IDENT(Lexing.lexeme lexbuf) }
| upper (digit|lower|upper|'_')* (* 他の「予約語」より後でないといけない *)
    { UIDENT(Lexing.lexeme lexbuf) }
| _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }

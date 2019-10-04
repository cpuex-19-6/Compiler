%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
let addtyp x = (x, Type.gentyp ())

let start_pos = Parsing.symbol_start_pos ()
%}

/* (* 字句を表すデータ型の定義 (caml2html: parser_token) *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF
%token XOR
%token FISZERO FLESS FISPOS FISNEG
%token FNEG FABS FHALF FSQR FLOOR FLOATOFINT INTOFFLOAT SQRT COS SIN TAN ATAN
%token READINT READFLOAT PRINTCHAR

/* (* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) *) */
%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* (* 開始記号の定義 *) */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* (* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Unit }
| BOOL
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Bool($1) }
| INT
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Int($1) }
| FLOAT
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Float($1) }
| IDENT
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Var($1) }
| simple_exp DOT LPAREN exp RPAREN
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Get($1, $4) }

exp: /* (* 一般の式 (caml2html: parser_exp) *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Not($2) }
| MINUS exp
    %prec prec_unary_minus
    { let (ln, e) = $2 in match e with
    | Float(f) -> ln, Float(-.f) (* -1.23などは型エラーではないので別扱い *)
    | e -> ln, Neg($2) }
| exp PLUS exp /* (* 足し算を構文解析するルール (caml2html: parser_add) *) */
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Add($1, $3) }
| exp MINUS exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Sub($1, $3) }
| exp EQUAL exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Eq($1, $3) }
| exp LESS_GREATER exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Not(let start = Parsing.symbol_start_pos () in start.pos_lnum, Eq($1, $3)) }
| exp LESS exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Not(let start = Parsing.symbol_start_pos () in start.pos_lnum, LE($3, $1)) }
| exp GREATER exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Not(let start = Parsing.symbol_start_pos () in start.pos_lnum, LE($1, $3)) }
| exp LESS_EQUAL exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, LE($1, $3) }
| exp GREATER_EQUAL exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, LE($3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, If($2, $4, $6) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FNeg($2) }
| exp PLUS_DOT exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FAdd($1, $3) }
| exp MINUS_DOT exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FSub($1, $3) }
| exp AST_DOT exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FMul($1, $3) }
| exp SLASH_DOT exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FDiv($1, $3) }
| XOR exp exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Xor($2,$3) }
| FISZERO exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FEq($2, (let start = Parsing.symbol_start_pos () in start.pos_lnum, Float(0.))) }
| FLESS exp exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FLE($2, $3) }
| FISPOS exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FLE((let start = Parsing.symbol_start_pos () in start.pos_lnum, Float(0.)), $2) }
| FISNEG exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FLE($2, (let start = Parsing.symbol_start_pos () in start.pos_lnum, Float(0.))) }
| FNEG exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FNeg($2) }
| FHALF exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FMul($2, (let start = Parsing.symbol_start_pos () in start.pos_lnum, Float(0.5))) }
| FSQR exp
   %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FSqr($2) }
| FABS exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FAbs($2) }
| FLOOR exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FFloor($2) }
| FLOATOFINT exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, ItoF($2) }
| INTOFFLOAT exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, FtoI($2) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Let(addtyp $2, $4, $6) }
| LET REC fundef IN exp
    %prec prec_let
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, LetRec($3, $5) }
| simple_exp actual_args
    %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, App($1, $2) }
| elems
    %prec prec_tuple
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Tuple($1) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, LetTuple($3, $6, $8) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Put($1, $4, $7) }
| exp SEMICOLON exp
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { let start = Parsing.symbol_start_pos () in start.pos_lnum, Array($2, $3) }
| error
    { failwith
    (let start_pos = Parsing.symbol_start_pos () in
        let end_pos = Parsing.symbol_end_pos () in
            Printf.sprintf "parse error (line: %d column: %d - line: %d column: %d)"
           (let start = Parsing.symbol_start_pos () in start.pos_lnum)
           (start_pos.pos_cnum - start_pos.pos_bol)
           (end_pos.pos_lnum)
           (end_pos.pos_cnum - end_pos.pos_bol)) }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }

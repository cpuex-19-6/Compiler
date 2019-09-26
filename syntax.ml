type t = (* MinCaml�ι�ʸ��ɽ������ǡ����� (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

(*変更部分*)
 let rec print_syntax outchan t n  = match t with 
  | Unit ->  Printf.fprintf outchan "%s" "UNIT\n";print_space outchan n
  | Bool(x) -> Printf.fprintf outchan "%s" ("BOOL  "^(string_of_bool(x))^"\n");print_space outchan n
  | Int(x) -> Printf.fprintf outchan "%s" ("INT  "^(string_of_int(x))^"\n");print_space outchan n
  | Float(x) -> Printf.fprintf outchan "%s" ("FLOAT  "^(string_of_float(x))^"\n");print_space outchan n
  | Not(x) -> Printf.fprintf outchan "%s" "NOT  ";print_syntax outchan x n
  | Neg(x) -> Printf.fprintf outchan "%s" "NEG  ";print_syntax outchan x n
  | Add(x,y) -> Printf.fprintf outchan "%s" "ADD\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | Sub(x,y) -> Printf.fprintf outchan "%s" "SUB\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | FNeg(x) -> Printf.fprintf outchan "%s" "FNEG  ";print_syntax outchan x n
  | FAdd(x,y) -> Printf.fprintf outchan "%s" "FADD\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | FSub(x,y) -> Printf.fprintf outchan "%s" "FSUB\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | FMul(x,y) -> Printf.fprintf outchan "%s" "FMUL\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | FDiv(x,y) -> Printf.fprintf outchan "%s" "FDIV\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | Eq(x,y) -> Printf.fprintf outchan "%s" "EQ\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | LE(x,y) -> Printf.fprintf outchan "%s" "LE\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | If(x,y,z) -> Printf.fprintf outchan "%s" "IF\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y (n+2);print_syntax outchan z n
  | Let((a,b),x,y) -> Printf.fprintf outchan "%s" "LET\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | Var(x) -> Printf.fprintf outchan "%s" "VAR  ";Printf.fprintf outchan "%s" (x^"\n");print_space outchan n
  | LetRec(x,y) -> Printf.fprintf outchan "%s" "LETREC\n";print_space outchan (n+2);print_idtype outchan x.name (n+2);print_idlist outchan x.args (n+2);print_syntax outchan x.body (n+2);print_syntax outchan y n
  | App(x,y) -> Printf.fprintf outchan "%s" "APP\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax_list outchan y n
  | Tuple(x) -> print_syntax_list outchan x n
  | LetTuple(x,y,z) -> Printf.fprintf outchan "%s" "LETTUPLE\n";print_space outchan (n+2);print_syntax outchan y (n+2);print_syntax outchan z (n+2)
  | Array(x,y) -> Printf.fprintf outchan "%s" "ARRAY\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | Get(x,y) -> Printf.fprintf outchan "%s" "Get\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y n
  | Put(x,y,z) -> Printf.fprintf outchan "%s" "PUT\n";print_space outchan (n+2);print_syntax outchan x (n+2);print_syntax outchan y (n+2);print_syntax outchan z n
and 
  print_syntax_list outchan t n= match t with
  |[] -> ()
  |x::xs -> print_syntax outchan x n;print_syntax_list outchan xs n
and
  print_idtype outchan t n = match t with
  |(a,b) -> Printf.fprintf outchan "%s\n" a;  Type.print_type outchan b n;print_space outchan n
and
  print_idlist outchan t n = match t with
  |[] -> ()
  |x::xs -> print_idtype outchan x n;print_idlist outchan xs n
and
  print_space outchan n = if n = 0 then () else (Printf.fprintf outchan  " %s" "";print_space outchan (n-1))
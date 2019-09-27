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
  | Unit ->  print_space outchan n;Printf.fprintf outchan "%s" "UNIT\n"
  | Bool(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("BOOL  "^(string_of_bool(x))^"\n")
  | Int(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("INT  "^(string_of_int(x))^"\n")
  | Float(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("FLOAT  "^(string_of_float(x))^"\n")
  | Not(x) -> print_space outchan n;Printf.fprintf outchan "%s" "NOT\n";print_syntax outchan x (n+2)
  | Neg(x) -> print_space outchan n;Printf.fprintf outchan "%s" "NEG\n";print_syntax outchan x (n+2)
  | Add(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "ADD\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | Sub(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "SUB\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | FNeg(x) -> print_space outchan n;Printf.fprintf outchan "%s" "FNEG\n";print_syntax outchan x (n+2)
  | FAdd(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "FADD\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | FSub(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "FSUB\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | FMul(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "FMUL\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | FDiv(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "FDIV\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | Eq(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "EQ\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | LE(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "LE\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | If(x,y,z) -> print_space outchan n;Printf.fprintf outchan "%s" "IF\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2);print_syntax outchan z (n+2)
  | Let((a,b),x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "LET\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | Var(x) -> print_space outchan n;Printf.fprintf outchan "%s" "VAR  ";Printf.fprintf outchan "%s" (x^"\n")
  | LetRec(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "LETREC\n";print_idtype outchan x.name (n+2);print_idlist outchan x.args (n+2);print_syntax outchan x.body (n+2);print_syntax outchan y (n+2)
  | App(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "APP\n";print_syntax outchan x (n+2);print_syntax_list outchan y (n+2)
  | Tuple(x) -> print_syntax_list outchan x n
  | LetTuple(x,y,z) -> print_space outchan n;Printf.fprintf outchan "%s" "LETTUPLE\n";print_syntax outchan y (n+2);print_syntax outchan z (n+2)
  | Array(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "ARRAY\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | Get(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "Get\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2)
  | Put(x,y,z) -> print_space outchan n;Printf.fprintf outchan "%s" "PUT\n";print_syntax outchan x (n+2);print_syntax outchan y (n+2);print_syntax outchan z (n+2)
and 
  print_syntax_list outchan t n= match t with
  |[] -> ()
  |x::xs -> print_syntax outchan x n;print_syntax_list outchan xs n
and
  print_idtype outchan t n = match t with
  |(a,b) -> Type.print_type outchan b n;Printf.fprintf outchan "  %s\n" a
and
  print_idlist outchan t n = match t with
  |[] -> ()
  |x::xs -> print_idtype outchan x n;print_idlist outchan xs n
and
  print_space outchan n = if n = 0 then () else (Printf.fprintf outchan  " %s" "";print_space outchan (n-1))
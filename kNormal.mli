type t = int * tt
and tt =
  | Unit
  | Int of int
  | Float of float
  | Xor of Id.t * Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | AndI of Id.t * int
  | FAbs of Id.t
  | FFloor of Id.t
  | ItoF of Id.t
  | FtoI of Id.t
  | FSqrt of Id.t
  | FEq of Id.t * Id.t
  | FLE of Id.t * Id.t
  | Read
  | FRead
  | Write of Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

val fv : t -> S.t
val f : Syntax.t -> t

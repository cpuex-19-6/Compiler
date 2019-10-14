(* give names to intermediate values (K-normalization) *)

type t = int * tt
and tt = (* K正規化後の式 (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | And of Id.t * Id.t
  | Or of Id.t * Id.t
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
  | ItoF of Id.t
  | FtoI of Id.t
  | FSqrt of Id.t
  | FEq of Id.t * Id.t
  | FLT of Id.t * Id.t
  | Read
  | FRead
  | Write of Id.t
  | IfEq of Id.t * Id.t * t * t (* 比較 + 分岐 (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* 比較 + 分岐 *)
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

let rec fv e =
  let (pos, ebody) = e in
  match ebody with (* 式に出現する（自由な）変数 (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) | Read | FRead-> S.empty
  | Neg(x) | FNeg(x) | AndI(x,_) | FAbs(x) | ItoF(x) | FtoI(x) | FSqrt(x) | Write(x)-> S.singleton x
  | And(x, y) | Or(x,y) | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) | FEq(x, y) | FLT(x, y)-> S.of_list [x; y]
  | IfEq(x, y, e1, e2) | IfLE(x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var(x) -> S.singleton x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App(x, ys) -> S.of_list (x :: ys)
  | Tuple(xs) | ExtFunApp(_, xs) -> S.of_list xs
  | Put(x, y, z) -> S.of_list [x; y; z]
  | LetTuple(xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

let insert_let ((pos, e), t) k = (* letを挿入する補助関数 (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let (pos', e'), t' = k x in
      (pos, Let((x, t), (pos, e), (pos', e'))), t'

let rec g env (pos, ebody) = 
  match ebody with (* K正規化ルーチン本体 (caml2html: knormal_g) *)
  | Syntax.Unit -> (pos, Unit), Type.Unit
  | Syntax.Bool(b) -> (pos, Int(if b then 1 else 0)), Type.Int (* 論理値true, falseを整数1, 0に変換 (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> (pos, Int(i)), Type.Int
  | Syntax.Float(d) -> (pos, Float(d)), Type.Float
  | Syntax.Not(e) -> g env (pos, Syntax.If(e, (pos, Syntax.Bool(false)), (pos, Syntax.Bool(true))))
  | Syntax.And(e1,e2) ->  
     insert_let (g env e1)
      (fun x -> insert_let (g env e2)
         (fun y -> (pos, And(x, y)), Type.Bool))
  | Syntax.Or(e1,e2) -> 
     insert_let (g env e1)
       (fun x -> insert_let (g env e2)
         (fun y -> (pos, Or(x, y)), Type.Bool))
  | Syntax.AndI(e1,e2) -> 
     insert_let (g env e1)
        (fun x -> (pos, AndI(x ,e2)), Type.Int)
  | Syntax.Neg(e) ->
      insert_let (g env e)
        (fun x -> (pos, Neg(x)), Type.Int)
  | Syntax.Add(e1, e2) -> (* 足し算のK正規化 (caml2html: knormal_add) *)
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
           (fun y -> (pos, Add(x, y)), Type.Int))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> (pos, Sub(x, y)), Type.Int))
  | Syntax.FNeg(e) ->
      insert_let (g env e)
        (fun x -> (pos, FNeg(x)), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> (pos, FAdd(x, y)), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> (pos, FSub(x, y)), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> (pos, FMul(x, y)), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> (pos, FDiv(x, y)), Type.Float))
  | Syntax.FAbs(e1) ->
      insert_let (g env e1)
      (fun x -> (pos,FAbs(x)), Type.Float)
  | Syntax.ItoF(e) -> 
      insert_let (g env e)
      (fun x -> (pos,ItoF(x)), Type.Float)
  | Syntax.FtoI(e) ->
      insert_let (g env e)
      (fun x -> (pos,FtoI(x)), Type.Float)
  | Syntax.FSqrt(e) ->
      insert_let (g env e)
      (fun x -> (pos,FSqrt(x)), Type.Float)
  | Syntax.Read -> 
      ((pos,Read),Type.Int)
  | Syntax.FRead -> 
      ((pos,FRead), Type.Float)
  | Syntax.Write(e) ->
      insert_let (g env e)
      (fun x -> (pos,Write(x)), Type.Unit)    
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (pos, Syntax.If((pos, cmp), (pos, Syntax.Bool(true)), (pos, Syntax.Bool(false))))
  (*| Syntax.FEq _ | Syntax.FLT _ as cmp ->
      g env (pos, Syntax.If((pos, cmp), (pos, Syntax.Bool(true)), (pos, Syntax.Bool(false))))*)
  | Syntax.FEq(e1, e2) ->
      insert_let (g env e1)
         (fun x -> insert_let (g env e2)
            (fun y -> (pos, FEq(x, y)), Type.Bool))
  | Syntax.FLT(e1, e2) -> 
       insert_let (g env e1)
          (fun x -> insert_let (g env e2)
            (fun y -> (pos, FLT(x, y)), Type.Bool))
  | Syntax.If((_, Syntax.Not(e1)), e2, e3) -> g env (pos, Syntax.If(e1, e3, e2)) (* notによる分岐を変換 (caml2html: knormal_not) *)
  | Syntax.If((_, Syntax.Eq(e1, e2)), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              (pos, IfEq(x, y, e3', e4')), t3))
  | Syntax.If((_, Syntax.LE(e1, e2)), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              (pos, IfLE(x, y, e3', e4')), t3))
  | Syntax.If(e1, e2, e3) -> g env (pos, Syntax.If((pos, Syntax.Eq(e1, (pos, Syntax.Bool(false)))), e3, e2)) (* 比較のない分岐を変換 (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      (pos, Let((x, t), e1', e2')), t2
  | Syntax.Var(x) when M.mem x env -> (pos, Var(x)), M.find x env
  | Syntax.Var(x) -> (* 外部配列の参照 (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> (pos, ExtArray x), t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      (pos, LetRec({ name = (x, t); args = yts; body = e1' }, e2')), t2
  | Syntax.App((_, Syntax.Var(f)), e2s) when not (M.mem f env) -> (* 外部関数の呼び出し (caml2html: knormal_extfunapp) *)
      (match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
          let rec bind xs = function (* "xs" are identifiers for the arguments *)
            | [] -> (pos, ExtFunApp(f, xs)), t
            | e2 :: e2s ->
                insert_let (g env e2)
                  (fun x -> bind (xs @ [x]) e2s) in
          bind [] e2s (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.App(e1, e2s) ->
      (match g env e1 with
      | _, Type.Fun(_, t) as g_e1 ->
          insert_let g_e1
            (fun f ->
              let rec bind xs = function (* "xs" are identifiers for the arguments *)
                | [] -> (pos, App(f, xs)), t
                | e2 :: e2s ->
                    insert_let (g env e2)
                      (fun x -> bind (xs @ [x]) e2s) in
              bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
        | [] -> (pos, Tuple(xs)), Type.Tuple(ts)
        | e :: es ->
            let _, t as g_e = g env e in
            insert_let g_e
              (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
        (fun y ->
          let e2', t2 = g (M.add_list xts env) e2 in
          (pos, LetTuple(xts, y, e2')), t2)
  | Syntax.Array(e1, e2) ->
      insert_let (g env e1)
        (fun x ->
          let _, t2 as g_e2 = g env e2 in
          insert_let g_e2
            (fun y ->
              let l =
                match t2 with
                | Type.Float -> "create_float_array"
                | _ -> "create_array" in
              (pos, ExtFunApp(l, [x; y])), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |        _, Type.Array(t) as g_e1 ->
          insert_let g_e1
            (fun x -> insert_let (g env e2)
                (fun y -> (pos, Get(x, y)), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> insert_let (g env e3)
                (fun z -> (pos, Put(x, y, z)), Type.Unit)))   

let f (pos, e) = fst (g M.empty (pos, e))

(* give names to intermediate values (K-normalization) *)

type t = (* K��������μ� (caml2html: knormal_t) *)
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t (* ��� + ʬ�� (caml2html: knormal_branch) *)
  | IfLE of Id.t * Id.t * t * t (* ��� + ʬ�� *)
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

(*変更部分*)
let rec print_normal outchan t n = match t with
  | Unit -> print_space outchan n;Printf.fprintf outchan "%s" "UNIT\n"
  | Int(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("INT  "^(string_of_int x)^"\n")
  | Float(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("FLOAT  "^(string_of_float x)^"\n")
  | Neg(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("NEG\n");print_id outchan x (n+2)
  | Add(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("ADD\n");print_id outchan x (n+2);print_id outchan y (n+2)
  | Sub(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("SUB\n");print_id outchan x (n+2);print_id outchan y (n+2)
  | FNeg(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("FNEG\n");print_id outchan x (n+2)
  | FAdd(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("FADD\n");print_id outchan x (n+2);print_id outchan y (n+2)
  | FSub(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("FSUB\n");print_id outchan x (n+2);print_id outchan y (n+2)
  | FMul(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("FMUL\n");print_id outchan x (n+2);print_id outchan y (n+2)
  | FDiv(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("FDIV\n");print_id outchan x (n+2);print_id outchan y (n+2)
  | IfEq(x,y,a,b) -> print_space outchan n;Printf.fprintf outchan "%s" ("IFEQ\n");print_id outchan x (n+2);print_id outchan y (n+2);print_normal outchan a (n+2);print_normal outchan b (n+2)
  | IfLE(x,y,a,b) -> print_space outchan n;Printf.fprintf outchan "%s" ("IFLE\n");print_id outchan x (n+2);print_id outchan y (n+2);print_normal outchan a (n+2);print_normal outchan b (n+2)
  | Let(x,y,z) -> print_space outchan n;Printf.fprintf outchan "%s" ("LET\n");print_idtype outchan x;print_normal outchan y (n+2);print_normal outchan z (n+2)
  | Var(x) -> print_space outchan n;Printf.fprintf outchan "%s" ("VAR\n");print_id outchan x (n+2)
  | LetRec(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" "LETREC\n";print_idtype outchan x.name (n+2);print_itlist outchan x.args;print_normal outchan x.body (n+2);print_normal outchan y (n+2)
  | App(x,y) -> print_space outchan n;Printf.fprintf outchan "%s" ("APP\n");print_id outchan x (n+2)
  | Tuple(x) -> print_space outchan n;Printf.fprintf outchan "%s" "Tuple\n";print_idlist outchan x (n+2)
  | LetTuple(x,y,z) -> print_space outchan n;Printf.fprintf outchan "%s" "LETTUPLE\n";print_itlist outchan x (n+2);print_id outchan y (n+2);print_normal outchan z (n+2)
  | _ -> ()
  and print_id outchan x n = print_space outchan n;Printf.fprintf outchan "%s\n" x
  and print_idlist outchan t n = match t with
   |[] -> ()
   |x::xs -> print_id outchan x n; print_idlist outchan xs n
  and print_idtype outchan t n = match t with
   |(a,b) -> Type.print_type outchan b n;Printf.fprintf outchan "  %s\n" a
  and print_itlist outchan t n = match t with
   |[] -> ()
   |x::xs -> print_idtype outchan x n;print_itlist outchan xs n
  and
    print_space outchan n = if n = 0 then () else (Printf.fprintf outchan  " %s" "";print_space outchan (n-1))

let rec fv = function (* ���˽и�����ʼ�ͳ�ʡ��ѿ� (caml2html: knormal_fv) *)
  | Unit | Int(_) | Float(_) | ExtArray(_) -> S.empty
  | Neg(x) | FNeg(x) -> S.singleton x
  | Add(x, y) | Sub(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | Get(x, y) -> S.of_list [x; y]
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

let insert_let (e, t) k = (* let��������������ؿ� (caml2html: knormal_insert) *)
  match e with
  | Var(x) -> k x
  | _ ->
      let x = Id.gentmp t in
      let e', t' = k x in
      Let((x, t), e, e'), t'

let rec g env = function (* K�������롼�������� (caml2html: knormal_g) *)
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool(b) -> Int(if b then 1 else 0), Type.Int (* ������true, false������1, 0���Ѵ� (caml2html: knormal_bool) *)
  | Syntax.Int(i) -> Int(i), Type.Int
  | Syntax.Float(d) -> Float(d), Type.Float
  | Syntax.Not(e) -> g env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) ->
      insert_let (g env e)
        (fun x -> Neg(x), Type.Int)
  | Syntax.Add(e1, e2) -> (* ­������K������ (caml2html: knormal_add) *)
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> Add(x, y), Type.Int))
  | Syntax.Sub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> Sub(x, y), Type.Int))
  | Syntax.FNeg(e) ->
      insert_let (g env e)
        (fun x -> FNeg(x), Type.Float)
  | Syntax.FAdd(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FAdd(x, y), Type.Float))
  | Syntax.FSub(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FSub(x, y), Type.Float))
  | Syntax.FMul(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FMul(x, y), Type.Float))
  | Syntax.FDiv(e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> FDiv(x, y), Type.Float))
  | Syntax.Eq _ | Syntax.LE _ as cmp ->
      g env (Syntax.If(cmp, Syntax.Bool(true), Syntax.Bool(false)))
  | Syntax.If(Syntax.Not(e1), e2, e3) -> g env (Syntax.If(e1, e3, e2)) (* not�ˤ��ʬ�����Ѵ� (caml2html: knormal_not) *)
  | Syntax.If(Syntax.Eq(e1, e2), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              IfEq(x, y, e3', e4'), t3))
  | Syntax.If(Syntax.LE(e1, e2), e3, e4) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y ->
              let e3', t3 = g env e3 in
              let e4', t4 = g env e4 in
              IfLE(x, y, e3', e4'), t3))
  | Syntax.If(e1, e2, e3) -> g env (Syntax.If(Syntax.Eq(e1, Syntax.Bool(false)), e3, e2)) (* ��ӤΤʤ�ʬ�����Ѵ� (caml2html: knormal_if) *)
  | Syntax.Let((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      Let((x, t), e1', e2'), t2
  | Syntax.Var(x) when M.mem x env -> Var(x), M.find x env
  | Syntax.Var(x) -> (* ��������λ��� (caml2html: knormal_extarray) *)
      (match M.find x !Typing.extenv with
      | Type.Array(_) as t -> ExtArray x, t
      | _ -> failwith (Printf.sprintf "external variable %s does not have an array type" x))
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      LetRec({ name = (x, t); args = yts; body = e1' }, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (M.mem f env) -> (* �����ؿ��θƤӽФ� (caml2html: knormal_extfunapp) *)
      (match M.find f !Typing.extenv with
      | Type.Fun(_, t) ->
          let rec bind xs = function (* "xs" are identifiers for the arguments *)
            | [] -> ExtFunApp(f, xs), t
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
                | [] -> App(f, xs), t
                | e2 :: e2s ->
                    insert_let (g env e2)
                      (fun x -> bind (xs @ [x]) e2s) in
              bind [] e2s) (* left-to-right evaluation *)
      | _ -> assert false)
  | Syntax.Tuple(es) ->
      let rec bind xs ts = function (* "xs" and "ts" are identifiers and types for the elements *)
        | [] -> Tuple(xs), Type.Tuple(ts)
        | e :: es ->
            let _, t as g_e = g env e in
            insert_let g_e
              (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple(xts, e1, e2) ->
      insert_let (g env e1)
        (fun y ->
          let e2', t2 = g (M.add_list xts env) e2 in
          LetTuple(xts, y, e2'), t2)
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
              ExtFunApp(l, [x; y]), Type.Array(t2)))
  | Syntax.Get(e1, e2) ->
      (match g env e1 with
      |        _, Type.Array(t) as g_e1 ->
          insert_let g_e1
            (fun x -> insert_let (g env e2)
                (fun y -> Get(x, y), t))
      | _ -> assert false)
  | Syntax.Put(e1, e2, e3) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2)
            (fun y -> insert_let (g env e3)
                (fun z -> Put(x, y, z), Type.Unit)))

let f e = fst (g M.empty e)

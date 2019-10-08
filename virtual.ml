(* translation into PowerPC assembly with infinite number of virtual registers *)

open Asm

let data = ref [] (* 浮動小数点数の定数テーブル (caml2html: virtual_data) *)

let classify xts ini addf addi =
  List.fold_left
    (fun acc (x, t) ->
      match t with
      | Type.Unit -> acc
      | Type.Float -> addf acc x
      | _ -> addi acc x t)
    ini
    xts

let separate xts =
  classify
    xts
    ([], [])
    (fun (int, float) x -> (int, float @ [x]))
    (fun (int, float) x _ -> (int @ [x], float))

let expand xts ini addf addi =
  classify
    xts
    ini
    (fun (offset, acc) x ->
      let offset = align offset in
      (offset + 8, addf x offset acc))
    (fun (offset, acc) x t ->
      (offset + 4, addi x t offset acc))

let rec g env (pos, ebody) =
  match ebody with (* 式の仮想マシンコード生成 (caml2html: virtual_g) *)
  | Closure.Unit -> Ans(pos, Nop)
  | Closure.Int(i) -> Ans(pos, Li(i))
  | Closure.Float(d) ->
      let l =
        try
          (* すでに定数テーブルにあったら再利用 *)
          let (l, _) = List.find (fun (_, d') -> d = d') !data in
          l
        with Not_found ->
          let l = Id.L(Id.genid "l") in
          data := (l, d) :: !data;
          l in
      Ans(pos, FLi(l))
  | Closure.Neg(x) -> Ans(pos, Neg(x))
  | Closure.And(x, y) -> Ans(pos, And(x, y))
  | Closure.Or(x, y) -> Ans(pos, Or(x, y))
  | Closure.AndI(x, y) -> Ans(pos, AndI(x, y))
  | Closure.FAbs(x) -> Ans(pos, FAbs(x))
  | Closure.ItoF(x) -> Ans(pos, ItoF(x))
  | Closure.FtoI(x) -> Ans(pos, FtoI(x))
  | Closure.FSqrt(x) -> Ans(pos, FSqrt(x))
  | Closure.FEq(x, y) -> Ans(pos, FEq(x, y))
  | Closure.FLT(x, y) -> Ans(pos, FLT(x,y))
  | Closure.Read -> Ans(pos, Read)
  | Closure.FRead -> Ans(pos, FRead)
  | Closure.Write(x) -> Ans(pos, Write(x))
  | Closure.Add(x, y) -> Ans(pos, Add(x, V(y)))
  | Closure.Sub(x, y) -> Ans(pos, Sub(x, V(y)))
  | Closure.FNeg(x) -> Ans(pos, FNeg(x))
  | Closure.FAdd(x, y) -> Ans(pos, FAdd(x, y))
  | Closure.FSub(x, y) -> Ans(pos, FSub(x, y))
  | Closure.FMul(x, y) -> Ans(pos, FMul(x, y))
  | Closure.FDiv(x, y) -> Ans(pos, FDiv(x, y))
  | Closure.IfEq(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(pos, IfEq(x, V(y), g env e1, g env e2))
      | Type.Float -> Ans(pos, IfFEq(x, y, g env e1, g env e2))
      | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE(x, y, e1, e2) ->
      (match M.find x env with
      | Type.Bool | Type.Int -> Ans(pos, IfLE(x, V(y), g env e1, g env e2))
      | Type.Float -> Ans(pos, IfFLE(x, y, g env e1, g env e2))
      | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.Let((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat pos e1' (x, t1) e2'
  | Closure.Var(x) ->
      (match M.find x env with
      | Type.Unit -> Ans(pos, Nop)
      | Type.Float -> Ans(pos, FMr(x))
      | _ -> Ans(pos, Mr(x)))
  | Closure.MakeCls((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) -> (* クロージャの生成 (caml2html: virtual_makecls) *)
      (* Closureのアドレスをセットしてから、自由変数の値をストア *)
      let e2' = g (M.add x t env) e2 in
      let offset, store_fv =
        expand
          (List.map (fun y -> (y, M.find y env)) ys)
          (4, e2')
          (fun y offset store_fv -> seq(pos, Stfd(y, x, C(offset)), store_fv))
          (fun y _ offset store_fv -> seq(pos, Stw(y, x, C(offset)), store_fv)) in
      Let(pos, (x, t), Mr(reg_hp),
          Let(pos, (reg_hp, Type.Int), Add(reg_hp, C(align offset)),
              let z = Id.genid "l" in
              Let(pos, (z, Type.Int), SetL(l),
                  seq(pos, Stw(z, x, C(0)),
                      store_fv))))
  | Closure.AppCls(x, ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(pos, CallCls(x, int, float))
  | Closure.AppDir(Id.L(x), ys) ->
      let (int, float) = separate (List.map (fun y -> (y, M.find y env)) ys) in
      Ans(pos, CallDir(Id.L(x), int, float))
  | Closure.Tuple(xs) -> (* 組の生成 (caml2html: virtual_tuple) *)
      let y = Id.genid "t" in
      let (offset, store) =
        expand
          (List.map (fun x -> (x, M.find x env)) xs)
          (pos, Ans(pos, Mr(y)))
          (fun x offset store -> seq(pos, Stfd(x, y, C(offset)), store))
          (fun x _ offset store -> seq(pos, Stw(x, y, C(offset)), store))  in
      Let(pos, (y, Type.Tuple(List.map (fun x -> M.find x env) xs)), Mr(reg_hp),
          Let(pos, (reg_hp, Type.Int), Add(reg_hp, C(align offset)),
              store))
  | Closure.LetTuple(xts, y, e2) ->
      let s = Closure.fv e2 in
      let (offset, load) =
        expand
          xts
          (pos, g (M.add_list xts env) e2)
          (fun x offset load ->
            if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
            fletd(pos, x, Lfd(y, C(offset)), load))
          (fun x t offset load ->
            if not (S.mem x s) then load else (* [XX] a little ad hoc optimization *)
            Let(pos, (x, t), Lwz(y, C(offset)), load)) in
      load
  | Closure.Get(x, y) -> (* 配列の読み出し (caml2html: virtual_get) *)
      let offset = Id.genid "o" in
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(pos, Nop)
      | Type.Array(Type.Float) ->
          Let(pos, (offset, Type.Int), Slw(y, C(3)),
              Ans(pos, Lfd(x, V(offset))))
      | Type.Array(_) ->
          Let(pos, (offset, Type.Int), Slw(y, C(2)),
              Ans(pos, Lwz(x, V(offset))))
      | _ -> assert false)
  | Closure.Put(x, y, z) ->
      let offset = Id.genid "o" in
      (match M.find x env with
      | Type.Array(Type.Unit) -> Ans(pos, Nop)
      | Type.Array(Type.Float) ->
          Let(pos, (offset, Type.Int), Slw(y, C(3)),
              Ans(pos, Stfd(z, x, V(offset))))
      | Type.Array(_) ->
          Let(pos, (offset, Type.Int), Slw(y, C(2)),
              Ans(pos, Stw(z, x, V(offset))))
      | _ -> assert false)
  | Closure.ExtArray(Id.L(x)) -> Ans(pos, SetL(Id.L("min_caml_" ^ x)))

(* 関数の仮想マシンコード生成 (caml2html: virtual_h) *)
let h { Closure.name = (Id.L(x), t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let (pos, ebody) = e in
  let (int, float) = separate yts in
  let (offset, load) =
    expand
      zts
      (4, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
      (fun z offset load -> fletd(pos, z, Lfd(x, C(offset)), load))
      (fun z t offset load -> Let(pos, (z, t), Lwz(x, C(offset)), load)) in
  match t with
  | Type.Fun(_, t2) ->
      { name = Id.L(x); args = int; fargs = float; body = load; ret = t2 }
  | _ -> assert false

(* プログラム全体の仮想マシンコード生成 (caml2html: virtual_f) *)
let f (Closure.Prog(fundefs, e)) =
  data := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog(!data, fundefs, e)

open Asm

(* for register coalescing *)
(* [XXX] Call�����ä��顢�����������̵��̣�Ȥ������ո��̤ʤΤ��ɤ�ʤ���
         ���Τ���ˡ�Call�����ä����ɤ����פ��֤��ͤ���1���Ǥ˴ޤ�롣 *)
let rec target' src (dest, t) = function
  | Mr(x) when x = src && is_reg dest ->
      assert (t <> Type.Unit);
      assert (t <> Type.Float);
      false, [dest]
  | FMr(x) when x = src && is_reg dest ->
      assert (t = Type.Float);
      false, [dest]
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2) | IfGE(_, _, e1, e2)
  | IfFEq(_, _, e1, e2) | IfFLE(_, _, e1, e2) ->
      let c1, rs1 = target src (dest, t) e1 in
      let c2, rs2 = target src (dest, t) e2 in
      c1 && c2, rs1 @ rs2
  | CallCls(x, ys, zs) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs @
             if x = src then [reg_cl] else [])
  | CallDir(_, ys, zs) ->
      true, (target_args src regs 0 ys @
             target_args src fregs 0 zs)
  | _ -> false, []
and target src dest = function (* register targeting (caml2html: regalloc_target) *)
  | Ans(_, exp) -> target' src dest exp
  | Let(_, xt, exp, e) ->
      let c1, rs1 = target' src xt exp in
      if c1 then true, rs1 else
      let c2, rs2 = target src dest e in
      c2, rs1 @ rs2
and target_args src all n = function (* auxiliary function for Call *)
  | [] -> []
  | y :: ys when src = y -> all.(n) :: target_args src all (n + 1) ys
  | _ :: ys -> target_args src all (n + 1) ys

type alloc_result = (* alloc�ˤ�����spilling�����ä����ɤ�����ɽ���ǡ����� *)
  | Alloc of Id.t (* allocated register *)
  | Spill of Id.t (* spilled variable *)
let rec alloc dest cont regenv x t =
  (* allocate a register or spill a variable *)
  assert (not (M.mem x regenv));
  let all =
    match t with
    | Type.Unit -> ["%r0"] (* dummy *)
    | Type.Float -> allfregs
    | _ -> allregs in
  if all = ["%r0"] then Alloc("%r0") else (* [XX] ad hoc optimization *)
  if is_reg x then Alloc(x) else
  let free = fv cont in
  try
    let (c, prefer) = target x dest cont in
    let live = (* �����Ƥ���쥸���� *)
      List.fold_left
        (fun live y ->
          if is_reg y then S.add y live else
          try S.add (M.find y regenv) live
          with Not_found -> live)
        S.empty
        free in
    let r = (* �����Ǥʤ��쥸������õ�� *)
      List.find
        (fun r -> not (S.mem r live))
        (prefer @ all) in
    (* Format.eprintf "allocated %s to %s@." x r; *)
    Alloc(r)
  with Not_found ->
    Format.eprintf "register allocation failed for %s@." x;
    let y = (* ���ι礦�쥸�����ѿ���õ�� *)
      List.find
        (fun y ->
          not (is_reg y) &&
          try List.mem (M.find y regenv) all
          with Not_found -> false)
        (List.rev free) in
    Format.eprintf "spilling %s from %s@." y (M.find y regenv);
    Spill(y)

(* auxiliary function for g and g'_and_restore *)
let add x r regenv =
  if is_reg x then (assert (x = r); regenv) else
  M.add x r regenv

(* auxiliary functions for g' *)
exception NoReg of Id.t * Type.t
let find x t regenv =
  if is_reg x then x else
  try M.find x regenv
  with Not_found -> raise (NoReg(x, t))
let find' x' regenv =
  match x' with
  | V(x) -> V(find x Type.Int regenv)
  | c -> c

let rec g dest cont regenv = function (* ̿����Υ쥸����������� (caml2html: regalloc_g) *)
  | Ans(pos, exp) -> g'_and_restore pos dest cont regenv exp
  | Let(pos, ((x, t) as xt), exp, e) ->
      assert (not (M.mem x regenv));
      let cont' = concat pos e dest cont in
      let (e1', regenv1) = g'_and_restore pos xt cont' regenv exp in
      (match alloc dest cont' regenv1 x t with
      | Spill(y) ->
          let r = M.find y regenv1 in
          let (e2', regenv2) = g dest cont (add x r (M.remove y regenv1)) e in
          let save =
            try Save(M.find y regenv, y)
            with Not_found -> Nop in
          (seq(pos, save, concat pos e1' (r, t) e2'), regenv2)
      | Alloc(r) ->
          let (e2', regenv2) = g dest cont (add x r regenv1) e in
          (concat pos e1' (r, t) e2', regenv2))
and g'_and_restore pos dest cont regenv exp = (* ���Ѥ�����ѿ��򥹥��å�����쥸������Restore (caml2html: regalloc_unspill) *)
  try g' pos dest cont regenv exp
  with NoReg(x, t) ->
    ((* Format.eprintf "restoring %s@." x; *)
     g dest cont regenv (Let(pos, (x, t), Restore(x), Ans(pos, exp))))
and g' pos dest cont regenv = function (* ��̿��Υ쥸����������� (caml2html: regalloc_gprime) *)
  | Nop | Li _ | SetL _ | Comment _ | Restore _ | FLi _ | Read | FRead as exp -> (Ans(pos, exp), regenv)
  | Mr(x) -> (Ans(pos, Mr(find x Type.Int regenv)), regenv)
  | And(x, y) -> (Ans(pos, And(find x Type.Bool regenv, find y Type.Bool regenv)), regenv)
  | Or(x, y) -> (Ans(pos, Or(find x Type.Bool regenv, find y Type.Bool regenv)), regenv)
  | Xor(x, y) -> (Ans(pos, Xor(find x Type.Bool regenv, find y Type.Bool regenv)), regenv)
  | AndI(x, y) -> (Ans(pos, AndI(find x Type.Int regenv, y)), regenv)
  | FAbs(x) -> (Ans(pos, FAbs(find x Type.Float regenv)), regenv)
  | ItoF(x) -> (Ans(pos, ItoF(find x Type.Int regenv)), regenv)
  | FtoI(x) -> (Ans(pos, FtoI(find x Type.Float regenv)), regenv)
  | FSqrt(x) -> (Ans(pos, FSqrt(find x Type.Float regenv)), regenv)
  | FFloor(x) -> (Ans(pos, FFloor(find x Type.Float regenv)), regenv)
  | FEq(x, y) -> (Ans(pos, FAdd(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FLT(x, y) -> (Ans(pos, FLT(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | Write(x) -> (Ans(pos, Write(find x Type.Int regenv)), regenv)
  | Neg(x) -> (Ans(pos, Neg(find x Type.Int regenv)), regenv)
  | Add(x, y') -> (Ans(pos, Add(find x Type.Int regenv, find' y' regenv)), regenv)
  | Sub(x, y') -> (Ans(pos, Sub(find x Type.Int regenv, find' y' regenv)), regenv)
  | Mul(x, y) -> (Ans(pos, Div(find x Type.Int regenv, find' y regenv)), regenv)
  | Div(x, y) -> (Ans(pos, Div(find x Type.Int regenv, find' y regenv)), regenv)
  | Rem(x, y) -> (Ans(pos, Rem(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | Array(x, y) -> (Ans(pos, Array(find x Type.Int regenv, find y Type.Int regenv)), regenv)
  | FArray(x, y) -> (Ans(pos, FArray(find x Type.Int regenv, find y Type.Float regenv)), regenv)
  | Slw(x, y') -> (Ans(pos, Slw(find x Type.Int regenv, find' y' regenv)), regenv)
  | Sra(x, y') -> (Ans(pos, Sra(find x Type.Int regenv, find' y' regenv)), regenv)
  | Lwz(x, y') -> (Ans(pos, Lwz(find x Type.Int regenv, find' y' regenv)), regenv)
  | Stw(x, y, z') -> (Ans(pos, Stw(find x Type.Int regenv, find y Type.Int regenv, find' z' regenv)), regenv)
  | FMr(x) -> (Ans(pos, FMr(find x Type.Float regenv)), regenv)
  | FNeg(x) -> (Ans(pos, FNeg(find x Type.Float regenv)), regenv)
  | FAdd(x, y) -> (Ans(pos, FAdd(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FSub(x, y) -> (Ans(pos, FSub(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FMul(x, y) -> (Ans(pos, FMul(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | FDiv(x, y) -> (Ans(pos, FDiv(find x Type.Float regenv, find y Type.Float regenv)), regenv)
  | Lfd(x, y') -> (Ans(pos, Lfd(find x Type.Int regenv, find' y' regenv)), regenv)
  | Stfd(x, y, z') -> (Ans(pos, Stfd(find x Type.Float regenv, find y Type.Int regenv, find' z' regenv)), regenv)
  | IfEq(x, y', e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfEq(find x Type.Int regenv, find' y' regenv, e1', e2')) e1 e2
  | IfLE(x, y', e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfLE(find x Type.Int regenv, find' y' regenv, e1', e2')) e1 e2
  | IfGE(x, y', e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfGE(find x Type.Int regenv, find' y' regenv, e1', e2')) e1 e2
  | IfFEq(x, y, e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfFEq(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2
  | IfFLE(x, y, e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfFLE(find x Type.Float regenv, find y Type.Float regenv, e1', e2')) e1 e2
  | IfZ(x, e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfZ(find x Type.Int regenv, e1', e2')) e1 e2
  | IfPos(x, e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfPos(find x Type.Int regenv, e1', e2')) e1 e2
  | IfNeg(x, e1, e2) as exp -> g'_if pos dest cont regenv exp (fun e1' e2' -> IfNeg(find x Type.Int regenv, e1', e2')) e1 e2
  | CallCls(x, ys, zs) as exp ->
      if List.length ys > Array.length regs - 2 || List.length zs > Array.length fregs - 1 then
        failwith (Format.sprintf "cannot allocate registers for arugments to %s" x)
      else
        g'_call pos dest cont regenv exp (fun ys zs -> CallCls(find x Type.Int regenv, ys, zs)) ys zs
  | CallDir(Id.L(x), ys, zs) as exp ->
      if List.length ys > Array.length regs - 1 || List.length zs > Array.length fregs - 1 then
        failwith (Format.sprintf "cannot allocate registers for arugments to %s" x)
      else
        g'_call pos dest cont regenv exp (fun ys zs -> CallDir(Id.L(x), ys, zs)) ys zs
  | Save(x, y) -> assert false
and g'_if pos dest cont regenv exp constr e1 e2 = (* if�Υ쥸����������� (caml2html: regalloc_if) *)
  let (e1', regenv1) = g dest cont regenv e1 in
  let (e2', regenv2) = g dest cont regenv e2 in
  let regenv' = (* ξ���˶��̤Υ쥸�����ѿ��������� *)
    List.fold_left
      (fun regenv' x ->
        try
          if is_reg x then regenv' else
          let r1 = M.find x regenv1 in
          let r2 = M.find x regenv2 in
          if r1 <> r2 then regenv' else
          M.add x r1 regenv'
        with Not_found -> regenv')
      M.empty
      (fv cont) in
  (List.fold_left
     (fun e x ->
       if x = fst dest || not (M.mem x regenv) || M.mem x regenv' then e else
       seq(pos, Save(M.find x regenv, x), e)) (* �����Ǥʤ��ѿ���ʬ��ľ���˥����� *)
     (Ans(pos, constr e1' e2'))
     (fv cont),
   regenv')
and g'_call pos dest cont regenv exp constr ys zs = (* �ؿ��ƤӽФ��Υ쥸����������� (caml2html: regalloc_call) *)
  (List.fold_left
     (fun e x ->
       if x = fst dest || not (M.mem x regenv) then e else
       seq(pos, Save(M.find x regenv, x), e))
     (Ans(pos, constr
            (List.map (fun y -> find y Type.Int regenv) ys)
            (List.map (fun z -> find z Type.Float regenv) zs)))
     (fv cont),
   M.empty)

let h { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } = (* �ؿ��Υ쥸����������� (caml2html: regalloc_h) *)
  let pos = match e with
  | Ans(n, _) -> n
  | Let(n, _, _, _) -> n in 
  let regenv = M.add x reg_cl M.empty in
  let (i, arg_regs, regenv) =
    List.fold_left
      (fun (i, arg_regs, regenv) y ->
        let r = regs.(i) in
        (i + 1,
         arg_regs @ [r],
         (assert (not (is_reg y));
          M.add y r regenv)))
      (0, [], regenv)
      ys in
  let (d, farg_regs, regenv) =
    List.fold_left
      (fun (d, farg_regs, regenv) z ->
        let fr = fregs.(d) in
        (d + 1,
         farg_regs @ [fr],
         (assert (not (is_reg z));
          M.add z fr regenv)))
      (0, [], regenv)
      zs in
  let a =
    match t with
    | Type.Unit -> Id.gentmp Type.Unit
    | Type.Float -> fregs.(0)
    | _ -> regs.(0) in
  let (e', regenv') = g (a, t) (Ans(pos, Mr(a))) regenv e in
  { name = Id.L(x); args = arg_regs; fargs = farg_regs; body = e'; ret = t }

let f (Prog(data, fundefs, e)) = (* �ץ���������ΤΥ쥸����������� (caml2html: regalloc_f) *)
  Format.eprintf "register allocation: may take some time (up to a few minutes, depending on the size of functions)@.";
  let fundefs' = List.map h fundefs in
  let e', regenv' = g (Id.gentmp Type.Unit, Type.Unit) (Ans(0, Nop)) M.empty e in
  Prog(data, fundefs', e')

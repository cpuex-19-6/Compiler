open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* ���Ǥ�Save���줿�ѿ��ν��� (caml2html: emit_stackset) *)
let stackmap = ref [] (* Save���줿�ѿ��Ρ������å��ˤ�������� (caml2html: emit_stackmap) *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
    stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = 4 * List.hd (locate x)
let stacksize () = align ((List.length !stackmap + 1) * 4)

let reg r =
  if is_reg r
  then String.sub r 1 (String.length r - 1)
  else r

let load_label r label =
  let r' = reg r in
  Printf.sprintf
    "\tlis\t%s, ha16(%s)\n\taddi\t%s, %s, lo16(%s)\n"
    r' label r' r' label


(* ���bit�ʲ���12bit����¦�Ρ� *)
let upper n = n asr 12 + if n land (1 lsl 11) = 0 then 0 else 1
(* ����12bit *)
let lower n = (n lsl 51) asr 51


(* �Կ��򥫥���ȤǤ���褦�ˤ��� *)
let pc = ref 0
let pcincr () = let n = !pc in pc := n + 2; n

(* �ؿ��ƤӽФ��Τ���˰������¤��ؤ���(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
                                         (function
                                           | (y', z) when y = y' -> (sw, z)
                                           | yz -> yz)
                                         xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* �������ɤ�����ɽ���ǡ����� (caml2html: emit_dest) *)
let rec g oc = function (* ̿����Υ�����֥����� (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function (* ��̿��Υ�����֥����� (caml2html: emit_gprime) *)
  (* �����Ǥʤ��ä���׻���̤�dest�˥��å� (caml2html: emit_nontail) *)
  | NonTail(_), Nop -> ()
  | NonTail(x), Li(n) ->
      let u = upper n in
      let l = lower n in
      if u = 0 then
        Printf.fprintf oc "%d \taddi\t%s, x0, %d\n" (pcincr()) (reg x) l
      else
        (Printf.fprintf oc "%d \tlui\t%s, %d\n" (pcincr()) (reg x) u;
        if l != 0 then
          Printf.fprintf oc "%d \taddi\t%s, %s, %d\n"(pcincr()) (reg x) (reg x) l)
  | NonTail(x), FLi(Id.L(l)) ->
      (* TODO: Li ��Ʊ�ͤ˽񤭴��� *)
      let s = load_label (reg reg_tmp) l in
      Printf.fprintf oc "%d %s\tlfd\t%s, 0(%s)\n"(pcincr()) s (reg x) (reg reg_tmp);
  | NonTail(x), SetL(Id.L(y)) ->
      let s = load_label x y in
      Printf.fprintf oc "%s" s
  | NonTail(x), Mr(y) when x = y -> ()
  | NonTail(x), Mr(y) -> Printf.fprintf oc "%d \taddi\t%s, %s, 0\n" (pcincr()) (reg x) (reg y)
  | NonTail(x), Neg(y) -> Printf.fprintf oc "%d \tsub\t%s, x0, %s\n" (pcincr())(reg x) (reg y)
  | NonTail(x), Add(y, V(z)) -> Printf.fprintf oc "%d \tadd\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), Add(y, C(z)) -> Printf.fprintf oc "%d \taddi\t%s, %s, %d\n" (pcincr()) (reg x) (reg y) z
  | NonTail(x), Sub(y, V(z)) -> Printf.fprintf oc "%d \tsub\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), Sub(y, C(z)) -> Printf.fprintf oc "%d \taddi\t%s, %s, -%d\n" (pcincr()) (reg x) (reg y) z
  | NonTail(x), Slw(y, V(z)) -> Printf.fprintf oc "%d \tslw\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)(* TODO: RISC-V *)
  | NonTail(x), Slw(y, C(z)) -> Printf.fprintf oc "%d \tslwi\t%s, %s, %d\n" (pcincr()) (reg x) (reg y) z (* TODO: RISC-V *)
  | NonTail(x), Lwz(y, V(z)) -> Printf.fprintf oc "%d \tlwzx\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z) (* TODO: RISC-V *)
  | NonTail(x), Lwz(y, C(z)) -> Printf.fprintf oc "%d \tlw\t%s, %d(%s)\n" (pcincr()) (reg x) z (reg y)
  | NonTail(_), Stw(x, y, V(z)) -> Printf.fprintf oc "%d \tstwx\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(_), Stw(x, y, C(z)) -> Printf.fprintf oc "%d \tsw\t%s, %d(%s)\n" (pcincr()) (reg x) z (reg y)
  | NonTail(x), FMr(y) when x = y -> ()
  | NonTail(x), FMr(y) -> Printf.fprintf oc "%d \tfmr\t%s, %s\n" (pcincr()) (reg x) (reg y)
  | NonTail(x), FNeg(y) -> Printf.fprintf oc "%d \tfneg\t%s, %s\n" (pcincr()) (reg x) (reg y)
  | NonTail(x), FAdd(y, z) -> Printf.fprintf oc "%d \tfadd\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), FSub(y, z) -> Printf.fprintf oc "%d \tfsub\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), FMul(y, z) -> Printf.fprintf oc "%d \tfmul\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), FDiv(y, z) -> Printf.fprintf oc "%d \tfdiv\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), Lfd(y, V(z)) -> Printf.fprintf oc "%d \tlfdx\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(x), Lfd(y, C(z)) -> Printf.fprintf oc "%d \tlfd\t%s, %d(%s)\n" (pcincr())(reg x) z (reg y)
  | NonTail(_), Stfd(x, y, V(z)) -> Printf.fprintf oc "%d \tstfdx\t%s, %s, %s\n" (pcincr()) (reg x) (reg y) (reg z)
  | NonTail(_), Stfd(x, y, C(z)) -> Printf.fprintf oc "%d \tstfd\t%s, %d(%s)\n" (pcincr()) (reg x) z (reg y)
  | NonTail(_), Comment(s) -> Printf.fprintf oc "#\t%s\n" s
  (* ����β���̿��μ��� (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "%d \tsw\t%s, %d(%s)\n" (pcincr()) (reg x) (offset y) (reg reg_sp)
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      Printf.fprintf oc "%d \tstfd\t%s, %d(%s)\n" (pcincr()) (reg x) (offset y) (reg reg_sp)
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* �����β���̿��μ��� (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "%d \tlw\t%s, %d(%s)\n" (pcincr()) (reg x) (offset y) (reg reg_sp)
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "%d \tlfd\t%s, %d(%s)\n" (pcincr()) (reg x) (offset y) (reg reg_sp)
  (* �������ä���׻���̤����쥸�����˥��åȤ��ƥ꥿���� (caml2html: emit_tailret) *)
  | Tail, (Nop | Stw _ | Stfd _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "%d \tblr\n" (pcincr());
  | Tail, (Li _ | SetL _ | Mr _ | Neg _ | Add _ | Sub _ | Slw _ | Lwz _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "%d \tblr\n" (pcincr()) ;
  | Tail, (FLi _ | FMr _ | FNeg _ | FAdd _ | FSub _ | FMul _ | FDiv _ | Lfd _ as exp) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "%d \tblr\n" (pcincr());
  | Tail, (Restore(x) as exp) ->
      (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
      | _ -> assert false);
      Printf.fprintf oc "%d \tblr\n" (pcincr());
  | Tail, IfEq(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "beq" "bne"
  | Tail, IfEq(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;
      g'_tail_if oc e1 e2 "beq" "bne"
  | Tail, IfLE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "ble" "bgt"
  | Tail, IfLE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;
      g'_tail_if oc e1 e2 "ble" "bgt"
  | Tail, IfGE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "bge" "blt"
  | Tail, IfGE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;
      g'_tail_if oc e1 e2 "bge" "blt"
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "beq" "bne"
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);
      g'_tail_if oc e1 e2 "ble" "bgt"
  | NonTail(z), IfEq(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne"
  | NonTail(z), IfEq(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne"
  | NonTail(z), IfLE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt"
  | NonTail(z), IfLE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt"
  | NonTail(z), IfGE(x, V(y), e1, e2) ->
      Printf.fprintf oc "\tcmpw\tcr7, %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "blt"
  | NonTail(z), IfGE(x, C(y), e1, e2) ->
      Printf.fprintf oc "\tcmpwi\tcr7, %s, %d\n" (reg x) y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bge" "blt"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq" "bne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tfcmpu\tcr7, %s, %s\n" (reg x) (reg y);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "ble" "bgt"
  (* �ؿ��ƤӽФ��β���̿��μ��� (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* �����ƤӽФ� (caml2html: emit_tailcall) *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tlw\t%s, 0(%s)\n" (reg reg_sw) (reg reg_cl);
      Printf.fprintf oc "\tmtctr\t%s\n\tbctr\n" (reg reg_sw);
  | Tail, CallDir(Id.L(x), ys, zs) -> (* �����ƤӽФ� *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tb\t%s\n" x
  | NonTail(a), CallCls(x, ys, zs) ->
      Printf.fprintf oc "\tmflr\t%s\n" (reg reg_tmp);
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tsw\t%s, %d(%s)\n" (reg reg_tmp) (ss - 4) (reg reg_sp);
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tlw\t%s, 0(%s)\n" (reg reg_tmp) (reg reg_cl);
      Printf.fprintf oc "\tmtctr\t%s\n" (reg reg_tmp);
      Printf.fprintf oc "\tbctrl\n";
      Printf.fprintf oc "\taddi\t%s, %s, -%d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tlw\t%s, %d(%s)\n" (reg reg_tmp) (ss - 4) (reg reg_sp);
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\taddi\t%s, %s, 0\n" (reg a) (reg regs.(0))
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tfmr\t%s, %s\n" (reg a) (reg fregs.(0));
      Printf.fprintf oc "\tmtlr\t%s\n" (reg reg_tmp)
  | (NonTail(a), CallDir(Id.L(x), ys, zs)) ->
      Printf.fprintf oc "\tmflr\t%s\n" (reg reg_tmp);
      g'_args oc [] ys zs;
      let ss = stacksize () in
      Printf.fprintf oc "\tsw\t%s, %d(%s)\n" (reg reg_tmp) (ss - 4) (reg reg_sp);
      Printf.fprintf oc "\taddi\t%s, %s, %d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tbl\t%s\n" x;
      Printf.fprintf oc "\taddi\t%s, %s, -%d\n" (reg reg_sp) (reg reg_sp) ss;
      Printf.fprintf oc "\tlw\t%s, %d(%s)\n" (reg reg_tmp) (ss - 4) (reg reg_sp);
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\taddi\t%s, %s, 0\n" (reg a) (reg regs.(0))
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tfmr\t%s, %s\n" (reg a) (reg fregs.(0));
      Printf.fprintf oc "\tmtlr\t%s\n" (reg reg_tmp)
and g'_tail_if oc e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s\tcr7, %s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "# %s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\tcr7, %s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tb\t%s\n" b_cont;
  Printf.fprintf oc "# %s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "# %s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs =
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\taddi\t%s, %s, 0\n" (reg r) (reg y))
    (shuffle reg_sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tfmr\t%s, %s\n" (reg fr) (reg z))
    (shuffle reg_fsw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "# %s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  if data <> [] then
    (Printf.fprintf oc "\t.data\n\t.literal8\n";
     List.iter
       (fun (Id.L(x), d) ->
         Printf.fprintf oc "\t.align 3\n";
         Printf.fprintf oc "# %s:\t %f\n" x d;
         Printf.fprintf oc "\t.long\t%ld\n" (gethi d);
         Printf.fprintf oc "\t.long\t%ld\n" (getlo d))
       data);
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc "# main program starts\n";
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("_R_0"), e);
  Printf.fprintf oc "# main program ends\n";
  (* Printf.fprintf oc "\tmr\tr3, %s\n" regs.(0); *)
  Printf.fprintf oc "\tblr\n"

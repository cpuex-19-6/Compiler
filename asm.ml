(* PowerPC assembly with a few virtual instructions *)

type id_or_imm = V of Id.t | C of int
type t = (* 命令の列 (caml2html: sparcasm_t) *)
  | Ans of int * exp
  | Let of int * (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 (caml2html: sparcasm_exp) *)
  | Nop
  | Li of int
  | FLi of float
  | SetL of Id.l
  | And of Id.t * Id.t
  | Or of Id.t * Id.t
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
  | Mr of Id.t
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Slw of Id.t * id_or_imm
  | Lwz of Id.t * id_or_imm
  | Stw of Id.t * Id.t * id_or_imm
  | FMr of Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Lfd of Id.t * id_or_imm
  | Stfd of Id.t * Id.t * id_or_imm
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t (* 左右対称ではないので必要 *)
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save) *)
  | Restore of Id.t (* スタック変数から値を復元 (caml2html: sparcasm_restore) *)
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog) *)
type prog = Prog of (Id.l * float) list * fundef list * t

let fletd(n, x, e1, e2) = Let(n, (x, Type.Float), e1, e2)
let seq(n, e1, e2) = Let(n, (Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = (* Array.init 27 (fun i -> Printf.sprintf "_R_%d" i) *)
  [| "%x4"; "%x5"; "%x6"; "%x7"; "%x8"; "%x9"; "%x10";
     "%x11"; "%x12"; "%x13"; "%x14"; "%x15"; "%x16"; "%x17"; "%x18";
     "%x19"; "%x20"; "%x21"; "%x22"; "%x23"; "%x24"; "%x25"; "%x26";
     "%x27"; "%x28"; "%x29"; "%x30" |]
let fregs = Array.init 32 (fun i -> Printf.sprintf "%%f%d" i)
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1) (* closure address (caml2html: sparcasm_regcl) *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
let reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *)
let reg_sp = "%x2" (* stack pointer *)
let reg_hp = "%x3" (* heap pointer (caml2html: sparcasm_reghp) *)
let reg_tmp = "%x31" (* [XX] ad hoc *)
let is_reg x = (x.[0] = '%')

(* レジスタの使い方 *)
(* 0番: 常に0のレジスタ *)
(* 1番: リンクレジスタ *)
(* 2番: スタックポインタ *)
(* 3番: ヒープポインタ *)

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Li(_) | FLi(_) | SetL(_) | Comment(_) | Restore(_) -> []
  | Mr(x) | Neg(x) | FMr(x) | FNeg(x) | Save(x, _) -> [x]
  | Add(x, y') | Sub(x, y') | Slw(x, y') | Lfd(x, y') | Lwz(x, y') -> x :: fv_id_or_imm y'
  | Stw(x, y, z') | Stfd(x, y, z') -> x :: y :: fv_id_or_imm z'
  | And(x, y) | Or(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) | FEq(x, y) | FLT(x, y) -> [x; y]
  | IfEq(x, y', e1, e2) | IfLE(x, y', e1, e2) | IfGE(x, y', e1, e2) ->  x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | IfFEq(x, y, e1, e2) | IfFLE(x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(_, exp) -> fv_exp exp
  | Let(_, (x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat n e1 xt e2 =
  match e1 with
  | Ans(n', exp) -> Let(n', xt, exp, e2)
  | Let(n', yt, exp, e1') -> Let(n', yt, exp, concat n' e1' xt e2)

let align i = (if i mod 8 = 0 then i else i + 4)

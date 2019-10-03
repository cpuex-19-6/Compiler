(****************************************************************)
(*                                                              *)
(* Ray Tracing Program for (Mini) Objective Caml                *)
(*                                                              *)
(* Original Program by Ryoji Kawamichi                          *)
(* Arranged for Chez Scheme by Motohico Nanano                  *)
(* Arranged for Objective Caml by Y. Oiwa and E. Sumii          *)
(* Added diffuse ray tracer by Y.Ssugawara                      *)
(*                                                              *)
(****************************************************************)

(*NOMINCAML open MiniMLRuntime;;*)
(*NOMINCAML open Globals;;*)
(*MINCAML*) let true = 1 in
(*MINCAML*) let false = 0 in
(*MINCAML*) let rec xor x y = if x then not y else y in

(******************************************************************************
   �����若����ｃ�������ｃ��
 *****************************************************************************)

(* 膃���� *)
let rec sgn x =
  if fiszero x then 0.0
  else if fispos x then 1.0
  else -1.0
in

(* ��＞散篁����膃��垸��荵� *)
let rec fneg_cond cond x =
  if cond then x else fneg x
in

(* (x+y) mod 5 *)
let rec add_mod5 x y =
  let sum = x + y in
  if sum >= 5 then sum - 5 else sum
in

(******************************************************************************
   ���������������篏���������������������������ｃ��
 *****************************************************************************)

(*
let rec vecprint v =
  (o_param_abc m) inFormat.eprintf "(%f %f %f)" v.(0) v.(1) v.(2)
in
*)

(* ��や撮��� *)
let rec vecset v x y z =
  v.(0) <- x;
  v.(1) <- y;
  v.(2) <- z
in

(* ��������ゃ�у�������� *)
let rec vecfill v elem =
  v.(0) <- elem;
  v.(1) <- elem;
  v.(2) <- elem
in

(* ��九�������� *)
let rec vecbzero v =
  vecfill v 0.0
in

(* ��潟����� *)
let rec veccpy dest src =
  dest.(0) <- src.(0);
  dest.(1) <- src.(1);
  dest.(2) <- src.(2)
in

(* 莊���≪�����箙� *)
let rec vecdist2 p q =
  fsqr (p.(0) -. q.(0)) +. fsqr (p.(1) -. q.(1)) +. fsqr (p.(2) -. q.(2))
in

(* 罩ｈ����� ��若�㊤�蚊�������с�������＜�� *)
let rec vecunit v =
  let il = 1.0 /. sqrt(fsqr v.(0) +. fsqr v.(1) +. fsqr v.(2)) in
  v.(0) <- v.(0) *. il;
  v.(1) <- v.(1) *. il;
  v.(2) <- v.(2) *. il
in

(* 膃��垬��罩ｈ����� ��若�㊤�蚊����с�����*)
let rec vecunit_sgn v inv =
  let l = sqrt (fsqr v.(0) +. fsqr v.(1) +. fsqr v.(2)) in
  let il = if fiszero l then 1.0 else if inv then -1.0 /. l else 1.0 /. l in
  v.(0) <- v.(0) *. il;
  v.(1) <- v.(1) *. il;
  v.(2) <- v.(2) *. il
in

(* ���腥� *)
let rec veciprod v w =
  v.(0) *. w.(0) +. v.(1) *. w.(1) +. v.(2) *. w.(2)
in

(* ���腥� 綣���医就綣������違�������� *)
let rec veciprod2 v w0 w1 w2 =
  v.(0) *. w0 +. v.(1) *. w1 +. v.(2) *. w2
in

(* ��ャ�����������������絎���医��������膊� *)
let rec vecaccum dest scale v =
  dest.(0) <- dest.(0) +. scale *. v.(0);
  dest.(1) <- dest.(1) +. scale *. v.(1);
  dest.(2) <- dest.(2) +. scale *. v.(2)
in

(* ������������������ *)
let rec vecadd dest v =
  dest.(0) <- dest.(0) +. v.(0);
  dest.(1) <- dest.(1) +. v.(1);
  dest.(2) <- dest.(2) +. v.(2)
in

(* ������������荀�膣����紕����腥� *)
let rec vecmul dest v =
  dest.(0) <- dest.(0) *. v.(0);
  dest.(1) <- dest.(1) *. v.(1);
  dest.(2) <- dest.(2) *. v.(2)
in

(* ���������������絎���医�� *)
let rec vecscale dest scale =
  dest.(0) <- dest.(0) *. scale;
  dest.(1) <- dest.(1) *. scale;
  dest.(2) <- dest.(2) *. scale
in

(* 篁����鐚����������������荀�膣����紕����腥����荐�膊�������膊� *)
let rec vecaccumv dest v w =
  dest.(0) <- dest.(0) +. v.(0) *. w.(0);
  dest.(1) <- dest.(1) +. v.(1) *. w.(1);
  dest.(2) <- dest.(2) +. v.(2) *. w.(2)
in

(******************************************************************************
   ��������吾�с����������若�炊�������吾����≪����祉�拷�∽��
 *****************************************************************************)

(* ��������鴻����ｇ┏ 0:��＜�� 1:絽���丈─罕� 2:膰�罔≧�� 3:���綽����罔≧�� 4:������*)
let rec o_texturetype m =
  let (m_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_tex
in

(* ���篏����綵∝�� 0:��贋�剛�� 1:綛渇�� 2:篋�罨≧�臥�� 3:������ *)
let rec o_form m =
  let (xm_tex, m_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_shape
in

(* ���絨���号�� 0:��≧�ｅ��絨������� 1:��≧�ｏ�����絎������♂�√��絨� 2:��≧�ｏ��絎������♂�√��絨� *)
let rec o_reflectiontype m =
  let (xm_tex, xm_shape, m_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_surface
in

(* ��臥�≪��紊���眼����������������������������� true:紊���眼����� false:�����眼����� *)
let rec o_isinvert m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       m_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m in
  m_invert
in

(* ���荵≪�������� true:���荵≪����� false:���荵∝�＜�� 2罨≧�臥�≪�������������炊����� *)
let rec o_isrot m =
  let (xm_tex, xm_shape, xm_surface, m_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m in
  m_isrot
in

(* ���篏�綵∝�吟�� a��������＜�若�� *)
let rec o_param_a m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       m_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_abc.(0)
in

(* ���篏�綵∝�吟�� b��������＜�若�� *)
let rec o_param_b m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       m_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_abc.(1)
in

(* ���篏�綵∝�吟�� c��������＜�若�� *)
let rec o_param_c m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       m_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_abc.(2)
in

(* ���篏�綵∝�吟�� abc��������＜�若�� *)
let rec o_param_abc m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       m_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_abc
in

(* ���篏����筝㊤��x綺ф�� *)
let rec o_param_x m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, m_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_xyz.(0)
in

(* ���篏����筝㊤��y綺ф�� *)
let rec o_param_y m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, m_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_xyz.(1)
in

(* ���篏����筝㊤��z綺ф�� *)
let rec o_param_z m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, m_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_xyz.(2)
in

(* ���篏������≧�ｅ��絨���� 0.0 -- 1.0 *)
let rec o_diffuse m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, m_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_surfparams.(0)
in

(* ���篏����筝�絎������♂�√��絨���� 0.0 -- 1.0 *)
let rec o_hilight m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, m_surfparams, xm_color,
       xm_rot123, xm_ctbl) = m
  in
  m_surfparams.(1)
in

(* ���篏���蚊�� R������ *)
let rec o_color_red m =
  let (xm_tex, xm_shape, m_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, m_color,
       xm_rot123, xm_ctbl) = m
  in
  m_color.(0)
in

(* ���篏���蚊�� G������ *)
let rec o_color_green m =
  let (xm_tex, xm_shape, m_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, m_color,
       xm_rot123, xm_ctbl) = m
  in
  m_color.(1)
in

(* ���篏���蚊�� B������ *)
let rec o_color_blue m =
  let (xm_tex, xm_shape, m_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, m_color,
       xm_rot123, xm_ctbl) = m
  in
  m_color.(2)
in

(* ���篏������臥�∽�合��綣���� y*z������篆���� 2罨≧�臥�≪����������с�����荵≪����������翫�������� *)
let rec o_param_r1 m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       m_rot123, xm_ctbl) = m
  in
  m_rot123.(0)
in

(* ���篏������臥�∽�合��綣���� x*z������篆���� 2罨≧�臥�≪����������с�����荵≪����������翫�������� *)
let rec o_param_r2 m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       m_rot123, xm_ctbl) = m
  in
  m_rot123.(1)
in

(* ���篏������臥�∽�合��綣���� x*y������篆���� 2罨≧�臥�≪����������с�����荵≪����������翫�������� *)
let rec o_param_r3 m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       m_rot123, xm_ctbl) = m
  in
  m_rot123.(2)
in

(* ���膩������阪����鴻�����������������荐�膊���������翫�����絎���違����若����� *)
(*
   0 -- 2 ���������荀�膣�: ���篏������堺��綺ф��膤祉��綛活��腱糸�����������膩�紮����
   3���������荀�膣�:
   ��贋�剛�������≦��
   綛渇�≒�� abc���������������������腥�
   篋�罨≧�臥�≪�����������篋�罨≧�合��綣����絎���育��
 *)
let rec o_param_ctbl m =
  let (xm_tex, xm_shape, xm_surface, xm_isrot,
       xm_abc, xm_xyz,
       xm_invert, xm_surfparams, xm_color,
       xm_rot123, m_ctbl) = m
  in
  m_ctbl
in

(******************************************************************************
   Pixel�����若�帥����＜�潟����≪����祉�拷�∽�亥召
 *****************************************************************************)

(* ��贋･���菴処掘��у�������������������祉�����RGB��� *)
let rec p_rgb pixel =
  let (m_rgb, xm_isect_ps, xm_sids, xm_cdif, xm_engy,
       xm_r20p, xm_gid, xm_nvectors ) = pixel in
  m_rgb
in

(* 蕋���違��������������篏����茵�腦���������鴻�������� *)
let rec p_intersection_points pixel =
  let (xm_rgb, m_isect_ps, xm_sids, xm_cdif, xm_engy,
       xm_r20p, xm_gid, xm_nvectors ) = pixel in
  m_isect_ps
in

(* 蕋���違�����������茵�腦����������篏���∝���垩�������� *)
(* ���篏���∝���垩�� ��������吾�с����������� * 4 + (solver���菴�������) *)
let rec p_surface_ids pixel =
  let (xm_rgb, xm_isect_ps, m_sids, xm_cdif, xm_engy,
       xm_r20p, xm_gid, xm_nvectors ) = pixel in
  m_sids
in

(* ����･���������荐�膊���������������������������� *)
let rec p_calc_diffuse pixel =
  let (xm_rgb, xm_isect_ps, xm_sids, m_cdif, xm_engy,
       xm_r20p, xm_gid, xm_nvectors ) = pixel in
  m_cdif
in

(* 茵�腦���鴻������･��������������������若����������祉��莠�綺����筝�������絲�筝����紊с����� *)
let rec p_energy pixel =
  let (xm_rgb, xm_isect_ps, xm_sids, xm_cdif, m_engy,
       xm_r20p, xm_gid, xm_nvectors ) = pixel in
  m_engy
in

(* 茵�腦���鴻������･��������������������若�����膩������違��1/5������綣����������荐�膊���������� *)
let rec p_received_ray_20percent pixel =
  let (xm_rgb, xm_isect_ps, xm_sids, xm_cdif, xm_engy,
       m_r20p, xm_gid, xm_nvectors ) = pixel in
  m_r20p
in

(* ��������������祉�������違����若�� ID *)
(*
   ��鴻�������若�喝婚罔� (x,y)�����鴻����違����若��ID��� (x+2*y) mod 5 ���絎�������
   腟�������筝���潟�������������������鴻����������������鴻��筝�筝�綏����4��鴻����ャ����違����若�����������
   0 1 2 3 4 0 1 2 3 4
   2 3 4 0 1 2 3 4 0 1
   4 0 1 2 3 4 0 1 2 3
   1 2 3 4 0 1 2 3 4 0
*)

let rec p_group_id pixel =
  let (xm_rgb, xm_isect_ps, xm_sids, xm_cdif, xm_engy,
       xm_r20p, m_gid, xm_nvectors ) = pixel in
  m_gid.(0)
in

(* ��違����若��ID�����祉�������������≪����祉�拷�∽�� *)
let rec p_set_group_id pixel id =
  let (xm_rgb, xm_isect_ps, xm_sids, xm_cdif, xm_engy,
       xm_r20p, m_gid, xm_nvectors ) = pixel in
  m_gid.(0) <- id
in

(* ���茵�腦���鴻�����������羈�膩������������� *)
let rec p_nvectors pixel =
  let (xm_rgb, xm_isect_ps, xm_sids, xm_cdif, xm_engy,
       xm_r20p, xm_gid, m_nvectors ) = pixel in
  m_nvectors
in

(******************************************************************************
   ���������羝���炊�劫�������������������＜�潟����≪����祉�拷�∽��
 *****************************************************************************)

(* ������������ *)
let rec d_vec d =
  let (m_vec, xm_const) = d in
  m_vec
in

(* �����������吾�с��������絲障�����篏���ｃ�� solver 蕭����������絎���違����若����� *)
let rec d_const d =
  let (dm_vec, m_const) = d in
  m_const
in

(******************************************************************************
   綛渇�∫�♂�≫��������絨�������
 *****************************************************************************)

(* ��∝����� ��������吾�с�����������*4 + (solver���菴�������) *)
let rec r_surface_id r =
  let (m_sid, xm_dvec, xm_br) = r in
  m_sid
in

(* ���羣����������絨���劫��������������(���������������) *)
let rec r_dvec r =
  let (xm_sid, m_dvec, xm_br) = r in
  m_dvec
in

(* ���篏�������絨���� *)
let rec r_bright r =
  let (xm_sid, xm_dvec, m_br) = r in
  m_br
in

(******************************************************************************
   �����若�粋�㏍�粋昭��帥����∽�亥召
 *****************************************************************************)

(* �����吾�≪�� *)
let rec rad x =
  x *. 0.017453293
in

(**** ��医�������若�帥��茯㏍�粋昭��� ****)
let rec read_screen_settings _ =

  (* ��鴻�������若�割賢綽����綺ф�� *)
  screen.(0) <- read_float ();
  screen.(1) <- read_float ();
  screen.(2) <- read_float ();
  (* ���荵∵�� *)
  let v1 = rad (read_float ()) in
  let cos_v1 = cos v1 in
  let sin_v1 = sin v1 in
  let v2 = rad (read_float ()) in
  let cos_v2 = cos v2 in
  let sin_v2 = sin v2 in
  (* ��鴻�������若�渇�≪��絅ヨ�������劫����������������� 羈�荀���鴻��������莊����200������������ *)
  screenz_dir.(0) <- cos_v1 *. sin_v2 *. 200.0;
  screenz_dir.(1) <- sin_v1 *. -200.0;
  screenz_dir.(2) <- cos_v1 *. cos_v2 *. 200.0;
  (* ��鴻�������若�渇��X��劫����������������� *)
  screenx_dir.(0) <- cos_v2;
  screenx_dir.(1) <- 0.0;
  screenx_dir.(2) <- fneg sin_v2;
  (* ��鴻�������若�渇��Y��劫����������������� *)
  screeny_dir.(0) <- fneg sin_v1 *. sin_v2;
  screeny_dir.(1) <- fneg cos_v1;
  screeny_dir.(2) <- fneg sin_v1 *. cos_v2;
  (* 荀���剛��臀�������������(腟九�上婚罔�) *)
  viewpoint.(0) <- screen.(0) -. screenz_dir.(0);
  viewpoint.(1) <- screen.(1) -. screenz_dir.(1);
  viewpoint.(2) <- screen.(2) -. screenz_dir.(2)

in

(* ���羣������宴��茯㏍�粋昭��� *)
let rec read_light _ =

  let nl = read_int () in

  (* ���膩���≫�� *)
  let l1 = rad (read_float ()) in
  let sl1 = sin l1 in
  light.(1) <- fneg sl1;
  let l2 = rad (read_float ()) in
  let cl1 = cos l1 in
  let sl2 = sin l2 in
  light.(0) <- cl1 *. sl2;
  let cl2 = cos l2 in
  light.(2) <- cl1 *. cl2;
  beam.(0) <- read_float ()

in

(* ������2罨≦就綣頫���� A ���筝≦�眼��������荵∵����� R ������������茵���� R^t * A * R ���篏���� *)
(* R ��� x,y,z荵吾����≪��������荵∵��������腥� R(z)R(y)R(x) *)
(* ��鴻�������若�喝婚罔�������������y荵後��荵≪����粋��綺����膃��垩����� *)

let rec rotate_quadratic_matrix abc rot =
  (* ���荵∵��������腥� R(z)R(y)R(x) ���荐�膊������� *)
  let cos_x = cos rot.(0) in
  let sin_x = sin rot.(0) in
  let cos_y = cos rot.(1) in
  let sin_y = sin rot.(1) in
  let cos_z = cos rot.(2) in
  let sin_z = sin rot.(2) in

  let m00 = cos_y *. cos_z in
  let m01 = sin_x *. sin_y *. cos_z -. cos_x *. sin_z in
  let m02 = cos_x *. sin_y *. cos_z +. sin_x *. sin_z in

  let m10 = cos_y *. sin_z in
  let m11 = sin_x *. sin_y *. sin_z +. cos_x *. cos_z in
  let m12 = cos_x *. sin_y *. sin_z -. sin_x *. cos_z in

  let m20 = fneg sin_y in
  let m21 = sin_x *. cos_y in
  let m22 = cos_x *. cos_y in

  (* a, b, c�����������ゃ�������������≪����� *)
  let ao = abc.(0) in
  let bo = abc.(1) in
  let co = abc.(2) in

  (* R^t * A * R ���荐�膊� *)

  (* X^2, Y^2, Z^2������ *)
  abc.(0) <- ao *. fsqr m00 +. bo *. fsqr m10 +. co *. fsqr m20;
  abc.(1) <- ao *. fsqr m01 +. bo *. fsqr m11 +. co *. fsqr m21;
  abc.(2) <- ao *. fsqr m02 +. bo *. fsqr m12 +. co *. fsqr m22;

  (* ���荵≪�������ｃ����������� XY, YZ, ZX������ *)
  rot.(0) <- 2.0 *. (ao *. m01 *. m02 +. bo *. m11 *. m12 +. co *. m21 *. m22);
  rot.(1) <- 2.0 *. (ao *. m00 *. m02 +. bo *. m10 *. m12 +. co *. m20 *. m22);
  rot.(2) <- 2.0 *. (ao *. m00 *. m01 +. bo *. m10 *. m11 +. co *. m20 *. m21)

in

(**** ��������吾�с�����1��ゃ�������若�帥��茯㏍�粋昭��� ****)
let rec read_nth_object n =

  let texture = read_int () in
  if texture <> -1 then
    (
      let form = read_int () in
      let refltype = read_int () in
      let isrot_p = read_int () in

      let abc = create_array 3 0.0 in
      abc.(0) <- read_float ();
      abc.(1) <- read_float (); (* 5 *)
      abc.(2) <- read_float ();

      let xyz = create_array 3 0.0 in
      xyz.(0) <- read_float ();
      xyz.(1) <- read_float ();
      xyz.(2) <- read_float ();

      let m_invert = fisneg (read_float ()) in (* 10 *)

      let reflparam = create_array 2 0.0 in
      reflparam.(0) <- read_float (); (* diffuse *)
      reflparam.(1) <- read_float (); (* hilight *)

      let color = create_array 3 0.0 in
      color.(0) <- read_float ();
      color.(1) <- read_float ();
      color.(2) <- read_float (); (* 15 *)

      let rotation = create_array 3 0.0 in
      if isrot_p <> 0 then
	(
	 rotation.(0) <- rad (read_float ());
	 rotation.(1) <- rad (read_float ());
	 rotation.(2) <- rad (read_float ())
	)
      else ();

      (* ��������＜�若�帥��罩ｈ����� *)

      (* 羈�: 筝�荐�罩ｈ����� (form = 2) ������ *)
      let m_invert2 = if form = 2 then true else m_invert in
      let ctbl = create_array 4 0.0 in
      (* ��������������������� abc ��� rotation ���������篏�������������*)
      let obj =
	(texture, form, refltype, isrot_p,
	 abc, xyz, (* x-z *)
	 m_invert2,
	 reflparam, (* reflection paramater *)
	 color, (* color *)
	 rotation, (* rotation *)
         ctbl (* constant table *)
	) in
      objects.(n) <- obj;

      if form = 3 then
	(
	  (* 2罨≧�臥��: X,Y,Z ��泣�ゃ�冴�����2罨≦就綣頫�������絲乗����������� *)
	 let a = abc.(0) in
	 abc.(0) <- if fiszero a then 0.0 else sgn a /. fsqr a; (* X^2 ������ *)
	 let b = abc.(1) in
	 abc.(1) <- if fiszero b then 0.0 else sgn b /. fsqr b; (* Y^2 ������ *)
	 let c = abc.(2) in
	 abc.(2) <- if fiszero c then 0.0 else sgn c /. fsqr c  (* Z^2 ������ *)
	)
      else if form = 2 then
	(* 綛渇��: 羈�膩����������������罩ｈ�����, 罐究�с��莢����腟延�� *)
	vecunit_sgn abc (not m_invert)
      else ();

      (* 2罨≦就綣頫����������荵√����������純�� *)
      if isrot_p <> 0 then
	rotate_quadratic_matrix abc rotation
      else ();

      true
     )
  else
    false (* �����若�帥��腟�篋� *)
in

(**** ���篏������若�水��篏����茯㏍�粋昭��� ****)
let rec read_object n =
  if n < 60 then
    if read_nth_object n then
      read_object (n + 1)
    else
      n_objects.(0) <- n
  else () (* failwith "too many objects" *)
in

let rec read_all_object _ =
  read_object 0
in

(**** AND, OR ��������������若�����茯㏍�粋昭��� ****)

(* ��������������若��1��ゃ��茯㏍�粋昭��帥��������������������菴���� *)
let rec read_net_item length =
  let item = read_int () in
  if item = -1 then create_array (length + 1) (-1)
  else
    let v = read_net_item (length + 1) in
    (v.(length) <- item; v)
in

let rec read_or_network length =
  let net = read_net_item 0 in
  if net.(0) = -1 then
    create_array (length + 1) net
  else
    let v = read_or_network (length + 1) in
    (v.(length) <- net; v)
in

let rec read_and_network n =
  let net = read_net_item 0 in
  if net.(0) = -1 then ()
  else (
    and_net.(n) <- net;
    read_and_network (n + 1)
  )
in

let rec read_parameter _ =
  (
   read_screen_settings();
   read_light();
   read_all_object ();
   read_and_network 0;
   or_net.(0) <- read_or_network 0
  )
in

(******************************************************************************
   ��雁�������������吾�с��������篋ょ�鴻��羆���������∽�亥召
 *****************************************************************************)

(* solver :
   ��������吾�с����� (��� index) ������������������ L, P ������������������
   ��雁�� Lt + P ��������������吾�с�����������篋ょ�鴻��羆����������
   篋ょ�鴻����������翫����� 0 ������篋ょ�鴻����������翫�����������篁ュ�����菴�������
   ������菴������ゃ�� nvector ��т困��鴻��羈�膩����������������羆�������������綽�荀����
   (��贋�剛�������翫��)

   篋ょ�鴻��綺ф����� t �����ゃ�������� solver_dist �����主��������������
*)

(* ��贋�剛��������絎������������≪��茵�腦���������������������ゅ�������� *)
(* i0 : ��≪�������眼��荵吾��index X:0, Y:1, Z:2         i2,i3���篁����2荵吾��index *)
let rec solver_rect_surface m dirvec b0 b1 b2 i0 i1 i2  =
  if fiszero dirvec.(i0) then false else
  let abc = o_param_abc m in
  let d = fneg_cond (xor (o_isinvert m) (fisneg dirvec.(i0))) abc.(i0) in

  let d2 = (d -. b0) /. dirvec.(i0) in
  if fless (fabs (d2 *. dirvec.(i1) +. b1)) abc.(i1) then
    if fless (fabs (d2 *. dirvec.(i2) +. b2)) abc.(i2) then
      (solver_dist.(0) <- d2; true)
    else false
  else false
in


(***** ��贋�剛����������吾�с����������翫�� ****)
let rec solver_rect m dirvec b0 b1 b2 =
  if      solver_rect_surface m dirvec b0 b1 b2 0 1 2 then 1   (* YZ 綛渇�� *)
  else if solver_rect_surface m dirvec b1 b2 b0 1 2 0 then 2   (* ZX 綛渇�� *)
  else if solver_rect_surface m dirvec b2 b0 b1 2 0 1 then 3   (* XY 綛渇�� *)
  else                                                     0
in


(* 綛渇�≪�������吾�с����������翫�� *)
let rec solver_surface m dirvec b0 b1 b2 =
  (* ��鴻��綛渇�≪��膃��垩�ゃ��莊���� *)
  (* 綛渇�≪��罐究�с��莢����腟延����������������� *)
  let abc = o_param_abc m in
  let d = veciprod dirvec abc in
  if fispos d then (
    solver_dist.(0) <- fneg (veciprod2 abc b0 b1 b2) /. d;
    1
   ) else 0
in


(* 3紊����2罨≦就綣� v^t A v ���荐�膊� *)
(* ���荵≪����＜����翫�����絲乗�������������粋��膊���������域����� *)
let rec quadratic m v0 v1 v2 =
  let diag_part =
    fsqr v0 *. o_param_a m +. fsqr v1 *. o_param_b m +. fsqr v2 *. o_param_c m
  in
  if o_isrot m = 0 then
    diag_part
  else
    diag_part
      +. v1 *. v2 *. o_param_r1 m
      +. v2 *. v0 *. o_param_r2 m
      +. v0 *. v1 *. o_param_r3 m
in

(* 3紊���医��1罨≦就綣� v^t A w ���荐�膊� *)
(* ���荵≪����＜����翫����� A ���絲乗�������������粋��膊���������域����� *)
let rec bilinear m v0 v1 v2 w0 w1 w2 =
  let diag_part =
    v0 *. w0 *. o_param_a m
      +. v1 *. w1 *. o_param_b m
      +. v2 *. w2 *. o_param_c m
  in
  if o_isrot m = 0 then
    diag_part
  else
    diag_part +. fhalf
      ((v2 *. w1 +. v1 *. w2) *. o_param_r1 m
	 +. (v0 *. w2 +. v2 *. w0) *. o_param_r2 m
	 +. (v0 *. w1 +. v1 *. w0) *. o_param_r3 m)
in


(* 2罨≧�臥�≪�障����������������翫�� *)
(* 2罨≦就綣���ц；�憗����������臥�� x^t A x - (0 ��� 1) = 0 ��� ��雁�� base + dirvec*t ���
   篋ょ�鴻��羆������������牙�������合��綣���� x = base + dirvec*t ���篁ｅ�ャ�����t���羆����������
   ��ゃ�障�� (base + dirvec*t)^t A (base + dirvec*t) - (0 ��� 1) = 0���
   絮������������� (dirvec^t A dirvec)*t^2 + 2*(dirvec^t A base)*t  +
   (base^t A base) - (0���1) = 0 ��������ｃ��t�����≪�����2罨≧�合��綣����茹ｃ����域��������*)

let rec solver_second m dirvec b0 b1 b2 =

  (* 茹ｃ�����綣� (-b' 賊 sqrt(b'^2 - a*c)) / a  ���篏睡��(b' = b/2) *)
  (* a = dirvec^t A dirvec *)
  let aa = quadratic m dirvec.(0) dirvec.(1) dirvec.(2) in

  if fiszero aa then
    0 (* 罩ｇ∈��������������翫�����1罨≧�合��綣����茹ｃ����������������∴��������������絽吾��紊т��紊� *)
  else (

    (* b' = b/2 = dirvec^t A base   *)
    let bb = bilinear m dirvec.(0) dirvec.(1) dirvec.(2) b0 b1 b2 in
    (* c = base^t A base  - (0���1)  *)
    let cc0 = quadratic m b0 b1 b2 in
    let cc = if o_form m = 3 then cc0 -. 1.0 else cc0 in
    (* ��ゅ�ュ�� *)
    let d = fsqr bb -. aa *. cc in

    if fispos d then (
      let sd = sqrt d in
      let t1 = if o_isinvert m then sd else fneg sd in
      (solver_dist.(0) <- (t1 -. bb) /.  aa; 1)
     )
    else
      0
   )
in

(**** solver �����＜�ゃ�潟����若����� ****)
let rec solver index dirvec org =
  let m = objects.(index) in
  (* ��雁�����紮���鴻�����篏������堺��篏�臀����������������綛活��腱糸�� *)
  let b0 =  org.(0) -. o_param_x m in
  let b1 =  org.(1) -. o_param_y m in
  let b2 =  org.(2) -. o_param_z m in
  let m_shape = o_form m in
  (* ���篏����腮�蕁����綽�������茖������∽�違����若�� *)
  if m_shape = 1 then       solver_rect m dirvec b0 b1 b2    (* ��贋�剛�� *)
  else if m_shape = 2 then  solver_surface m dirvec b0 b1 b2 (* 綛渇�� *)
  else                      solver_second m dirvec b0 b1 b2  (* 2罨≧�臥��/������ *)
in

(******************************************************************************
   solver��������若�����篏睡��蕭�������
 *****************************************************************************)
(*
   ���絽悟��solver ������罕������雁�� start + t * dirvec ������篏����篋ょ�鴻�� t �����ゃ��������菴����
   t �����ゃ�� solver_dist�����主��

   solver_fast ��������雁�������劫�������������� dirvec �����ゃ�����篏���ｃ�������若��������篏睡��
   ������������ solver_rect_fast, solver_surface_fast, solver_second_fast�����若��

   solver_fast2 ������dirvec�����雁�����紮���� start ���������������篏���ｃ�������若��������篏睡��
   ��贋�剛�������ゃ��������start��������若��������������蕭������������с�������������с��solver_fast���
   ��������� solver_rect_fast�����������������若�吟��������篁ュ��������篏������ゃ��������
   solver_surface_fast2��障�����solver_second_fast2�����������������若��

   紊����dconst�����劫�����������������sconst���紮���鴻����≪����������若�����
*)

(***** solver_rect���dirvec�����若�����篏睡��蕭������� ******)
let rec solver_rect_fast m v dconst b0 b1 b2 =
  let d0 = (dconst.(0) -. b0) *. dconst.(1) in
  if  (* YZ綛渇�≪�����茵�腦���ゅ�� *)
    if fless (fabs (d0 *. v.(1) +. b1)) (o_param_b m) then
      if fless (fabs (d0 *. v.(2) +. b2)) (o_param_c m) then
	not (fiszero dconst.(1))
      else false
    else false
  then
    (solver_dist.(0) <- d0; 1)
  else let d1 = (dconst.(2) -. b1) *. dconst.(3) in
  if  (* ZX綛渇�≪�����茵�腦���ゅ�� *)
    if fless (fabs (d1 *. v.(0) +. b0)) (o_param_a m) then
      if fless (fabs (d1 *. v.(2) +. b2)) (o_param_c m) then
	not (fiszero dconst.(3))
      else false
    else false
  then
    (solver_dist.(0) <- d1; 2)
  else let d2 = (dconst.(4) -. b2) *. dconst.(5) in
  if  (* XY綛渇�≪�����茵�腦���ゅ�� *)
    if fless (fabs (d2 *. v.(0) +. b0)) (o_param_a m) then
      if fless (fabs (d2 *. v.(1) +. b1)) (o_param_b m) then
	not (fiszero dconst.(5))
      else false
    else false
  then
    (solver_dist.(0) <- d2; 3)
  else
    0
in

(**** solver_surface���dirvec�����若�����篏睡��蕭������� ******)
let rec solver_surface_fast m dconst b0 b1 b2 =
  if fisneg dconst.(0) then (
    solver_dist.(0) <-
      dconst.(1) *. b0 +. dconst.(2) *. b1 +. dconst.(3) *. b2;
    1
   ) else 0
in

(**** solver_second ���dirvec�����若�����篏睡��蕭������� ******)
let rec solver_second_fast m dconst b0 b1 b2 =

  let aa = dconst.(0) in
  if fiszero aa then
    0
  else
    let neg_bb = dconst.(1) *. b0 +. dconst.(2) *. b1 +. dconst.(3) *. b2 in
    let cc0 = quadratic m b0 b1 b2 in
    let cc = if o_form m = 3 then cc0 -. 1.0 else cc0 in
    let d = (fsqr neg_bb) -. aa *. cc in
    if fispos d then (
      if o_isinvert m then
	solver_dist.(0) <- (neg_bb +. sqrt d) *. dconst.(4)
      else
	solver_dist.(0) <- (neg_bb -. sqrt d) *. dconst.(4);
      1)
    else 0
in

(**** solver ���dirvec�����若�����篏睡��蕭������� *******)
let rec solver_fast index dirvec org =
  let m = objects.(index) in
  let b0 = org.(0) -. o_param_x m in
  let b1 = org.(1) -. o_param_y m in
  let b2 = org.(2) -. o_param_z m in
  let dconsts = d_const dirvec in
  let dconst = dconsts.(index) in
  let m_shape = o_form m in
  if m_shape = 1 then
    solver_rect_fast m (d_vec dirvec) dconst b0 b1 b2
  else if m_shape = 2 then
    solver_surface_fast m dconst b0 b1 b2
  else
    solver_second_fast m dconst b0 b1 b2
in




(* solver_surface���dirvec+start�����若�����篏睡��蕭������� *)
let rec solver_surface_fast2 m dconst sconst b0 b1 b2 =
  if fisneg dconst.(0) then (
    solver_dist.(0) <- dconst.(0) *. sconst.(3);
    1
   ) else 0
in

(* solver_second���dirvec+start�����若�����篏睡��蕭������� *)
let rec solver_second_fast2 m dconst sconst b0 b1 b2 =

  let aa = dconst.(0) in
  if fiszero aa then
    0
  else
    let neg_bb = dconst.(1) *. b0 +. dconst.(2) *. b1 +. dconst.(3) *. b2 in
    let cc = sconst.(3) in
    let d = (fsqr neg_bb) -. aa *. cc in
    if fispos d then (
      if o_isinvert m then
	solver_dist.(0) <- (neg_bb +. sqrt d) *. dconst.(4)
      else
	solver_dist.(0) <- (neg_bb -. sqrt d) *. dconst.(4);
      1)
    else 0
in

(* solver������dirvec+start�����若�����篏睡��蕭������� *)
let rec solver_fast2 index dirvec =
  let m = objects.(index) in
  let sconst = o_param_ctbl m in
  let b0 = sconst.(0) in
  let b1 = sconst.(1) in
  let b2 = sconst.(2) in
  let dconsts = d_const dirvec in
  let dconst = dconsts.(index) in
  let m_shape = o_form m in
  if m_shape = 1 then
    solver_rect_fast m (d_vec dirvec) dconst b0 b1 b2
  else if m_shape = 2 then
    solver_surface_fast2 m dconst sconst b0 b1 b2
  else
    solver_second_fast2 m dconst sconst b0 b1 b2
in

(******************************************************************************
   ��劫�����������������絎���違����若��������荐�膊���������∽�亥召
 *****************************************************************************)

(* ��贋�剛����������吾�с��������絲障�������������� *)
let rec setup_rect_table vec m =
  let const = create_array 6 0.0 in

  if fiszero vec.(0) then (* YZ綛渇�� *)
    const.(1) <- 0.0
  else (
    (* ��≪�� X 綺ф�� *)
    const.(0) <- fneg_cond (xor (o_isinvert m) (fisneg vec.(0))) (o_param_a m);
    (* ��劫�����������������篏�������������X��劫�����1��蚊����� *)
    const.(1) <- 1.0 /. vec.(0)
  );
  if fiszero vec.(1) then (* ZX綛渇�� : YZ綛渇�≪�����罕�*)
    const.(3) <- 0.0
  else (
    const.(2) <- fneg_cond (xor (o_isinvert m) (fisneg vec.(1))) (o_param_b m);
    const.(3) <- 1.0 /. vec.(1)
  );
  if fiszero vec.(2) then (* XY綛渇�� : YZ綛渇�≪�����罕�*)
    const.(5) <- 0.0
  else (
    const.(4) <- fneg_cond (xor (o_isinvert m) (fisneg vec.(2))) (o_param_c m);
    const.(5) <- 1.0 /. vec.(2)
  );
  const
in

(* 綛渇�≪�������吾�с��������絲障�������������� *)
let rec setup_surface_table vec m =
  let const = create_array 4 0.0 in
  let d =
    vec.(0) *. o_param_a m +. vec.(1) *. o_param_b m +. vec.(2) *. o_param_c m
  in
  if fispos d then (
    (* ��劫�����������������篏������������医抗��≪�������贋�劫����� 1 ��蚊����� *)
    const.(0) <- -1.0 /. d;
    (* ��������鴻��綛渇�≪��������莊���≪����劫��������������篏�������������絨����3罨＞��綵√�����篆���� *)
    const.(1) <- fneg (o_param_a m /. d);
    const.(2) <- fneg (o_param_b m /. d);
    const.(3) <- fneg (o_param_c m /. d)
   ) else
    const.(0) <- 0.0;
  const

in

(* 2罨≧�臥�≪��絲障�������������� *)
let rec setup_second_table v m =
  let const = create_array 5 0.0 in

  let aa = quadratic m v.(0) v.(1) v.(2) in
  let c1 = fneg (v.(0) *. o_param_a m) in
  let c2 = fneg (v.(1) *. o_param_b m) in
  let c3 = fneg (v.(2) *. o_param_c m) in

  const.(0) <- aa;  (* 2罨≧�合��綣���� a 篆���� *)

  (* b' = dirvec^t A start ���������(dirvec^t A)������������荐�膊����const.(1:3)�����主�����
     b' ���羆����������������������������������start������腥������������域��������膃��垩�������������� *)
  if o_isrot m <> 0 then (
    const.(1) <- c1 -. fhalf (v.(2) *. o_param_r2 m +. v.(1) *. o_param_r3 m);
    const.(2) <- c2 -. fhalf (v.(2) *. o_param_r1 m +. v.(0) *. o_param_r3 m);
    const.(3) <- c3 -. fhalf (v.(1) *. o_param_r1 m +. v.(0) *. o_param_r2 m)
   ) else (
    const.(1) <- c1;
    const.(2) <- c2;
    const.(3) <- c3
   );
  if not (fiszero aa) then
    const.(4) <- 1.0 /. aa (* a篆���違�������違��羆�������茹ｃ�����綣���с����蚊��膊����羔��ｻ *)
  else ();
  const

in

(* �����������吾�с����������ゃ�����茖������∽�違����若����с����若��������篏���� *)
let rec iter_setup_dirvec_constants dirvec index =
  if index >= 0 then (
    let m = objects.(index) in
    let dconst = (d_const dirvec) in
    let v = d_vec dirvec in
    let m_shape = o_form m in
    if m_shape = 1 then  (* rect *)
      dconst.(index) <- setup_rect_table v m
    else if m_shape = 2 then  (* surface *)
      dconst.(index) <- setup_surface_table v m
    else                      (* second *)
      dconst.(index) <- setup_second_table v m;

    iter_setup_dirvec_constants dirvec (index - 1)
  ) else ()
in

let rec setup_dirvec_constants dirvec =
  iter_setup_dirvec_constants dirvec (n_objects.(0) - 1)
in

(******************************************************************************
   ��雁�����紮���鴻����≪����������若�������������������吾�с��������絲障�����荐�膊���������∽�亥召
 *****************************************************************************)

let rec setup_startp_constants p index =
  if index >= 0 then (
    let obj = objects.(index) in
    let sconst = o_param_ctbl obj in
    let m_shape = o_form obj in
    sconst.(0) <- p.(0) -. o_param_x obj;
    sconst.(1) <- p.(1) -. o_param_y obj;
    sconst.(2) <- p.(2) -. o_param_z obj;
    if m_shape = 2 then (* surface *)
      sconst.(3) <-
	veciprod2 (o_param_abc obj) sconst.(0) sconst.(1) sconst.(2)
    else if m_shape > 2 then (* second *)
      let cc0 = quadratic obj sconst.(0) sconst.(1) sconst.(2) in
      sconst.(3) <- if m_shape = 3 then cc0 -. 1.0 else cc0
    else ();
    setup_startp_constants p (index - 1)
   ) else ()
in

let rec setup_startp p =
  veccpy startp_fast p;
  setup_startp_constants p (n_objects.(0) - 1)
in

(******************************************************************************
   筝���������������鴻����������吾�с�������������障����������������������ゅ����������∽�亥召
 *****************************************************************************)

(**** ��� q �����������吾�с����� m ���紊���������������������ゅ�������� ****)

(* ��贋�剛�� *)
let rec is_rect_outside m p0 p1 p2 =
  if
    if (fless (fabs p0) (o_param_a m)) then
      if (fless (fabs p1) (o_param_b m)) then
	fless (fabs p2) (o_param_c m)
      else false
    else false
  then o_isinvert m else not (o_isinvert m)
in

(* 綛渇�� *)
let rec is_plane_outside m p0 p1 p2 =
  let w = veciprod2 (o_param_abc m) p0 p1 p2 in
  not (xor (o_isinvert m) (fisneg w))
in

(* 2罨≧�臥�� *)
let rec is_second_outside m p0 p1 p2 =
  let w = quadratic m p0 p1 p2 in
  let w2 = if o_form m = 3 then w -. 1.0 else w in
  not (xor (o_isinvert m) (fisneg w2))
in

(* ���篏����筝㊤��綺ф�����綛活��腱糸��������筝���с�����������茖������∽�違����若�� *)
let rec is_outside m q0 q1 q2 =
  let p0 = q0 -. o_param_x m in
  let p1 = q1 -. o_param_y m in
  let p2 = q2 -. o_param_z m in
  let m_shape = o_form m in
  if m_shape = 1 then
    is_rect_outside m p0 p1 p2
  else if m_shape = 2 then
    is_plane_outside m p0 p1 p2
  else
    is_second_outside m p0 p1 p2
in

(**** ��� q ��� AND ��������������若�� iand �����������������������������������ゅ�� ****)
let rec check_all_inside ofs iand q0 q1 q2 =
  let head = iand.(ofs) in
  if head = -1 then
    true
  else (
    if is_outside objects.(head) q0 q1 q2 then
      false
    else
      check_all_inside (ofs + 1) iand q0 q1 q2
   )
in

(******************************************************************************
   茵�腦���鴻��篁�������篏����綵宴����ャ�ｃ����������������������ゅ����������∽�亥召
 *****************************************************************************)

(* ��� intersection_point ������������膩������������������劫�����莨帥�����   *)
(* ���篏������吟�ゃ����� (=綵宴����������ｃ��������) ��������������ゅ�����������*)

(**** AND ��������������若�� iand ���綵怨�������������������ゅ�� ****)
let rec shadow_check_and_group iand_ofs and_group =
  if and_group.(iand_ofs) = -1 then
    false
  else
    let obj = and_group.(iand_ofs) in
    let t0 = solver_fast obj light_dirvec intersection_point in
    let t0p = solver_dist.(0) in
    if (if t0 <> 0 then fless t0p (-0.2) else false) then
      (* Q: 篋ょ�鴻�����茖����絎������������鴻�������������吾�с�������� *)
      (* ��ャ�ｃ�����������������������茯帥�鴻�����*)
      let t = t0p +. 0.01 in
      let q0 = light.(0) *. t +. intersection_point.(0) in
      let q1 = light.(1) *. t +. intersection_point.(1) in
      let q2 = light.(2) *. t +. intersection_point.(2) in
      if check_all_inside 0 and_group q0 q1 q2 then
	true
      else
	shadow_check_and_group (iand_ofs + 1) and_group
	  (* 罨＜����������吾�с��������������茖���鴻���｢��� *)
    else
      (* 篋ょ�鴻����������翫��: 罐究�с��罩�(�����眼�����)�����翫�����    *)
      (* AND ��������������演�������������������������������障��������������*)
      (* 篋ょ�鴻���������������������������｢膣≪�������≦��������        *)
      if o_isinvert (objects.(obj)) then
	shadow_check_and_group (iand_ofs + 1) and_group
      else
	false
in

(**** OR ��違����若�� or_group ���綵宴����������������ゅ�� ****)
let rec shadow_check_one_or_group ofs or_group =
  let head = or_group.(ofs) in
  if head = -1 then
    false
  else (
    let and_group = and_net.(head) in
    let shadow_p = shadow_check_and_group 0 and_group in
    if shadow_p then
      true
    else
      shadow_check_one_or_group (ofs + 1) or_group
   )
in

(**** OR ��違����若�����������������������綵宴����ャ�ｃ�������������������������ゅ�� ****)
let rec shadow_check_one_or_matrix ofs or_matrix =
  let head = or_matrix.(ofs) in
  let range_primitive = head.(0) in
  if range_primitive = -1 then (* OR茵�������腟�篋������若�� *)
    false
  else
    if (* range primitive �����＜����������障�����range_primitive���篋ゃ�����篋����腆肴�� *)
      if range_primitive = 99 then      (* range primitive �����＜�� *)
	true
      else              (* range_primitive��������� *)
	let t = solver_fast range_primitive light_dirvec intersection_point in
        (* range primitive �����吟�ゃ����������������� *)
        (* or group ������篋ょ�鴻��������            *)
	if t <> 0 then
          if fless solver_dist.(0) (-0.1) then
            if shadow_check_one_or_group 1 head then
              true
	    else false
	  else false
	else false
    then
      if (shadow_check_one_or_group 1 head) then
	true (* 篋ょ�鴻�������������с��綵宴����ャ��篋������ゆ������｢膣∝��篋� *)
      else
	shadow_check_one_or_matrix (ofs + 1) or_matrix (* 罨＜��荀�膣����荅���� *)
    else
      shadow_check_one_or_matrix (ofs + 1) or_matrix (* 罨＜��荀�膣����荅���� *)

in

(******************************************************************************
   ���膩�������篏����篋ゅ勲��ゅ��
 *****************************************************************************)

(**** ������AND��������������若�������������ゃ�������若�鴻����劫�����絲障�����****)
(**** 篋ょ�鴻�����������������������茯帥�鴻�����                     ****)
let rec solve_each_element iand_ofs and_group dirvec =
  let iobj = and_group.(iand_ofs) in
  if iobj = -1 then ()
  else (
    let t0 = solver iobj dirvec startp in
    if t0 <> 0 then
      (
       (* 篋ょ�鴻�����������������������篋ょ�鴻��篁����荀�膣����筝㏍�������障�����������������茯帥�鴻�����*)
       (* 篁���障�с��筝㏍�ф��絨���� t �����ゃ��罸���鴻�����*)
       let t0p = solver_dist.(0) in

       if (fless 0.0 t0p) then
	 if (fless t0p tmin.(0)) then
	   (
	    let t = t0p +. 0.01 in
	    let q0 = dirvec.(0) *. t +. startp.(0) in
	    let q1 = dirvec.(1) *. t +. startp.(1) in
	    let q2 = dirvec.(2) *. t +. startp.(2) in
	    if check_all_inside 0 and_group q0 q1 q2 then
	      (
		tmin.(0) <- t;
		vecset intersection_point q0 q1 q2;
		intersected_object_id.(0) <- iobj;
		intsec_rectside.(0) <- t0
	       )
	    else ()
	   )
	 else ()
       else ();
       solve_each_element (iand_ofs + 1) and_group dirvec
      )
    else
      (* 篋ょ�鴻�����������������������������篏���������眼�����������������篁ヤ��篋ょ�鴻�������� *)
      if o_isinvert (objects.(iobj)) then
	solve_each_element (iand_ofs + 1) and_group dirvec
      else ()

   )
in

(**** 1��ゃ�� OR-group �����ゃ�����篋ょ�鴻��茯帥�鴻�� ****)
let rec solve_one_or_network ofs or_group dirvec =
  let head = or_group.(ofs) in
  if head <> -1 then (
    let and_group = and_net.(head) in
    solve_each_element 0 and_group dirvec;
    solve_one_or_network (ofs + 1) or_group dirvec
   ) else ()
in

(**** OR��������������劫��篏������ゃ�����篋ょ�鴻��茯帥�鴻�����****)
let rec trace_or_matrix ofs or_network dirvec =
  let head = or_network.(ofs) in
  let range_primitive = head.(0) in
  if range_primitive = -1 then (* �����������吾�с�����腟�篋� *)
    ()
  else (
    if range_primitive = 99 (* range primitive ������ *)
    then (solve_one_or_network 1 head dirvec)
    else
      (
	(* range primitive ���茵�腦���������������遺困��鴻�������� *)
       let t = solver range_primitive dirvec startp in
       if t <> 0 then
	 let tp = solver_dist.(0) in
	 if fless tp tmin.(0)
	 then (solve_one_or_network 1 head dirvec)
	 else ()
       else ()
      );
    trace_or_matrix (ofs + 1) or_network dirvec
  )
in

(**** ��������若�号��篏� ****)
(* ��������若�拷��紮���� ViewPoint ��������������鴻����������鴻�㏍�ｃ�恰�劫�������������� *)
(* Vscan ���������篋ょ�� crashed_point ���茵�腦���������������吾�с�����         *)
(* crashed_object ���菴���������∽�域��篏����菴������ゃ��篋ょ�鴻�������＜�������遵�ゃ�� *)
let rec judge_intersection dirvec = (
  tmin.(0) <- (1000000000.0);
  trace_or_matrix 0 (or_net.(0)) dirvec;
  let t = tmin.(0) in

  if (fless (-0.1) t) then
    (fless t 100000000.0)
  else false
 )
in

(******************************************************************************
   ���膩�������篏����篋ゅ勲��ゅ�� 蕭�������
 *****************************************************************************)

let rec solve_each_element_fast iand_ofs and_group dirvec =
  let vec = (d_vec dirvec) in
  let iobj = and_group.(iand_ofs) in
  if iobj = -1 then ()
  else (
    let t0 = solver_fast2 iobj dirvec in
    if t0 <> 0 then
      (
        (* 篋ょ�鴻�����������������������篋ょ�鴻��篁����荀�膣����筝㏍�������障�����������������茯帥�鴻�����*)
        (* 篁���障�с��筝㏍�ф��絨���� t �����ゃ��罸���鴻�����*)
       let t0p = solver_dist.(0) in

       if (fless 0.0 t0p) then
	 if (fless t0p tmin.(0)) then
	   (
	    let t = t0p +. 0.01 in
	    let q0 = vec.(0) *. t +. startp_fast.(0) in
	    let q1 = vec.(1) *. t +. startp_fast.(1) in
	    let q2 = vec.(2) *. t +. startp_fast.(2) in
	    if check_all_inside 0 and_group q0 q1 q2 then
	      (
		tmin.(0) <- t;
		vecset intersection_point q0 q1 q2;
		intersected_object_id.(0) <- iobj;
		intsec_rectside.(0) <- t0;
	       )
	    else ()
	   )
	 else ()
       else ();
       solve_each_element_fast (iand_ofs + 1) and_group dirvec
      )
    else
       (* 篋ょ�鴻�����������������������������篏���������眼�����������������篁ヤ��篋ょ�鴻�������� *)
       if o_isinvert (objects.(iobj)) then
	 solve_each_element_fast (iand_ofs + 1) and_group dirvec
       else ()
   )
in

(**** 1��ゃ�� OR-group �����ゃ�����篋ょ�鴻��茯帥�鴻�� ****)
let rec solve_one_or_network_fast ofs or_group dirvec =
  let head = or_group.(ofs) in
  if head <> -1 then (
    let and_group = and_net.(head) in
    solve_each_element_fast 0 and_group dirvec;
    solve_one_or_network_fast (ofs + 1) or_group dirvec
   ) else ()
in

(**** OR��������������劫��篏������ゃ�����篋ょ�鴻��茯帥�鴻�����****)
let rec trace_or_matrix_fast ofs or_network dirvec =
  let head = or_network.(ofs) in
  let range_primitive = head.(0) in
  if range_primitive = -1 then (* �����������吾�с�����腟�篋� *)
    ()
  else (
    if range_primitive = 99 (* range primitive ������ *)
    then solve_one_or_network_fast 1 head dirvec
    else
      (
	(* range primitive ���茵�腦���������������遺困��鴻�������� *)
       let t = solver_fast2 range_primitive dirvec in
       if t <> 0 then
	 let tp = solver_dist.(0) in
	 if fless tp tmin.(0)
	 then (solve_one_or_network_fast 1 head dirvec)
	 else ()
       else ()
      );
    trace_or_matrix_fast (ofs + 1) or_network dirvec
   )
in

(**** ��������若�号��篏� ****)
let rec judge_intersection_fast dirvec =
(
  tmin.(0) <- (1000000000.0);
  trace_or_matrix_fast 0 (or_net.(0)) dirvec;
  let t = tmin.(0) in

  if (fless (-0.1) t) then
    (fless t 100000000.0)
  else false
)
in

(******************************************************************************
   ���篏����������篋ゅ勲��鴻��羈�膩����������������羆���������∽��
 *****************************************************************************)

(**** 篋ょ�鴻�����羈�膩����������������荐�膊������� ****)
(* 茵�腦���������������吾�с��������羆������������� solver ���菴������ゃ�� *)
(* 紊���� intsec_rectside 腟���宴�ф検������������綽�荀�������������  *)
(* nvector �����違�㏍�若�������� *)

let rec get_nvector_rect dirvec =
  let rectside = intsec_rectside.(0) in
  (* solver ���菴������ゃ����吟�ゃ����ｃ����≪����劫�����腓冴�� *)
  vecbzero nvector;
  nvector.(rectside-1) <- fneg (sgn (dirvec.(rectside-1)))
in

(* 綛渇�� *)
let rec get_nvector_plane m =
  (* m_invert ���絽吾�� true ��������� *)
  nvector.(0) <- fneg (o_param_a m); (* if m_invert then fneg m_a else m_a *)
  nvector.(1) <- fneg (o_param_b m);
  nvector.(2) <- fneg (o_param_c m)
in

(* 2罨≧�臥�� :  grad x^t A x = 2 A x ���罩ｈ����������� *)
let rec get_nvector_second m =
  let p0 = intersection_point.(0) -. o_param_x m in
  let p1 = intersection_point.(1) -. o_param_y m in
  let p2 = intersection_point.(2) -. o_param_z m in

  let d0 = p0 *. o_param_a m in
  let d1 = p1 *. o_param_b m in
  let d2 = p2 *. o_param_c m in

  if o_isrot m = 0 then (
    nvector.(0) <- d0;
    nvector.(1) <- d1;
    nvector.(2) <- d2
   ) else (
    nvector.(0) <- d0 +. fhalf (p1 *. o_param_r3 m +. p2 *. o_param_r2 m);
    nvector.(1) <- d1 +. fhalf (p0 *. o_param_r3 m +. p2 *. o_param_r1 m);
    nvector.(2) <- d2 +. fhalf (p0 *. o_param_r2 m +. p1 *. o_param_r1 m)
   );
  vecunit_sgn nvector (o_isinvert m)

in

let rec get_nvector m dirvec =
  let m_shape = o_form m in
  if m_shape = 1 then
    get_nvector_rect dirvec
  else if m_shape = 2 then
    get_nvector_plane m
  else (* 2罨≧�臥�� or ���篏� *)
    get_nvector_second m
  (* retval = nvector *)
in

(******************************************************************************
   ���篏�茵���≪�����(��俄�������≧�ｅ��絨����)���羆�������
 *****************************************************************************)

(**** 篋ょ�剛�������������鴻����ｃ����蚊��荐�膊������� ****)
let rec utexture m p =
  let m_tex = o_texturetype m in
  (* ��堺�������������吾�с����������� *)
  texture_color.(0) <- o_color_red m;
  texture_color.(1) <- o_color_green m;
  texture_color.(2) <- o_color_blue m;
  if m_tex = 1 then
    (
     (* zx��劫����������с�������惹─罕� (G) *)
     let w1 = p.(0) -. o_param_x m in
     let flag1 =
       let d1 = (floor (w1 *. 0.05)) *. 20.0 in
      fless (w1 -. d1) 10.0
     in
     let w3 = p.(2) -. o_param_z m in
     let flag2 =
       let d2 = (floor (w3 *. 0.05)) *. 20.0 in
       fless (w3 -. d2) 10.0
     in
     texture_color.(1) <-
       if flag1
       then (if flag2 then 255.0 else 0.0)
       else (if flag2 then 0.0 else 255.0)
    )
  else if m_tex = 2 then
    (* y荵御�劫�������鴻�������ゃ�� (R-G) *)
    (
      let w2 = fsqr (sin (p.(1) *. 0.25)) in
      texture_color.(0) <- 255.0 *. w2;
      texture_color.(1) <- 255.0 *. (1.0 -. w2)
    )
  else if m_tex = 3 then
    (* ZX��∽�劫��������綽���� (G-B) *)
    (
      let w1 = p.(0) -. o_param_x m in
      let w3 = p.(2) -. o_param_z m in
      let w2 = sqrt (fsqr w1 +. fsqr w3) /. 10.0 in
      let w4 =  (w2 -. floor w2) *. 3.1415927 in
      let cws = fsqr (cos w4) in
      texture_color.(1) <- cws *. 255.0;
      texture_color.(2) <- (1.0 -. cws) *. 255.0
    )
  else if m_tex = 4 then (
    (* �����≫����������� (B) *)
    let w1 = (p.(0) -. o_param_x m) *. (sqrt (o_param_a m)) in
    let w3 = (p.(2) -. o_param_z m) *. (sqrt (o_param_c m)) in
    let w4 = (fsqr w1) +. (fsqr w3) in
    let w7 =
      if fless (fabs w1) 1.0e-4 then
	15.0 (* atan +infty = pi/2 *)
      else
	let w5 = fabs (w3 /. w1)
	in
	((atan w5) *. 30.0) /. 3.1415927
    in
    let w9 = w7 -. (floor w7) in

    let w2 = (p.(1) -. o_param_y m) *. (sqrt (o_param_b m)) in
    let w8 =
      if fless (fabs w4) 1.0e-4 then
	15.0
      else
	let w6 = fabs (w2 /. w4)
	in ((atan w6) *. 30.0) /. 3.1415927
    in
    let w10 = w8 -. (floor w8) in
    let w11 = 0.15 -. (fsqr (0.5 -. w9)) -. (fsqr (0.5 -. w10)) in
    let w12 = if fisneg w11 then 0.0 else w11 in
    texture_color.(2) <- (255.0 *. w12) /. 0.3
   )
  else ()
in

(******************************************************************************
   茵�腦���鴻��綵����������羣������贋･���������絨�������荐�膊���������∽�亥召
 *****************************************************************************)

(* 綵������ｃ����������������≧�ｅ�����筝�絎������♂�√��絨�������������絲�筝����RGB��ゃ�����膊� *)
let rec add_light bright hilight hilight_scale =

  (* ��≧�ｅ�� *)
  if fispos bright then
    vecaccum rgb bright texture_color
  else ();

  (* 筝�絎������♂�√��絨� cos ^4 ��≪����� *)
  if fispos hilight then (
    let ihl = fsqr (fsqr hilight) *. hilight_scale in
    rgb.(0) <- rgb.(0) +. ihl;
    rgb.(1) <- rgb.(1) +. ihl;
    rgb.(2) <- rgb.(2) +. ihl
  ) else ()
in

(* ������篏�������������羣�������絨�������荐�膊���������∽��(��贋�剛�����綛渇�≪�����) *)
let rec trace_reflections index diffuse hilight_scale dirvec =

  if index >= 0 then (
    let rinfo = reflections.(index) in (* ��≦抗��≪�����絨������� *)
    let dvec = r_dvec rinfo in    (* ���絨���������劫��������������(��������������� *)

    (*���絨�������������������������絎���������������♂�≪��綵���������違�����絨�������絮���������醇�ф����� *)
    if judge_intersection_fast dvec then
      let surface_id = intersected_object_id.(0) * 4 + intsec_rectside.(0) in
      if surface_id = r_surface_id rinfo then
	(* ��♂�≪�����茵�腦���鴻�����羣����綵宴�������ｃ����������������医��絨�������絮���� *)
        if not (shadow_check_one_or_matrix 0 or_net.(0)) then
	  (* 絮����������絨������������� RGB��������吾��絲�筝�������膊� *)
          let p = veciprod nvector (d_vec dvec) in
          let scale = r_bright rinfo in
          let bright = scale *. diffuse *. p in
          let hilight = scale *. veciprod dirvec (d_vec dvec) in
          add_light bright hilight hilight_scale
        else ()
      else ()
    else ();
    trace_reflections (index - 1) diffuse hilight_scale dirvec
  ) else ()

in

(******************************************************************************
   ��贋･������菴処掘������
 *****************************************************************************)
let rec trace_ray nref energy dirvec pixel dist =
  if nref <= 4 then (
    let surface_ids = p_surface_ids pixel in
    if judge_intersection dirvec then (
    (* ��������吾�с����������吟�ゃ����ｃ����翫�� *)
      let obj_id = intersected_object_id.(0) in
      let obj = objects.(obj_id) in
      let m_surface = o_reflectiontype obj in
      let diffuse = o_diffuse obj *. energy in

      get_nvector obj dirvec; (* 羈�膩���������������� get *)
      veccpy startp intersection_point;  (* 篋ゅ勲��鴻����違�������������阪����鴻�������� *)
      utexture obj intersection_point; (*��������鴻����ｃ��荐�膊� *)

      (* pixel tuple��������宴����主�������� *)
      surface_ids.(nref) <- obj_id * 4 + intsec_rectside.(0);
      let intersection_points = p_intersection_points pixel in
      veccpy intersection_points.(nref) intersection_point;

      (* ��≧�ｅ��絨�������0.5篁ヤ�������翫�������翠���･��������泣�潟�������潟�違��茵���� *)
      let calc_diffuse = p_calc_diffuse pixel in
      if fless (o_diffuse obj) 0.5 then
	calc_diffuse.(nref) <- false
      else (
	calc_diffuse.(nref) <- true;
	let energya = p_energy pixel in
	veccpy energya.(nref) texture_color;
	vecscale energya.(nref) ((1.0 /. 256.0) *. diffuse);
	let nvectors = p_nvectors pixel in
	veccpy nvectors.(nref) nvector;
       );

      let w = (-2.0) *. veciprod dirvec nvector in
      (* ���絨���������劫�������������若�号�劫�����紊���� *)
      vecaccum dirvec w nvector;

      let hilight_scale = energy *. o_hilight obj in

      (* ���羣���������贋･絮������翫�����RGB�����������������������潟����� *)
      if not (shadow_check_one_or_matrix 0 or_net.(0)) then
        let bright = fneg (veciprod nvector light) *. diffuse in
        let hilight = fneg (veciprod dirvec light) in
        add_light bright hilight hilight_scale
      else ();

      (* ���羣����������絨���������＜������｢��� *)
      setup_startp intersection_point;
      trace_reflections (n_reflections.(0)-1) diffuse hilight_scale dirvec;

      (* �����帥�� 0.1������紊���閬���ｃ����������������♂�√��絨�������菴処掘������ *)
      if fless 0.1 energy then (

	if(nref < 4) then
	  surface_ids.(nref+1) <- -1
	else ();

	if m_surface = 2 then (   (* 絎������♂�√��絨� *)
	  let energy2 = energy *. (1.0 -. o_diffuse obj) in
	  trace_ray (nref+1) energy2 dirvec pixel (dist +. tmin.(0))
	 ) else ();

       ) else ()

     ) else (
      (* ���������篏�������綵���������������ｃ����翫��������羣���������������������� *)

      surface_ids.(nref) <- -1;

      if nref <> 0 then (
	let hl = fneg (veciprod dirvec light) in
        (* 90属���莇���������翫�����0 (���������) *)
	if fispos hl then
	  (
	   (* �����ゃ����ゃ��綣桁墾���茹�綺���� cos^3 ���罸�箴� *)
	   let ihl = fsqr hl *. hl *. energy *. beam.(0) in
	   rgb.(0) <- rgb.(0) +. ihl;
	   rgb.(1) <- rgb.(1) +. ihl;
	   rgb.(2) <- rgb.(2) +. ihl
          )
	else ()
       ) else ()
     )
   ) else ()
in


(******************************************************************************
   ����･������菴処掘������
 *****************************************************************************)

(* ��������鴻����劫�������劫���������������������･������綣激�����荐�膊������� *)
(* ����･��������劫�������������� dirvec�����≪��������絎���違����若��������篏�������������������茵�腦���ゅ��
   ���蕭�������茵����������������篏����綵������ｃ��������������緇�������絨����菴処掘��������� *)
let rec trace_diffuse_ray dirvec energy =

  (* ���������������篏����綵����������茯帥�鴻�� *)
  if judge_intersection_fast dirvec then
    let obj = objects.(intersected_object_id.(0)) in
    get_nvector obj (d_vec dirvec);
    utexture obj intersection_point;

    (* ���������篏������上��������������綣激�����羆������������贋･���羣���������帥��荐�膊� *)
    if not (shadow_check_one_or_matrix 0 or_net.(0)) then
      let br =  fneg (veciprod nvector light) in
      let bright = (if fispos br then br else 0.0) in
      vecaccum diffuse_ray (energy *. bright *. o_diffuse obj) texture_color
    else ()
  else ()
in

(* ���������������羆冴�������������劫��������������������������絲障�������������������������壕����������ャ��
   ����･������綣激�������泣�潟�������潟�違��������膊������� *)
let rec iter_trace_diffuse_rays dirvec_group nvector org index =
  if index >= 0 then (
    let p = veciprod (d_vec dirvec_group.(index)) nvector in

    (* ��������� 2n ��������� 2n+1 ������������篋������������������劫�������������������ャ�ｃ��������
       羈�膩���������������������������������������吾����т戎��� *)
    if fisneg p then
      trace_diffuse_ray dirvec_group.(index + 1) (p /. -150.0)
    else
      trace_diffuse_ray dirvec_group.(index) (p /. 150.0);

    iter_trace_diffuse_rays dirvec_group nvector org (index - 2)
   ) else ()
in

(* 筝���������������劫��������������������������絲障�������������劫���������･��������泣�潟�������潟�違����� *)
let rec trace_diffuse_rays dirvec_group nvector org =
  setup_startp org;
  (* ��������� 2n ��������� 2n+1 ������������篋������������������劫�������������������ャ�ｃ�����������
     羈�膩���������������������������������������帥�泣�潟�������潟�違��篏帥�������� *)
  (* ��������� 120 / 2 = 60���������������������菴処掘 *)
  iter_trace_diffuse_rays dirvec_group nvector org 118
in

(* ��������劫��������������300��������������������������＜����障��菴処掘���������������罧�������240������
   �����������������ゃ���������･���菴処掘���������60������������������菴処掘���4��祉�����茵���� *)
let rec trace_diffuse_ray_80percent group_id nvector org =

  if group_id <> 0 then
    trace_diffuse_rays dirvecs.(0) nvector org
  else ();

  if group_id <> 1 then
    trace_diffuse_rays dirvecs.(1) nvector org
  else ();

  if group_id <> 2 then
    trace_diffuse_rays dirvecs.(2) nvector org
  else ();

  if group_id <> 3 then
    trace_diffuse_rays dirvecs.(3) nvector org
  else ();

  if group_id <> 4 then
    trace_diffuse_rays dirvecs.(4) nvector org
  else ()

in

(* 筝�筝�綏����4��鴻������･���菴処掘腟�������篏帥��������300���������������������������菴処掘����������･������
   荐�膊����������20%(60���)���菴処掘羝���������с��罧����80%(240���)���菴処掘������ *)
let rec calc_diffuse_using_1point pixel nref =

  let ray20p = p_received_ray_20percent pixel in
  let nvectors = p_nvectors pixel in
  let intersection_points = p_intersection_points pixel in
  let energya = p_energy pixel in

  veccpy diffuse_ray ray20p.(nref);
  trace_diffuse_ray_80percent
    (p_group_id pixel)
    nvectors.(nref)
    intersection_points.(nref);
  vecaccumv rgb energya.(nref) diffuse_ray

in

(* ���������筝�筝�綏����4��鴻��菴処掘腟����������膊�����������･������羆���������������ャ�� 300 ������������
   菴処掘������綽�荀����������������5��劫��膊�������������1��鴻��������60���(20%)菴処掘��������������ф����� *)

let rec calc_diffuse_using_5points x prev cur next nref =

  let r_up = p_received_ray_20percent prev.(x) in
  let r_left = p_received_ray_20percent cur.(x-1) in
  let r_center = p_received_ray_20percent cur.(x) in
  let r_right = p_received_ray_20percent cur.(x+1) in
  let r_down = p_received_ray_20percent next.(x) in

  veccpy diffuse_ray r_up.(nref);
  vecadd diffuse_ray r_left.(nref);
  vecadd diffuse_ray r_center.(nref);
  vecadd diffuse_ray r_right.(nref);
  vecadd diffuse_ray r_down.(nref);

  let energya = p_energy cur.(x) in
  vecaccumv rgb energya.(nref) diffuse_ray

in

(* 筝�筝�綏����4��鴻��篏帥����������贋･���������茵�腦���鴻���������������･���������荐�膊������� *)
let rec do_without_neighbors pixel nref =
  if nref <= 4 then
    (* 茵�腦���∝���垩��������(���莢�)��������с����� *)
    let surface_ids = p_surface_ids pixel in
    if surface_ids.(nref) >= 0 then (
      let calc_diffuse = p_calc_diffuse pixel in
      if calc_diffuse.(nref) then
	calc_diffuse_using_1point pixel nref
      else ();
      do_without_neighbors pixel (nref + 1)
     ) else ()
  else ()
in

(* ��糸�鋇���т��筝�綏��劻����鴻�����������(荀���������������糸�����腴���х�＜��篋�)���腆肴�� *)
let rec neighbors_exist x y next =
  if (y + 1) < image_size.(1) then
    if y > 0 then
      if (x + 1) < image_size.(0) then
	if x > 0 then
	  true
	else false
      else false
    else false
  else false
in

let rec get_surface_id pixel index =
  let surface_ids = p_surface_ids pixel in
  surface_ids.(index)
in

(* 筝�筝�綏����4��鴻����贋･���菴処掘���腟������������������������≪��茵�腦������������������������с�����
   ��������������≪��茵�腦���������������違�����������4��鴻��腟�������篏帥����������ц��膊���������ュ�堺�ャ�� *)
let rec neighbors_are_available x prev cur next nref =
  let sid_center = get_surface_id cur.(x) nref in

  if get_surface_id prev.(x) nref = sid_center then
    if get_surface_id next.(x) nref = sid_center then
      if get_surface_id cur.(x-1) nref = sid_center then
	if get_surface_id cur.(x+1) nref = sid_center then
	  true
	else false
      else false
    else false
  else false
in

(* ��贋･���������茵�腦���鴻���������������･���������綣激��������筝�筝�綏����4��鴻��腟�������篏睡��������荐�膊�
   ���������������筝�筝�綏����4��鴻��荐�膊�腟�������篏帥����������翫�������������������鴻��
   do_without_neighbors�����������帥����� *)

let rec try_exploit_neighbors x y prev cur next nref =
  let pixel = cur.(x) in
  if nref <= 4 then

    (* 茵�腦���∝���垩��������(���莢�)��� *)
    if get_surface_id pixel nref >= 0 then
      (* ������4��鴻��茖�絎����篏帥�������� *)
      if neighbors_are_available x prev cur next nref then (

	(* ����･���������荐�膊���������������違��腴���ｃ����������医��������荐�膊������� *)
	let calc_diffuse = p_calc_diffuse pixel in
        if calc_diffuse.(nref) then
	  calc_diffuse_using_5points x prev cur next nref
	else ();

	(* 罨＜�����絨�茵�腦���鴻�� *)
	try_exploit_neighbors x y prev cur next (nref + 1)
      ) else
	(* ������4��鴻��茖�絎����篏帥�������������с��������������篏帥����������号�������������帥����� *)
	do_without_neighbors cur.(x) nref
    else ()
  else ()
in

(******************************************************************************
   PPM�����＜�ゃ�������吾��莨若�翠�∽��
 *****************************************************************************)
let rec write_ppm_header _ =
  (
    print_char 80; (* 'P' *)
    print_char (48 + 3); (* +6 if binary *) (* 48 = '0' *)
    print_char 10;
    print_int image_size.(0);
    print_char 32;
    print_int image_size.(1);
    print_char 32;
    print_int 255;
    print_char 10
  )
in

let rec write_rgb_element x =
  let ix = int_of_float x in
  let elem = if ix > 255 then 255 else if ix < 0 then 0 else ix in
  print_int elem
in

let rec write_rgb _ =
   write_rgb_element rgb.(0); (* Red   *)
   print_char 32;
   write_rgb_element rgb.(1); (* Green *)
   print_char 32;
   write_rgb_element rgb.(2); (* Blue  *)
   print_char 10
in

(******************************************************************************
   �����������ゃ�潟��荐�膊����綽�荀���������宴�����������������罨＜�������ゃ�潟��菴処掘���茵���ｃ����������∽�亥召
 *****************************************************************************)

(* ����･��������泣�潟�������潟�違�с��筝�筝�綏����4��鴻��腟�������篏帥�������с��罨＜�������ゃ�潟��荐�膊����
   茵����������������腟���������������祉�������ゃ��荐�膊���с�������� *)

(* ����･������ 60���(20%)������荐�膊���������������∽�� *)
let rec pretrace_diffuse_rays pixel nref =
  if nref <= 4 then

    (* ��∝���垩�������鴻�� *)
    let sid = get_surface_id pixel nref in
    if sid >= 0 then (
      (* ����･������荐�膊���������������違��腴���ｃ����������� *)
      let calc_diffuse = p_calc_diffuse pixel in
      if calc_diffuse.(nref) then (
	let group_id = p_group_id pixel in
	vecbzero diffuse_ray;

	(* 5��ゃ����劫��������������������(���60���)�����������������違����若��ID���絲上��������������
	   筝���ら�吾����ц申莊� *)
	let nvectors = p_nvectors pixel in
	let intersection_points = p_intersection_points pixel in
	trace_diffuse_rays
	  dirvecs.(group_id)
	  nvectors.(nref)
	  intersection_points.(nref);
	let ray20p = p_received_ray_20percent pixel in
	veccpy ray20p.(nref) diffuse_ray
       ) else ();
      pretrace_diffuse_rays pixel (nref + 1)
     ) else ()
  else ()
in

(* �����������祉�����絲障�������贋･���菴処掘�������･���������20%������荐�膊����茵���� *)

let rec pretrace_pixels line x group_id lc0 lc1 lc2 =
  if x >= 0 then (

    let xdisp = scan_pitch.(0) *. float_of_int (x - image_center.(0)) in
    ptrace_dirvec.(0) <- xdisp *. screenx_dir.(0) +. lc0;
    ptrace_dirvec.(1) <- xdisp *. screenx_dir.(1) +. lc1;
    ptrace_dirvec.(2) <- xdisp *. screenx_dir.(2) +. lc2;
    vecunit_sgn ptrace_dirvec false;
    vecbzero rgb;
    veccpy startp viewpoint;

    (* ��贋･���菴処掘 *)
    trace_ray 0 1.0 ptrace_dirvec line.(x) 0.0;
    veccpy (p_rgb line.(x)) rgb;
    p_set_group_id line.(x) group_id;

    (* ����･������20%���菴処掘 *)
    pretrace_diffuse_rays line.(x) 0;

    pretrace_pixels line (x-1) (add_mod5 group_id 1) lc0 lc1 lc2

   ) else ()
in

(* �����������ゃ�潟�������������祉�����絲障����贋･���菴処掘�������･������20%������荐�膊���������� *)
let rec pretrace_line line y group_id =
  let ydisp = scan_pitch.(0) *. float_of_int (y - image_center.(1)) in

  (* �����ゃ�潟��筝㊤�����������������������������荐�膊� *)
  let lc0 = ydisp *. screeny_dir.(0) +. screenz_dir.(0) in
  let lc1 = ydisp *. screeny_dir.(1) +. screenz_dir.(1) in
  let lc2 = ydisp *. screeny_dir.(2) +. screenz_dir.(2) in
  pretrace_pixels line (image_size.(0) - 1) group_id lc0 lc1 lc2
in


(******************************************************************************
   ��贋･���菴処掘�������･���20%菴処掘���腟�������������腟���������������祉����ゃ��荐�膊���������∽��
 *****************************************************************************)

(* �����������祉��������腟���������������祉����ゃ��荐�膊� *)
let rec scan_pixel x y prev cur next =
  if x < image_size.(0) then (

    (* ��障�������贋･���菴処掘��у�����������RGB��ゃ��緇���� *)
    veccpy rgb (p_rgb cur.(x));

    (* 罨＜�������贋･���������茵�腦���鴻����ゃ������������･���������������絲�筝���������潟����� *)
    if neighbors_exist x y next then
      try_exploit_neighbors x y prev cur next 0
    else
      do_without_neighbors cur.(x) 0;

    (* 緇������������ゃ��PPM�����＜�ゃ�������阪�� *)
    write_rgb ();

    scan_pixel (x + 1) y prev cur next
   ) else ()
in

(* 筝������ゃ�喝�������������祉����ゃ��荐�膊� *)
let rec scan_line y prev cur next group_id = (

  if y < image_size.(1) then (

    if y < image_size.(1) - 1 then
      pretrace_line next (y + 1) group_id
    else ();
    scan_pixel 0 y prev cur next;
    scan_line (y + 1) cur next prev (add_mod5 group_id 2);
   ) else ()
)
in

(******************************************************************************
   ��������祉����������宴����主�������������若�炊����������蚊��綵������∽�亥召
 *****************************************************************************)

(* 3罨≦�����������������5荀�膣������������蚊��綵���� *)
let rec create_float5x3array _ = (
  let vec = create_array 3 0.0 in
  let array = create_array 5 vec in
  array.(1) <- create_array 3 0.0;
  array.(2) <- create_array 3 0.0;
  array.(3) <- create_array 3 0.0;
  array.(4) <- create_array 3 0.0;
  array
)
in

(* ��������祉�����茵����tuple�����蚊��綵���� *)
let rec create_pixel _ =
  let m_rgb = create_array 3 0.0 in
  let m_isect_ps = create_float5x3array() in
  let m_sids = create_array 5 0 in
  let m_cdif = create_array 5 false in
  let m_engy = create_float5x3array() in
  let m_r20p = create_float5x3array() in
  let m_gid = create_array 1 0 in
  let m_nvectors = create_float5x3array() in
  (m_rgb, m_isect_ps, m_sids, m_cdif, m_engy, m_r20p, m_gid, m_nvectors)
in

(* 罔���劫��1�����ゃ�割賢��������������祉��荀�膣������蚊��綵������� *)
let rec init_line_elements line n =
  if n >= 0 then (
    line.(n) <- create_pixel();
    init_line_elements line (n-1)
   ) else
    line
in

(* 罔���劫��1�����ゃ�喝�������������祉�����������篏���� *)
let rec create_pixelline _ =
  let line = create_array image_size.(0) (create_pixel()) in
  init_line_elements line (image_size.(0)-2)
in

(******************************************************************************
   ����･��������泣�潟�������潟�違����ゃ�������劫��������������臂ゃ��荐�膊���������∽�亥召
 *****************************************************************************)

(* ��������������������堺�ャ��������筝�罕�������絽����������������600��������劫��������������������������絎�������
   腴���剛��筝���������≪��100��������ゅ��絽����������������������100������腴���剛��筝������≫�����10 x 10 ���
   ��弱����吟��筝���吟�����������������篏帥�������������������с����壕��������������������������絲�綺����綏����
   紊с����������с�����������茖�罩ｃ�����������������������腟���������������� *)

let rec tan x =
  sin(x) /. cos(x)
in

(* ��������������������堺�ャ�������������∝�吟��筝�罕�������絽�������������綺ф�����茖�罩ｃ����� *)
let rec adjust_position h ratio =
  let l = sqrt(h*.h +. 0.1) in
  let tan_h = 1.0 /. l in
  let theta_h = atan tan_h in
   let tan_m = tan (theta_h *. ratio) in
  tan_m *. l
in

(* ��������������������堺�ャ�������������∝�吟��筝�罕�������絽�������������������������荐�膊������� *)
let rec calc_dirvec icount x y rx ry group_id index =
  if icount >= 5 then (
    let l = sqrt(fsqr x +. fsqr y +. 1.0) in
    let vx = x /. l in
    let vy = y /. l in
    let vz = 1.0 /. l in

    (* 腴���剛��������絲丞О������絽���������� *)
    let dgroup = dirvecs.(group_id) in
    vecset (d_vec dgroup.(index))    vx vy vz;
    vecset (d_vec dgroup.(index+40)) vx vz (fneg vy);
    vecset (d_vec dgroup.(index+80)) vz (fneg vx) (fneg vy);
    vecset (d_vec dgroup.(index+1)) (fneg vx) (fneg vy) (fneg vz);
    vecset (d_vec dgroup.(index+41)) (fneg vx) (fneg vz) vy;
    vecset (d_vec dgroup.(index+81)) (fneg vz) vx vy
   ) else
    let x2 = adjust_position y rx in
    calc_dirvec (icount + 1) x2 (adjust_position x2 ry) rx ry group_id index
in

(* 腴���剛��筝���� 10x10��弱�����茵�筝㏍��������������������荐�膊������� *)
let rec calc_dirvecs col ry group_id index =
  if col >= 0 then (
    (* 綏������� *)
    let rx = (float_of_int col) *. 0.2 -. 0.9 in (* ������綺ф�� *)
    calc_dirvec 0 0.0 0.0 rx ry group_id index;
    (* �勀����� *)
    let rx2 = (float_of_int col) *. 0.2 +. 0.1 in (* ������綺ф�� *)
    calc_dirvec 0 0.0 0.0 rx2 ry group_id (index + 2);

    calc_dirvecs (col - 1) ry (add_mod5 group_id 1) index
   ) else ()
in

(* 腴���剛��筝����10x10��弱��������茵����絲障��������������������������荐�膊������� *)
let rec calc_dirvec_rows row group_id index =
  if row >= 0 then (
    let ry = (float_of_int row) *. 0.2 -. 0.9 in (* 茵����綺ф�� *)
    calc_dirvecs 4 ry group_id index; (* 筝�茵����荐�膊� *)
    calc_dirvec_rows (row - 1) (add_mod5 group_id 2) (index + 4)
   ) else ()
in

(******************************************************************************
   dirvec �����＜�≪����蚊��綵�������茵����
 *****************************************************************************)


let rec create_dirvec _ =
  let v3 = create_array 3 0.0 in
  let consts = create_array n_objects.(0) v3 in
  (v3, consts)
in

let rec create_dirvec_elements d index =
  if index >= 0 then (
    d.(index) <- create_dirvec();
    create_dirvec_elements d (index - 1)
   ) else ()
in

let rec create_dirvecs index =
  if index >= 0 then (
    dirvecs.(index) <- create_array 120 (create_dirvec());
    create_dirvec_elements dirvecs.(index) 118;
    create_dirvecs (index-1)
   ) else ()
in

(******************************************************************************
   茖������∽�育�������若�喝�冴�����dirvec���������������茵����
 *****************************************************************************)

let rec init_dirvec_constants vecset index =
  if index >= 0 then (
    setup_dirvec_constants vecset.(index);
    init_dirvec_constants vecset (index - 1)
   ) else ()
in

let rec init_vecset_constants index =
  if index >= 0 then (
    init_dirvec_constants dirvecs.(index) 119;
    init_vecset_constants (index - 1)
   ) else ()
in

let rec init_dirvecs _ =
  create_dirvecs 4;
  calc_dirvec_rows 9 0 0;
  init_vecset_constants 4
in

(******************************************************************************
   絎������♂�√��絨���������������ょ��篏�������絨������宴�����������������
 *****************************************************************************)

(* ���絨�綛渇�≪��菴遵�������� *)
let rec add_reflection index surface_id bright v0 v1 v2 =
  let dvec = create_dirvec() in
  vecset (d_vec dvec) v0 v1 v2; (* ���絨������������� *)
  setup_dirvec_constants dvec;

  reflections.(index) <- (surface_id, dvec, bright)
in

(* ��贋�剛����������≪����ゃ����������宴��菴遵�������� *)
let rec setup_rect_reflection obj_id obj =
  let sid = obj_id * 4 in
  let nr = n_reflections.(0) in
  let br = 1.0 -. o_diffuse obj in
  let n0 = fneg light.(0) in
  let n1 = fneg light.(1) in
  let n2 = fneg light.(2) in
  add_reflection nr (sid+1) br light.(0) n1 n2;
  add_reflection (nr+1) (sid+2) br n0 light.(1) n2;
  add_reflection (nr+2) (sid+3) br n0 n1 light.(2);
  n_reflections.(0) <- nr + 3
in

(* 綛渇�≪����ゃ����������宴��菴遵�������� *)
let rec setup_surface_reflection obj_id obj =
  let sid = obj_id * 4 + 1 in
  let nr = n_reflections.(0) in
  let br = 1.0 -. o_diffuse obj in
  let p = veciprod light (o_param_abc obj) in

  add_reflection nr sid br
    (2.0 *. o_param_a obj *. p -. light.(0))
    (2.0 *. o_param_b obj *. p -. light.(1))
    (2.0 *. o_param_c obj *. p -. light.(2));
  n_reflections.(0) <- nr + 1
in


(* �����������吾�с��������絲障��������絨�������綛渇�≪����������違����������宴��菴遵�������� *)
let rec setup_reflections obj_id =
  if obj_id >= 0 then
    let obj = objects.(obj_id) in
    if o_reflectiontype obj = 2 then
      if fless (o_diffuse obj) 1.0 then
	let m_shape = o_form obj in
	(* ��贋�剛�����綛渇�≪����帥�泣����若�� *)
	if m_shape = 1 then
	  setup_rect_reflection obj_id obj
	else if m_shape = 2 then
	  setup_surface_reflection obj_id obj
	else ()
      else ()
    else ()
  else ()
in

(*****************************************************************************
   ���篏������九勝
 *****************************************************************************)

(* �����ゃ�������������鴻�����������茵������∽�違�����罨≦�若�喝�冴�� *)
let rec rt size_x size_y =
(
 image_size.(0) <- size_x;
 image_size.(1) <- size_y;
 image_center.(0) <- size_x / 2;
 image_center.(1) <- size_y / 2;
 scan_pitch.(0) <- 128.0 /. float_of_int size_x;
 let prev = create_pixelline () in
 let cur  = create_pixelline () in
 let next = create_pixelline () in
 read_parameter();
 write_ppm_header ();
 init_dirvecs();
 veccpy (d_vec light_dirvec) light;
 setup_dirvec_constants light_dirvec;
 setup_reflections (n_objects.(0) - 1);
 pretrace_line cur 0 0;
 scan_line 0 prev cur next 2
)
in

let _ = rt 512 512

in 0

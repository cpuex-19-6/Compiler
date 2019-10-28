let rec trace_ray nref energy dirvec pixel dist =
  if nref <= 4 then (
    let surface_ids = p_surface_ids pixel in
    if judge_intersection dirvec then (
    (* オブジェクトにぶつかった場合 *)
      let obj_id = intersected_object_id.(0) in
      let obj = objects.(obj_id) in
      let m_surface = o_reflectiontype obj in
      let diffuse = o_diffuse obj *. energy in

      get_nvector obj dirvec; (* 法線ベクトルを get *)
      veccpy startp intersection_point;  (* 交差点を新たな光の発射点とする *)
      utexture obj intersection_point; (*テクスチャを計算 *)

      (* pixel tupleに情報を格納する *)
      surface_ids.(nref) <- obj_id * 4 + intsec_rectside.(0);
      let intersection_points = p_intersection_points pixel in
      veccpy intersection_points.(nref) intersection_point;

      (* 拡散反射率が0.5以上の場合のみ間接光のサンプリングを行う *)
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
      (* 反射光の方向にトレース方向を変更 *)
      vecaccum dirvec w nvector;

      let hilight_scale = energy *. o_hilight obj in

      (* 光源光が直接届く場合、RGB成分にこれを加味する *)
      if not (shadow_check_one_or_matrix 0 or_net.(0)) then
        let bright = fneg (veciprod nvector light) *. diffuse in
        let hilight = fneg (veciprod dirvec light) in
        add_light bright hilight hilight_scale
      else ();

      (* 光源光の反射光が無いか探す *)
      setup_startp intersection_point;
      trace_reflections (n_reflections.(0)-1) diffuse hilight_scale dirvec;

      (* 重みが 0.1より多く残っていたら、鏡面反射元を追跡する *)
      if fless 0.1 energy then (

	if(nref < 4) then
	  surface_ids.(nref+1) <- -1
	else ();

	if m_surface = 2 then (   (* 完全鏡面反射 *)
	  let energy2 = energy *. (1.0 -. o_diffuse obj) in
	  trace_ray (nref+1) energy2 dirvec pixel (dist +. tmin.(0))
	 ) else ();

       ) else ()

     ) else (
      (* どの物体にも当たらなかった場合。光源からの光を加味 *)

      surface_ids.(nref) <- -1;

      if nref <> 0 then (
	let hl = fneg (veciprod dirvec light) in
        (* 90°を超える場合は0 (光なし) *)
	if fispos hl then
	  (
	   (* ハイライト強度は角度の cos^3 に比例 *)
	   let ihl = fsqr hl *. hl *. energy *. beam.(0) in
	   rgb.(0) <- rgb.(0) +. ihl;
	   rgb.(1) <- rgb.(1) +. ihl;
	   rgb.(2) <- rgb.(2) +. ihl
          )
	else ()
       ) else ()
     )
   ) else ()
in ()
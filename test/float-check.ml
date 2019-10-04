let rec test_fadd m n =
  m +. n in
let rec test_fsub m n =
  m -. n in
let rec test_fmul m n =
  m *. n in
let rec test_fdiv m n =
  m /. n in
let rec test_sqrt n =
  sqrt n in
let rec test_sin n =
  sin n in
let rec test_cos n =
  cos n in
let rec test_atan n =
  atan n in
let rec test_fhalf n = (* ×0.5 *)
  fhalf n in
let rec test_fsqr n = (* n×n *)
  fsqr n in
let rec test_fabs n =
  fabs n in
let rec test_fneg n =
  fneg n in
let rec test_fless m n = (* m < n *)
  fless m n in
let rec test_fiszero n = 
  fiszero n in
let rec test_fispos n =
  fispos n in
let rec test_fisneg n =
  fisneg n in
let rec test_floor n = (* 床関数 *)
  floor n in
let rec test_ftoi n = (* int_of_float *)
  int_of_float n in
let rec test_itof n = (* float_of_int *)
  float_of_int n in
let _ = test_fadd 0.2 0.5 in
let _ = test_fsub 0.2 0.5 in
let _ = test_fmul 0.2 0.5 in
let _ = test_fdiv 0.2 0.5 in
let _ = test_sqrt 1.0 in
let _ = test_sin 1.0 in
let _ = test_cos 1.0 in
let _ = test_atan 1.0 in
let _ = test_fhalf 1.0 in
let _ = test_fsqr 1.0 in
let _ = test_fabs -1.0 in
let _ = test_fneg -1.0 in
let _ = test_fless 0.2 0.5 in
let _ = test_fiszero 0.1 in
let _ = test_fispos 0.1 in
let _ = test_fisneg 0.1 in
let _ = test_floor -0.25 in
let _ = test_ftoi 3.0 in
let _ = test_itof 3 in
()

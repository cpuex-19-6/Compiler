let rec pi_div e x = 
 (* if 0. <= e && e < (3.1415926535*.2.) then e 
else*) if ((0. > e) && (-.e <= x)) then  pi_div (e+.x) (x/.2.)
(*else if 0. < e && e <= x then pi_div (e-.x/.2.) (x/.2.)*)
else pi_div e (x*.2.) in
let a = pi_div 3.0 (3.1415927*.2.) in
  ()
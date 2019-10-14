let rec pi_div e x = 
 (*if (not (fless e 0.)) && fless e (3.1415926535*.2.) then e 
else*) if (*(fless e 0.)*)true && (*(not (fless x (-.e)))*)true then  pi_div (e+.x) (x/.2.)
(*else if 0. < e && e <= x then pi_div (e-.x/.2.) (x/.2.)*)
else pi_div e (x*.2.) in
let a = pi_div 3.0 (3.1415927*.2.) in
  ()
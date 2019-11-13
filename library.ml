 let pi = 3.1415926535 in

 let rec pi_div e x = 
     if (0. <= e) && e < (3.1415926535*.2.) then e 
     else if (e < 0.) && (x >= (-.e))then  pi_div (e+.x) (x/.2.)
     else if  (0. < e) &&  (x >= e)  then pi_div (e-.x/.2.) (x/.2.)
     else pi_div e (x*.2.) in

let rec pi4div x = 
      if  x < (3.1415927/.2.) then (x,1.)
      else if  x < 3.1415927 then (3.1415927-.x,-.1.)
      else if x < (3.1415927*.1.5) then (x-.3.1415927,-.1.)
      else (3.1415927*.2.-.x,1.) in 

let rec pi4div2 x = 
      if x < (3.1415927/.2.) then (x,1.)
      else if x < 3.1415927 then (3.1415927-.x,1.)
      else if x < (3.1415927*.1.5) then (x-.3.1415927,-.1.)
      else (3.1415927*.2.-.x,-.1.) in 
  
let rec tailor_cos y = 
     let xx = y *. y in
     let t2 = xx /. 2. in
     let t4 = xx *. t2 /. 12. in
     let t6 = xx *. t4 /. 30. in
     let t8 = xx *. t6 /. 56. in
     let t10 = xx *. t8 /. 90. in
     let t12 = xx *. t10 /. 132. in
         1. -. t2 +. t4 -. t6 +. t8 -. t10 +. t12 in 

(*let rec tailor_sin y = 
     let xx = y *. y in
     let t3 = y *. xx /. 6. in
     let t5 = y *. t3 /. 20. in
     let t7 = y *. t5 /. 42. in
     let t9 = y *. t7 /. 72. in
     let t11 = y *. t9 /. 110. in
     let t13 = y *. t11 /. 132. in
     y -. t3 +. t5 -. t7 +. t9 -. t11 +. t13 in*)

let rec cos x = 
  let (a,b) = (pi4div(pi_div x (3.1415926535 *. 2.))) in
  b *. (tailor_cos a)
  in

let rec sin x = 
  let (a,b) = pi4div2(pi_div x (3.1415926535 *. 2.)) in
  b *. (tailor_cos ((pi/. 2.) -. a)) in

let rec tailor_atan y = 
  let xx = y *. y in
  let t3 = y *. xx /. 3. in
  let t5 = xx *. xx *. y /. 5. in
  let t7 = xx *. xx *. xx *. y /. 7. in
  let t9 = xx *. xx *. xx *. xx *. y /. 9. in
  let t11 = xx *. xx *. xx *. xx *. xx *. xx *. y /. 11. in
  y -. t3 +. t5 -. t7 +. t9 -. t11 in

let rec atan y = 
  if y < 0. then -.(atan (-.y)) 
  else if y > 1. then (pi /. 2.)-. (atan(1. /. y)) 
  else if y > 0.41421356 then (pi /. 4.) +. atan ((y-.1.)/.(1.+.y)) 
  else tailor_atan y
  in 
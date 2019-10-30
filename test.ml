(*let rec tailor y = 
  let xx = y *. y in
  let t2 = xx /. 2. in
  let t4 = xx *. t2 /. 12. in
  let t6 = xx *. t4 /. 30. in
  let t8 = xx *. t6 /. 56. in
  let t10 = xx *. t8 /. 90. in
  let t12 = xx *. t10 /. 132. in
      1. (*-. t2 +. t4 -. t6 +. t8 -. t10 +. t12*) in let _ = tailor 1. in ()*)

 let rec tailor y = 1/     
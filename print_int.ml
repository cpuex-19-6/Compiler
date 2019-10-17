(*let rec my_print_int x = 
  if x < 0 then (print_char '-' ;(my_print_int (-x)))
  else if x < 10 then print_char(char_of_int(x+48))
  else (my_print_int(x/10);print_char(char_of_int((x mod 10) + 48))) *)

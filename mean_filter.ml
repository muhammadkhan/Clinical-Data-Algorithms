module P = Data_parser.DataParser

let to_stencil k =
  let rec helper k' =
    if k' = 0 then []
    else (1. /. (float_of_int k))::helper(k' - 1)
  in
  helper k

let all_data = P.parse "CS5540_ecog(txt).txt"

(*Part 1*)

let c1 = Analysis.choose_rand all_data
let c2 = Analysis.choose_rand all_data
let c3 = Analysis.choose_rand all_data

let s = to_stencil 3

let c1' = Analysis.convolution s c1
let c2' = Analysis.convolution s c2
let c3' = Analysis.convolution s c3

(*let intify (lst : float list) : (int*int) array =
  let i = ref (-1) in
  let f (fl : float) =
    incr i;
    (10*(!i), 150*(int_of_float fl))
  in
  Array.of_list (List.map f lst)*)

let run () = 
  Io.write_strs_to_file (List.map Io.signal_to_string [c1;c1';c2;c2';c3;c3']) "out.txt";
  print_endline "File written!"

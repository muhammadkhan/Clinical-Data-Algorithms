module P = Data_parser.DataParser

let to_stencil k =
  let rec helper k' =
    if k' = 0 then []
    else (1. /. (float_of_int k))::helper(k' - 1)
  in
  helper k

let all_data (s:string) = P.parse s

(*Part 1*)

(*let intify (lst : float list) : (int*int) array =
  let i = ref (-1) in
  let f (fl : float) =
    incr i;
    (10*(!i), 150*(int_of_float fl))
  in
  Array.of_list (List.map f lst)*)


(**I did it this way so that we are able to run multiple 
trials without having to build every time.*)
let mean_filter (s:string) = 
  let c1 = Analysis.choose_rand (all_data s) in
  let c2 = Analysis.choose_rand (all_data s) in
  let c3 = Analysis.choose_rand (all_data s) in

  let k = to_stencil 3 in  

  let c1' = Analysis.convolution k c1
  let c2' = Analysis.convolution k c2
  let c3' = Analysis.convolution k c3
  Io.write_strs_to_file (List.map Io.signal_to_string [c1;c1';c2;c2';c3;c3']) "meanfilter.txt";
  print_endline "File written!"

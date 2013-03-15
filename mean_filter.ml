module P = Data_parser.DataParser

let to_stencil k =
  let rec helper k' =
    if k' = 0 then []
    else (1. /. (float_of_int k))::helper(k' - 1)
  in
  helper k

let all_data = P.parse "test.txt"

(*Part 1*)
(*
let c1 = (print_endline "parsed!"; Analysis.choose_rand all_data)
let c2 = Analysis.choose_rand all_data
let c3 = Analysis.choose_rand all_data*)

let s = to_stencil 3
(*
let c1' = Analysis.convolution s c1
let c2' = Analysis.convolution s c2
let c3' = Analysis.convolution s c3*)

let x1 = (print_endline "parsed!"; Array.get all_data 0)
let x1' = (print_endline "got x1"; Analysis.convolution s x1)

(*let intify (lst : float list) : (int*int) array =
  let i = ref (-1) in
  let f (fl : float) =
    incr i;
    (10*(!i), 150*(int_of_float fl))
  in
  Array.of_list (List.map f lst)*)
let () = print_endline "convolved"
let run () = 
  Io.write_strs_to_file (List.map Io.signal_to_string [x1;x1']) "out.txt";
  print_endline "File written!"

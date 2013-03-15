(**This is the module to handle the calculations of the 
autocorrelation function *)

module P = Data_parser.DataParser
module A = Analysis
module M = Mean_filter

(*Chooses three random signals and draws their correlograms*)
let draw_correlograms (s: string) = 
  let s1 = A.choose_rand (M.all_data s) in 
  let s2 = A.choose_rand (M.all_data s) in 
  let s3 = A.choose_rand (M.all_data s) in
  let stencil = M.to_stencil 5 in
  let s1' = A.convolution stencil s1 in
  let s2' = A.convolution stencil s2 in
  let s3' = A.convolution stencil s3 in

  Io.write_strs_to_file (List.map Io.signal_to_string [s1;s1';s2;s2';s3;s3']) s;
  print_endline "File written"

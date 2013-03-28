
(**This is the file that processes all of the harr_transform functions
Because these are a critical part of this project*)

(*Do we need to dictate *)
module P = Data_parser.DataParser  
module A = Analysis
module M = Mean_filter


let all_data (s:string) = P.parse s 

(*test function to cut the signal in half to see how this affects *)

(*computes the harr coefficients of a given signal *)
let harr (p:P.signal) : P.signal = 
  A.harr_transform p 

(*------Now we use the Mean Filter to smooth------*)
let smoother_harr (p: P.signal) : P.signal = 
  let x = M.to_stencil 3 in
  (*Applying our mean filter to the *)
  let c = A.convolution x p in
  harr c 

(**Chooses 3 signals from all the data,
and then compare regular harr to using the difference between 
a regular harr transform and one with a filter*)
let harr_vs_filter (s:string) = 
  let x = all_data s in 
  (**We need to change our parse function so that we do not have empty
   random instances*) 
  let c1 = A.choose_rand (x) in
  let c2 = A.choose_rand (x) in 
  let c3 = A.choose_rand (x) in
  print_endline "we have chosen our random signals";
  let c1' = harr c1 in
  let c2' = harr c2 in 
  let c3' = harr c3 in 
  print_endline "conducted harr transforms";
  let c1'' = smoother_harr c1 in 
  let c2'' = smoother_harr c2 in 
  let c3'' = smoother_harr c3 in 
  print_endline "smoother harr conducted";
  Io.write_strs_to_file (List.map Io.signal_to_string [c1';c1'';c2';c2'';c3';c3'']) "harr_vs_filter.txt";
  print_endline "File written!"

(**This allows us to check 
how our inverse harr allows us to output the
signal exactly as it is.*)
let check_harr_inv (s:string) = 
  let x = all_data s in
  let c1 = A.choose_rand (x) in 
  let c2 = A.choose_rand (x) in 

  let c1' = A.inv_harr (harr c1) in 
  let c2' = A.inv_harr (harr c2) in 
  Io.write_strs_to_file (List.map Io.signal_to_string [c1;c1';c2;c2']) "harr_valid.txt";
  print_endline "File written!"


(**This is the file that processes all of the harr_transform functions
Because these are a critical part of this project*)

(*Do we need to dictate *)
module P = Data_parser.DataParser  
module A = Analysis
module M = Mean_filter


let all_data (s:string) = P.parse s 

(*Calculates the difference score between the two waves
s1 is the set wave, we are calculating s2's deviation from it*)
let diff_score (s1:P.signal) (s2: P.signal) = 
  let f a b c = 
    a +. abs_float (b -. c)
  in List.fold_left2 0. s1 s2

(*computes the harr coefficients of a given signal *)
let harr (p:P.signal) : P.signal = 
  let x = A.reduce_2n p in 
  A.harr_trans x 

(*Don't know if I want this to be a unit function*)
let random_harr () : P.signal = 
  (*Pick one random signal from all data*)
  (*Reduce the chosen signal into a power of 2*)
  let red = A.reduce_2n (A.choose_rand all_data) in 
  let h_f = A.harr_trans red in 
  A.inv_harr h_f 

(*calculates the difference score between a harr_transform and 
its inverse Harr transform*)
(*let compare_harr (p: P.signal) : float =
  let red = A.reduce_2n p in 
  let h_f = harr red in 
  let h_i = A.inv_harr h_f in 
  diff_score h_f h_i *)

(*------Now we use the Mean Filter to smooth------*)
let smoother_harr (p: P.signal) : P.signal = 
  let x = M.to_stencil 3 in
  (*Applying our mean filter to the *)
  let c = A.convolution x p in
  harr c 

(** Now that we have this, we are able to make a run function
	which will take in one of these other functions and output it to 
	a text file. This will showcase our adaptability to 
	get the different results that we need from one set of functions.
*)

(*Chooses 3 signals from all the data,
and then compare regular harr to using the difference between *)
let harr_vs_filter (s:string) = 
  let c1 = A.choose_rand (all_data s) in
  let c2 = A.choose_rand (all_data s) in 
  let c3 = A.choose_rand (all_data s) in
  let c1' = harr c1 in
  let c2' = harr c2 in 
  let c3' = harr c3 in 
  let c1'' = smoother_harr c1 in 
  let c2'' = smoother_harr c2 in 
  let c3'' = smoother_harr c3 in 
  Io.write_strs_to_file (List.map Io.signal_to_string [c1';c1'';c2';c2'';c3';c3'']) "harr_vs_filter.txt";
  print_endline "File written!"

(**This allows us to check 
how our inverse harr allows us to output the
signal exactly as it is.*)
let check_harr_inv (s:string) = 
  let c1 = A.choose_rand (all_data s) in 
  let c2 = A.choose_rand (all_data s) in 

  let c1' = A.inv_harr (harr c1) in 
  let c2' = A.inv_harr (harr c2) in 
  Io.write_strs_to_file (List.map Io.signal_to_string [c1;c1';c2;c2']) "harr_valid.txt";
  print_endline "File written!"








  

  





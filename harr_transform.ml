
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

(*computes the harr coefficients of a given *)
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
let compare_harr (p: P.signal) : float =
  let red = A.reduce_2n p in 
  let h_f = A.harr_trans red in 
  let h_i = A.inv_harr h_f in 
  diff_score h_f h_i

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




  

  





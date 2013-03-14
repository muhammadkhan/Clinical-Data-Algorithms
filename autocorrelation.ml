(**This is the module to handle the calculations of the 
autocorrelation function *)

module P = Data_parser.Parser
module A = Analysis
module M = Mean_filter

let all_data (s:string) = P.parse s 

(*I have no idea how t is determined here*)
let autocorrelate (p: P.signal) (t: int) = 
  A.autocorrelation p t 

(*Chooses three random signals and draws their correlograms*)
let draw_correlograms (s: string) = 
  let s1 = A.choose_rand (all_data s) in 
  let s2 = A.choose_rand (all_data s) in 
  let s3 = A.choose_rand (all_data s) in
  
  (*There is no function to compute correlograms for a given signal*)
  (*--FINISH MUHAMMAD*)



(*---Now we apply the mean filter to the signals to smooth them *)

let correlograms_w_filter (s:string) = 
  let s1 = A.choose_rand (all_data s) in 
  let s2 = A.choose_rand (all_data s) in 
  let s3 = A.choose_rand (all_data s) in 

  (**compute the initial correlograms here too,
   might need to define another function in analysis.ml
   FINISH MUHAMMAD*)

  (*the prime ones are the smoothed signals*)
  let m = M.to_stencil 3 in
  let s1' = A.convolution m s1 in
  let s2' = A.convolution m s2 in
  let s3' = A.convolution m s3 in 

  (**Compute the correlograms again here
  and then use your Io.write_strs_to_file function
  FINISH MUHAMMAD*)



	
    


  
module P = Parser

(*These are the analysis tools to monitor the data input*)
(*Ideally the type should be of signal*)

(*Should really see if we are able to do this as a list folding *)
let zero_cross (lst:float list) (v:float) = 
  let compare a b = 
    if (a < v && b > v) || (a > v && b < v) then true
    else false
  in   	
  let index = ref 0 in 
  let rec helper ls acc = 
    match ls with 
    |[] -> List.rev acc
    |h::[] -> List.rev acc
    |h1::h2::t -> begin 
      if compare h1 h2 then begin
        let j = !index in 
        index := !index + 1;
        helper (h2::t) (j::acc)
        end  
      else begin
        index := !index + 1;
        helper (h2::t) acc
        end
      end
  in
  helper lst []

  (**Takes in two vectors, which are float lists and outputs
  the convolution of the two *)
  let convolution f g =
    (*The total number of elements in the result set*)
    let n_m = (List.length f) + (List.length g) - 1 in 






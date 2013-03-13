(*module P = Parser*)

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

  (**I believe that this is 
  the mean filter, but we need*)
  let rec convolve_with_stencil orig k =
    let rec stencil n =
      match n with
      | 0 -> []
      | n -> if n <= (List.length orig) - k then 0.::stencil(n-1) else (1. /. (float_of_int k))::stencil(n-1)
    in
    let dot_p = List.fold_left2 (fun r x1 x2 -> r +. x1 *. x2) 0. in
    if List.length orig < k then []
    else
      let (_,t) = match orig with [] -> failwith "no" | h::t -> h,t in
      (dot_p orig (stencil (List.length orig)))::(convolve_with_stencil t k)
  
  (*the pairwise difference function *)
  let pairwise_diff (lst:float list) = 
    let rec p_d_help (ls: float list) (acc : float list) : float list =
      let diff a b = (a -. b) /. 2.0 in
      match ls with 
      |[] -> List.rev acc
      |h1::h2::tl -> begin
        let i = diff h1 h2 in 
        p_d_help tl (i::acc)
      end
      |h::[] -> List.rev acc
    in
    p_d_help lst []  
   
  (*the pairwise average function *) 
  let pairwise_avg (lst: float list) =  
    let rec p_a_help (ls: float list) (acc: float list) : float list = 
      let average a b = (a +. b) /. 2.0 in 
      match ls with
      |[] -> List.rev acc 
      |h1::h2::tl -> begin
        let i = average h1 h2 in 
        p_a_help tl (i::acc)
      end
      |h::[] -> List.rev acc
    in
    p_a_help lst []

  let append a b = 
    List.rev_append (List. rev a) b 

  let compute (l:float list) = 
    let i = pairwise_avg l in
    let j = pairwise_diff l in 
    append i j 

  (*Forward harr_transform*)
  let harr_transform (i: float list) = 
    (*we need to establish a base case accurately*)
    let rec helper l =
      if List.length l = 1 then l 
      else begin
        let p_a = pairwise_avg l in
        let p_d = pairwise_diff l in 
        append (helper p_a) p_d  
      end
    in helper i  

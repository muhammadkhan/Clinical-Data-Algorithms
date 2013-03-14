module P = Data_parser.DataParser

(*These are the analysis tools to monitor the data input*)
(*Ideally the type should be of signal*)

(*Chooses a random *)
let choose_rand (d:float list array) : float list = 
  let x = Random.int 40 in 
  Array.get d x 

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
    let n = List.length f in let m = List.length g in
    let rec downto0 x = if x = 0 then [0] else x::downto0(x-1) in
    let func r elem =
      let rec sum k =
	     match k with
	     | (-1) -> 0.
	     | k -> try
		 (List.nth f k) *. (List.nth g (elem - k)) +. sum (k-1)
      with _ -> sum(k-1)
      in
      sum(elem)::r
    in
    List.fold_left func [] (downto0 (n + m - 2))

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
   
  let first_n n l =  
    let rec f_n_help n acc lst = 
      if n = 0 then ((List.rev acc), lst) 
      else 
        match lst with 
        (*We might have to failwith in this case*)
        [] -> failwith "hello" 
        |hd::tl -> f_n_help (n-1) (hd::acc) tl
    in
    f_n_help n [] l    

  (*reduces a list to its greatest power of two*)
  let reduce_2n (l: float list) = 
    let rec n2_help acc lst =
      let a = List.length acc in 
      if List.length lst < a then acc
      else begin
        let (x,t) = first_n a lst in 
        n2_help (append acc x) t
      end
    in
    match l with 
    [] -> []
    |h::t -> n2_help [h] t   

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

  (**inverse Harr transform
  given a Harr transform, we should be able to 
  get the original signal from it *)
  let inv_harr (i:float list) = 
    let compute (l1: float list) (l2: float list) = 
      let gen_range a b = [(a +. b);(a -. b)] in
      (*We can use List.fold_left2*)
      let f a b c = 
        let x = gen_range b c in 
        append a x 
      in List.fold_left2 f [] l1 l2
    in 
    (*Returns the first n elements of the list and the tail*)  
    let rec inv_help acc (lst: float list) = 
      if lst = [] then acc 
      else begin 
        let l = List.length acc in 
        let (h, t) = first_n l lst in
        inv_help (compute acc h) t
      end
    in
    match i with 
    |[] -> []
    |h::t -> inv_help [h] t

 let autocorrelation lst t =
    let toEnd lst' =
      let f (x, acc) elem =
	      if x > t then (x+1,acc)
	      else (x+1, elem::acc)
      in
    List.rev(snd(List.fold_left f ((-2),[]) lst'))
    in
    let x1 = toEnd lst in
    let x2 = List.rev(toEnd (List.rev lst)) in
    let correlation x y =
      let lx = float_of_int(List.length x) in
      let ly = float_of_int(List.length y) in
      if lx <> ly then failwith "lists must be of equal length"
      else
	let ux = List.fold_left (+.) 0. (List.map (fun c -> c /. lx) x) in
	let uy = List.fold_left (+.) 0. (List.map (fun c -> c /. ly) y) in
	let diff_x = List.map (fun c -> c -. ux) x in
	let diff_y = List.map (fun c -> c -. uy) y in
	let sum2 = List.fold_left2 (fun r elem1 elem2 -> r +. elem1 *. elem2) 0. in
	let numerator = sum2 diff_x diff_y in
	let denominator = sqrt(sum2 diff_x diff_x) *. sqrt(sum2 diff_y diff_y) in
	numerator /. denominator
    in
    correlation x1 x2

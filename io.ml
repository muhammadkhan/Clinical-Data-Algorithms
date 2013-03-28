type signal = float list
type data = signal array
type reader = {read_next : unit->string option}

let make_reader fname : reader =
  let in_channel = open_in fname in
  let closed = ref false in
  let read_next_line () =
    if !closed then None
    else
      try
	Some (Scanf.fscanf in_channel "%[^\r\n]\n" (fun x->x))
      with
	End_of_file ->
	  let _ = close_in_noerr in_channel in
	  closed := true;
	  None
  in
  {read_next = read_next_line}

let signal_to_string s =
  let str_lst = List.map (fun fl -> string_of_float fl) s in
  let f r elem =
    if String.length r = 0 then elem
    else r ^ (" " ^ elem)
  in
  List.fold_left f "" str_lst

let write_strs_to_file strs filename =
  let oc = open_out filename in
  List.iter (Printf.fprintf oc "%s\n") strs;
  close_out oc

let str_to_float_lst str : signal =
  let rec s_help s a =
    (*let s = String.trim (String.sub s 0 ((String.length s) / 2)) in*)
    let s = String.trim s in 
    if s = "" then List.rev a
    else if String.length s < 8 then
      match s.[0] with
      | '-' -> let i = float_of_string(String.sub s 0 6) in s_help "" (i::a)
      | _ -> let i = float_of_string(String.sub s 0 5) in s_help "" (i::a)
    else
      match s.[0] with
      | '-' -> let tl = String.sub s 7 (String.length s - 7) in
	       let i = float_of_string(String.sub s 0 6) in s_help tl (i::a)
      | _ -> let i = float_of_string (String.sub s 0 5) in
	     let tl = String.sub s 6 (String.length s - 6) in s_help tl (i::a)
  in
  s_help str []

let parse fname : data =
  let r = make_reader fname in
  let acc = ref [] in
  let stopper = ref true in
  while !stopper do
    match (r.read_next ()) with
    |Some(s) -> (let i = str_to_float_lst s in
		let n = Array.make 1 [] in
		Array.set n 0 i;
		acc := n::(!acc);)
    |None -> (stopper := false;)
  done;
  Array.concat (List.rev (!acc))

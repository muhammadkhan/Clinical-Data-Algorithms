(*This is the module to handle all of the IO for this CS 5540 project *)

  type signal = float list
  type data = signal array
  type reader = {read_next : unit -> string option}
  
  let make_reader (file_name : string):reader = 
    let in_channel = open_in file_name in 
    let closed = ref false in 
    let read_next_line = fun () ->
      if !closed then
        None
      else
        try
          Some (Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> x))
        with
          End_of_file ->
            let _ = close_in_noerr in_channel in
            let _ = closed := true in
            None in
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
  
  (**takes a string of the format that we know
  and convert it to a list of floats *)
  let str_to_float_lst (str:string) : signal =
    let rec s_help (s:string) (a : signal) =
      let s = String.trim s in 
      if s = "" then List.rev a
      else if String.length s < 8 then 
        match s.[0] with
        | '-' -> begin
          let i = float_of_string (String.sub s 0 6) in 
          s_help "" (i::a)
        end
        | _ -> begin
          let i = float_of_string (String.sub s 0 5) in
          s_help "" (i::a)
        end
      else 
        match s.[0] with 
        |'-' -> begin
          let tl = String.sub s 7 (String.length s -7) in
          let i = float_of_string (String.sub s 0 6) in 
          s_help tl (i::a)
        end
        | _ -> begin
          let i = float_of_string (String.sub s 0 5) in
          let tl = String.sub s 6 (String.length s -6) in
          s_help tl (i::a)
        end  
    in
    s_help str []

    (*We will actually parse a file here *)
    let parse (file_name : string) : data =
      let r =  make_reader file_name in
      (*We are able to make a list of one dim arrays*)
      let acc = ref [] in 
      (*let index = ref 0 in *)
      let stopper = ref true in 
      while(!stopper <> false) do 
        match r.read_next () with
        |Some s -> begin
          let (i:signal) = str_to_float_lst s in 
          let n = Array.make 1 [] in
          Array.set n 0 i;
          (*index := !index + 1; *)
          acc := n::(!acc)
          end
        |None ->
        stopper := false;  
      done;
      Array.concat (List.rev !acc)













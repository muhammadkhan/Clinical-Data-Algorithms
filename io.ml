(*This is the module to handle all of the IO for this CS 5540 project *)

module IO =
  struct
  type reader = {read_next : unit -> string option}
  
  let make_reader (file_name : string) = 
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
  
  (**takes a string of the format that we know
  and convert it to a list of floats *)
  let str_to_float_lst (str:string) : float list =
    let rec s_help (s:string) (a : float list) =
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
  end;;

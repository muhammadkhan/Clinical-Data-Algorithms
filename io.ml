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
    let neg st = match st.[0] with |'-' -> true |_ -> false 
    in 
    let rec s_help (s:string) (a : float list) = 
      match String.length s with 
      | 0 -> List.rev a
      | 5 | 6 -> 
        if neg s then begin
          let i = float_of_string (String.sub s 0 6) in
          s_help "" (i::a)
        end else begin
          let i = float_of_string(String.sub s 0 5) in
          s_help "" (i::a)
        end
      | _ -> 
        if neg s then begin
          let tl = String.sub s 8 (String.length s -1) in
          let i = float_of_string (String.sub s 0 6) in 
          s_help tl (i::a)
        end else begin
          let tl = String.sub s 7 (String.length s -1) in 
          let i = float_of_string (String.sub s 0 5) in
          s_help tl (i::a)
        end 
    in
    s_help str []      


  end;;

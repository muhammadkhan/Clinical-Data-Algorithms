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

  (**There is a currently an 
  issue in the parsing of negative*)
  (*print_endline s; 
      print_int (String.length s);
      match String.length s with 
      | 0 -> List.rev a
      | 5 | 6 -> 
        if (s.[0] = '-') then begin
          print_endline "entered first first case";
          let i = float_of_string (String.sub s 0 6) in
          s_help "" (i::a)
        end else begin
          print_endline "entered second second case";
          let i = float_of_string(String.sub s 0 5) in
          s_help "" (i::a)
        end
      | _ -> 
        if (s.[0] = '-') then begin
          let tl = String.sub s 8 (String.length s -1) in
          let i = float_of_string (String.sub s 0 6) in 
          s_help tl (i::a)
        end else begin
          let tl = String.sub s 7 (String.length s -1) in 
          let i = float_of_string (String.sub s 0 5) in
          s_help tl (i::a)
        end 
    in
    s_help str []*)
  (*let getFirst (str:string): char =
    str.[0]*)

  (*Does not work for negatives yet *)
  let str_to_float_lst (str:string) : float list =
    let rec s_help (s:string) (a : float list) =
      if s = "" then List.rev a
      else if String.length s < 8 then 
        match s.[0] with
        | '-' -> begin
          print_endline "first first case";
          let i = float_of_string (String.sub s 0 6) in 
          s_help "" (i::a)
        end
        | _ -> begin
          print_endline "first second case";
          let i = float_of_string (String.sub s 0 5) in
          s_help "" (i::a)
        end
      else 
        match s.[0] with 
        |'-' -> begin
          print_endline "second first case";
          let tl = String.sub s 7 (String.length s -7) in
          let i = float_of_string (String.sub s 0 6) in 
          s_help tl (i::a)
        end
        | _ -> begin
          print_endline "second second case";
          let i = float_of_string (String.sub s 0 5) in
          (*print_endline (string_of_float i);*)
          (*Don't forget that the last argument is the length*)
          let tl = String.sub s 6 (String.length s -6) in
          (*print_endline tl;*)
          s_help tl (i::a)
        end  
    in
    s_help str []     
  (*We can trim to avoid whitespace*) 
  end;;

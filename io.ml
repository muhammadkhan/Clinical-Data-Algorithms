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
  end;;

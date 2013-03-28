module DataParser = struct
  module I = Io
  type signal = float list
  type data = signal array
  
  (*We need to change this in order to make all of the entries*)
  let parse fname : data =
    let r : I.reader = I.make_reader fname in
    let curr = ref [] in 

    let index = ref 0 in
    let stopper = ref true in
    while !stopper do
      match (r.I.read_next()) with
      |Some(s) ->  begin
       let i = List.tl(I.str_to_float_lst s) in
       curr := i::(!curr);
		   print_endline("Read index " ^ (string_of_int !index));
		   incr(index);
       end
      |None -> (stopper := false;)
    done;
    Array.of_list (!curr)

end

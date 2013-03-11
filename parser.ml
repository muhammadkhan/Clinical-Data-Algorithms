
(**This is the actual parser module,
	which actually takes in the IO module as a functor.
	In our parse method, we have the IO module read, 
	but every line we parse into our different data types. 
*)
module Parser = 
  functor (I: IO) -> 
    struct
    type signal = float list
    type data  = signal array

    (*We will actually parse a file here *)
    let parse (file_name : string) : data =
      let r =  I.make_reader file_name in
      (*We always initialize this to 40*) 
      let arr = Array.make 40 [] in 
      let index = ref 0 in 
      let stopper = ref true in 
      while(!stopper = true) do 
        match r.read_next () with
        |Some s -> begin
          let (i:signal) = I.str_to_float_lst s in 
          Array.set arr (!index) i; 
          end
        |None -> stopper := false;  
      done;
      arr
      
    end 

















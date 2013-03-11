#!/bin/bash
#Builds CS 5540 Project 2

ocaml -c io.mli io.ml parser.mli parser.ml
if [$? -ne 0]; then
  echo 'Something here does not compile'
else 
  ocamlmktop -o toplevel.exe io.cmo parser.cmo
  if [$? -ne 0]; then
    echo 'Something here does not compile'
  else  
  	echo 'Nice job, everything compiles!'
  fi	
fi  
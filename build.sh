#!/bin/bash
#Builds CS 5540 Project 2

#I still need to see if this works and creates an appropriate toplevel. 
ocamlc -c io.ml data_parser.ml analysis.ml mean_filter.ml autocorrelation.ml harr_transform.ml
if [$? -ne 0]; then
  echo 'Something here does not compile'
else 
  ocamlmktop -o toplevel.exe io.cmo data_parser.cmo analysis.cmo mean_filter.cmo autocorrelation.cmo harr_transform.cmo
  if [$? -ne 0]; then
    echo 'Something here does not compile'
  else  
  	echo 'Nice job, everything compiles!'
  fi	
fi  

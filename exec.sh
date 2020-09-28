#!/bin/sh
## exec.sh
#python main.py "$@" > log.txt

# -----------------------------------------------------------------------------------------------------
## The below is the logic to caputre error messages coming from the t_error() method in scan.py
## as that output is not written to the file. THe below piece of code tracks error messages,
## puts them in a file log.txt (if any), appends those error messages to the corresponding filename.out 
## file and then deletes the log.txt file for the next run of the program.
# -----------------------------------------------------------------------------------------------------
# TO WRITE OUTPUT TO THE FILE:
#if grep -qF "***" log.txt;then
#   filename=$(basename "$@")
#   fname="${filename%.*}"
#   cat log.txt >> $fname.out
#   rm log.txt
#else
#   rm log.txt
#fi

# TO WRITE TO STANDARD OUTPUT STREAM
python3 SemanticAnalyser.py "$@"
#!/bin/bash


ABC_FILES=$(find -name "*.abc" )


output_abc ()
{
  for file in $ABC_FILES
  do
    abcm2ps -O ${file%*.abc}.ps $file
  done
}


clean_ps ()
{
  rm *.ps
}

# main...

if [ "$1" = "clean" ]
# Test if command line argument present (non-empty).
then
 clean_ps
else
 output_abc
fi


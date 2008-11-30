#!/bin/bash


LY_FILES=$(find -name "*.ly" )


output_ly ()
{
  for file in $LY_FILES
  do
    lilypond $file
  done
}


clean_ps ()
{
  rm *.ps; rm *.pdf
}

# main...

if [ "$1" = "clean" ]
# Test if command line argument present (non-empty).
then
 clean_ps
else
 output_ly
fi


#!/bin/bash

infiles=$(find ./LAI -type f -name "*quality_*")
#echo $infiles

for infile in $infiles; do
  outfilename=$(sed "s+quality_++" <<< $(basename $infile))
  outfile=$(dirname $infile)/$outfilename
  
  echo "$infile -> $outfile"
  mv $infile $outfile
done

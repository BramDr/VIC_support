#!/bin/bash

files=$(find ./ -type f -name "*Dhari_5min*")

for file in $files; do
  outfile=$(sed "s+Dhari_5min+Dahri_5min+" <<< $(basename $file))
  
  echo $file $outfile
  mv $file $outfile
done

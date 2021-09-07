#!/bin/bash

in_dir="./Saves/Combined_6hourly/"

in_files=$(find $in_dir -type f -name "*.nc")

for in_file in $in_files; do
  out_file=$(sed "s+_6hourly+_6hourly_ERA5+g" <<< $in_file)
  
  echo "$in_file -> $out_file"
  
  mkdir -p $(dirname $out_file)
  mv $in_file $out_file
done

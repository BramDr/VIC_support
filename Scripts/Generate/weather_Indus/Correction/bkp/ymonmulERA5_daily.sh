#!/bin/bash

pr_dir="../Combined_daily/pr_daily_ERA5/"
tas_dir="../Combined_daily/tas_daily_ERA5/"
pr_cor_file="./Saves/ERA5_daily/pr_correction_daily_ERA5.nc"
tas_cor_file="./Saves/ERA5_daily/tas_correction_daily_ERA5.nc"
out_dir="./Out/ERA5_daily/"

pr_files=$(find $pr_dir -type f -name "*.nc")
tas_files=$(find $tas_dir -type f -name "*.nc")

for pr_file in $pr_files; do
  pr_out_file="$out_dir/pr_daily_ERA5/$(basename $pr_file)"
  pr_out_file=$(sed "s+ERA5+ERA5adj+g" <<< $pr_out_file)
  
  echo "$pr_file -> $pr_out_file"
  
  mkdir -p $(dirname $pr_out_file)
  cdo -z zip_2 --no_history ymonmul $pr_file -enlarge,$pr_file $pr_cor_file $pr_out_file
done

for tas_file in $tas_files; do
  tas_out_file="$out_dir/tas_daily_ERA5/$(basename $tas_file)"
  tas_out_file=$(sed "s+ERA5+ERA5adj+g" <<< $tas_out_file)
  
  echo "$tas_file -> $tas_out_file"
  
  mkdir -p $(dirname $tas_out_file)
  cdo -z zip_2 --no_history ymonadd $tas_file -enlarge,$tas_file $tas_cor_file $tas_out_file
done

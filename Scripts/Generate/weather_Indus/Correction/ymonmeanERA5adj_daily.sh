#!/bin/bash

force_dirs="./Out/ERA5_daily/pr_daily_ERA5adj/ ./Out/ERA5_daily/tas_daily_ERA5adj/ ../Combined_daily/lwdown_daily_ERA5/ ../Combined_daily/swdown_daily_ERA5/ ../Converted_daily/vp_daily_ERA5/ ../Combined_daily/psurf_daily_ERA5/ ../Converted_daily/wind10_daily_ERA5/"
force_dirs="./Out/ERA5_daily/pr_daily_ERA5adj/ ./Out/ERA5_daily/tas_daily_ERA5adj/ ../Combined_daily/lwdown_daily_ERA5/ ../Combined_daily/swdown_daily_ERA5/"
out_dir="./Saves/ERA5adj_daily/"
years=$(seq 1979 2014)

mkdir -p $out_dir
mkdir -p $out_dir/monthly

for force_dir in $force_dirs; do
  force_files=""
  var_name=$(sed "s+_.*++g" <<< $(basename $force_dir))
  
  for year in $years; do
    force_file=$(find $force_dir -type f -name "*$year.nc")
    
    force_month_name=$(sed "s+daily+monthly+" <<< $(basename $force_file))
    force_month_file="$out_dir/monthly/$force_month_name"
    if [ ! -f $force_month_file ]; then
      cdo monmean $force_file $force_month_file
    fi
    
    force_files="$force_files $force_month_file"
  done
  
  force_name_mergetime=$(sed "s+$year+mergetime+" <<< $(basename $force_month_file))
  force_file_mergetime="$out_dir/$force_name_mergetime"
  cdo mergetime $force_files $force_file_mergetime

  force_name_ymonmean=$(sed "s+$year+ymonmean+" <<< $(basename $force_month_file))
  force_file_ymonmean="$out_dir/$force_name_ymonmean"
  cdo ymonmean $force_file_mergetime $force_file_ymonmean
  
  if [ "$var_name" == "pr" ]; then
    force_name_ymonsum=$(sed "s+$year+ymonsum+" <<< $(basename $force_month_file))
    force_file_ymonsum="$out_dir/$force_name_ymonsum"
    cdo muldpm $force_file_ymonmean $force_file_ymonsum
  fi
done

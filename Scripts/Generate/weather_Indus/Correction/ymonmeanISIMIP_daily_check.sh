#!/bin/bash

sub_dirs="GFDL-ESM4 IPSL-CM6A-LR MPI-ESM1-2-HR MRI-ESM2-0 UKESM1-0-LL"
#sub_dirs="GFDL-ESM4"
out_dir="./Check/"
years=$(seq 1979 2014)

for sub_dir in $sub_dirs; do
  mkdir -p "$out_dir/$sub_dir/"
  mkdir -p "$out_dir/$sub_dir/monthly"
  
  force_dirs="./Out/$sub_dir/pr_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/tas_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/lwdown_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/swdown_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/vp_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/psurf_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/wind10_daily_"$sub_dir"adj_historical/"
  force_dirs="./Out/$sub_dir/pr_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/tas_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/lwdown_daily_"$sub_dir"adj_historical/ ./Out/$sub_dir/swdown_daily_"$sub_dir"adj_historical/"
  
  for force_dir in $force_dirs; do
    echo $force_dir
    
    force_files=""
    var_name=$(sed "s+_.*++g" <<< $(basename $force_dir))
    
    for year in $years; do
      force_file=$(find $force_dir -type f -name "*$year.nc")
      
      force_month_name=$(sed "s+daily+monthly+" <<< $(basename $force_file))
      force_month_file="$out_dir/$sub_dir/monthly/$force_month_name"
      if [ ! -f $force_month_file ]; then
        cdo monmean $force_file $force_month_file
      fi
      
      force_files="$force_files $force_month_file"
    done
  
    force_name_mergetime=$(sed "s+$year+mergetime+" <<< $(basename $force_month_file))
    force_file_mergetime="$out_dir/$sub_dir/$force_name_mergetime"
    cdo mergetime $force_files $force_file_mergetime

    force_name_ymonmean=$(sed "s+$year+ymonmean+" <<< $(basename $force_month_file))
    force_file_ymonmean="$out_dir/$sub_dir/$force_name_ymonmean"
    cdo ymonmean $force_file_mergetime $force_file_ymonmean
    
    if [ "$var_name" == "pr" ]; then
      force_name_ymonsum=$(sed "s+$year+ymonsum+" <<< $(basename $force_month_file))
      force_file_ymonsum="$out_dir/$sub_dir/$force_name_ymonsum"
      cdo muldpm $force_file_ymonmean $force_file_ymonsum
    fi
  done
done

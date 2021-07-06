#!/bin/bash

sub_dirs="GFDL-ESM4_historical IPSL-CM6A-LR_historical MPI-ESM1-2-HR_historical MRI-ESM2-0_historical UKESM1-0-LL_historical GFDL-ESM4_ssp126 IPSL-CM6A-LR_ssp126 MPI-ESM1-2-HR_ssp126 MRI-ESM2-0_ssp126 UKESM1-0-LL_ssp126 GFDL-ESM4_ssp370 IPSL-CM6A-LR_ssp370 MPI-ESM1-2-HR_ssp370 MRI-ESM2-0_ssp370 UKESM1-0-LL_ssp370 GFDL-ESM4_ssp585 IPSL-CM6A-LR_ssp585 MPI-ESM1-2-HR_ssp585 MRI-ESM2-0_ssp585 UKESM1-0-LL_ssp585"
out_dir="./Out/"

for sub_dir in $sub_dirs; do
  pr_dir="../Disaggregated_daily/pr_daily_$sub_dir/"
  tas_dir="../Disaggregated_daily/tas_daily_$sub_dir/"
  
  pr_cor_file="./Saves/$sub_dir/pr_correction_daily_$sub_dir.nc"
  tas_cor_file="./Saves/$sub_dir/tas_correction_daily_$sub_dir.nc"
  pr_cor_file=$(sed "s+_historical++g" <<< $pr_cor_file)
  pr_cor_file=$(sed "s+_ssp126++g" <<< $pr_cor_file)
  pr_cor_file=$(sed "s+_ssp370++g" <<< $pr_cor_file)
  pr_cor_file=$(sed "s+_ssp585++g" <<< $pr_cor_file)
  tas_cor_file=$(sed "s+_historical++g" <<< $tas_cor_file)
  tas_cor_file=$(sed "s+_ssp126++g" <<< $tas_cor_file)
  tas_cor_file=$(sed "s+_ssp370++g" <<< $tas_cor_file)
  tas_cor_file=$(sed "s+_ssp585++g" <<< $tas_cor_file)
  
  model=$(sed "s+_.*++" <<< $sub_dir)
  
  pr_files=$(find $pr_dir -type f -name "*.nc")
  tas_files=$(find $tas_dir -type f -name "*.nc")

  for pr_file in $pr_files; do
    pr_out_file="$out_dir/pr_daily_$sub_dir/$(basename $pr_file)"
    pr_out_file=$(sed "s+$model+$model""adj+g" <<< $pr_out_file)
    
    if [ -e $pr_out_file ]; then
      continue
    fi
    
    echo "$pr_file -> $pr_out_file"
    
    mkdir -p $(dirname $pr_out_file)
    cdo -z zip_2 --no_history ymonmul $pr_file -enlarge,$pr_file $pr_cor_file $pr_out_file
  done

  for tas_file in $tas_files; do
    tas_out_file="$out_dir/tas_daily_$sub_dir/$(basename $tas_file)"
    tas_out_file=$(sed "s+$model+$model""adj+g" <<< $tas_out_file)
    
    if [ -e $tas_out_file ]; then
      continue
    fi
    
    echo "$tas_file -> $tas_out_file"
    
    mkdir -p $(dirname $tas_out_file)
    cdo -z zip_2 --no_history ymonadd $tas_file -enlarge,$tas_file $tas_cor_file $tas_out_file
  done
done

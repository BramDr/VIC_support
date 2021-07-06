#!/bin/bash

sub_dirs="GFDL-ESM4_historical IPSL-CM6A-LR_historical MPI-ESM1-2-HR_historical MRI-ESM2-0_historical UKESM1-0-LL_historical"
out_dir="./Check/"
pr_years=$(seq 1999 2011)
tas_years=$(seq 1999 2011)

mkdir -p $out_dir

for sub_dir in $sub_dirs; do
  pr_dir="../Corrected_daily/pr_daily_$sub_dir/"
  tas_dir="../Corrected_daily/tas_daily_$sub_dir/"
  
  model=$(sed "s+_.*++" <<< $sub_dir)
  
  pr_dir=$(sed "s+$model+$model""adj+g" <<< $pr_dir)
  tas_dir=$(sed "s+$model+$model""adj+g" <<< $tas_dir)

  pr_files=""
  for pr_year in $pr_years; do
    pr_file=$(find $pr_dir -type f -name "*$pr_year.nc")
    pr_files="$pr_files $pr_file"
  done

  tas_files=""
  for tas_year in $tas_years; do
    tas_file=$(find $tas_dir -type f -name "*$tas_year.nc")
    tas_files="$tas_files $tas_file"
  done

  pr_file_mergetime="$out_dir/$sub_dir/pr_daily_$sub_dir""_mergetime.nc"
  tas_file_mergetime="$out_dir/$sub_dir/tas_daily_$sub_dir""_mergetime.nc"
  mkdir -p $(dirname $pr_file_mergetime)
  cdo mergetime $pr_files $pr_file_mergetime
  cdo mergetime $tas_files $tas_file_mergetime

  pf_file_seldate="$out_dir/$sub_dir/pr_daily_$sub_dir""_seldate.nc"
  tas_file_seldate="$out_dir/$sub_dir/tas_daily_$sub_dir""_seldate.nc"
  cdo seldate,1999-01-02,2012-01-01 $pr_file_mergetime $pf_file_seldate
  cdo seldate,1999-01-02,2012-01-01 $tas_file_mergetime $tas_file_seldate

  pr_file_ymonmean="$out_dir/$sub_dir/pr_daily_$sub_dir""_ymonmean.nc"
  tas_file_ymonmean="$out_dir/$sub_dir/tas_daily_$sub_dir""_ymonmean.nc"
  cdo ymonmean $pf_file_seldate $pr_file_ymonmean
  cdo ymonmean $tas_file_seldate $tas_file_ymonmean

  pr_file_ymonsum="$out_dir/$sub_dir/pr_daily_$sub_dir""_ymonsum.nc"
  cdo muldpm $pr_file_ymonmean $pr_file_ymonsum
done

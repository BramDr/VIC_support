#!/bin/bash

pr_dir="./Out/ERA5adj_daily/pr_daily_ERA5adj/"
tas_dir="./Out/ERA5adj_daily/tas_daily_ERA5adj/"
out_dir="./Check/ERA5_daily/"
pr_years=$(seq 1999 2011)
tas_years=$(seq 1999 2011)

mkdir -p $out_dir

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

pr_file_mergetime="$out_dir/pr_daily_ERA5_mergetime.nc"
tas_file_mergetime="$out_dir/tas_daily_ERA5_mergetime.nc"
cdo mergetime $pr_files $pr_file_mergetime
cdo mergetime $tas_files $tas_file_mergetime

pf_file_seldate="$out_dir/pr_daily_ERA5_seldate.nc"
tas_file_seldate="$out_dir/tas_daily_ERA5_seldate.nc"
cdo seldate,1999-01-02,2012-01-01 $pr_file_mergetime $pf_file_seldate
cdo seldate,1999-01-02,2012-01-01 $tas_file_mergetime $tas_file_seldate

pr_file_ymonmean="$out_dir/pr_daily_ERA5_ymonmean.nc"
tas_file_ymonmean="$out_dir/tas_daily_ERA5_ymonmean.nc"
cdo ymonmean $pf_file_seldate $pr_file_ymonmean
cdo ymonmean $tas_file_seldate $tas_file_ymonmean

pr_file_ymonsum="$out_dir/pr_daily_ERA5_ymonsum.nc"
cdo muldpm $pr_file_ymonmean $pr_file_ymonsum

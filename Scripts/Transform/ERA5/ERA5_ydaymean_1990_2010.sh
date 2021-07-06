#!/bin/bash
in_dir="../../../Data/VIC/Forcing/Indus_5min/tas_daily_ERA5/"
in_years=$(seq 1990 2010)
tmp_dir="./Tmp/"
out_file="../../../Data/transformed/ERA5/tas_daily_ERA5_ydaymean_1990_2010.nc"

in_files=""
for in_year in $in_years; do
  in_file=$(find $in_dir -type f -name "*$in_year.nc")
  in_files="$in_files $in_file"
done

tmp_file="$tmp_dir/$(basename $out_file)"
mkdir -p $(dirname $tmp_file)
cdo mergetime $in_files $tmp_file

mkdir -p $(dirname $out_file)
cdo ydaymean $tmp_file $out_file
rm $tmp_file

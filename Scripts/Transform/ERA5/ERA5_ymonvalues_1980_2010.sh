#!/bin/bash
in_dir="../../../Data/VIC/Forcing/Indus_5min/tas_daily_ERA5adj/"
in_years=$(seq 1980 2010)
tmp_dir="./Tmp/"
monmean_file="../../../Data/transformed/ERA5/tas_daily_ERA5adj_ymonmean_1980_2010.nc"
yearmax_file="../../../Data/transformed/ERA5/tas_daily_ERA5adj_ymonmeanmax_1980_2010.nc"
yearmin_file="../../../Data/transformed/ERA5/tas_daily_ERA5adj_ymonmeanmin_1980_2010.nc"

in_files=""
for in_year in $in_years; do
  in_file=$(find $in_dir -type f -name "*$in_year.nc")
  in_files="$in_files $in_file"
done

tmp_file="$tmp_dir/$(basename $monmean_file)"
mkdir -p $(dirname $tmp_file)
cdo mergetime $in_files $tmp_file

mkdir -p $(dirname $monmean_file)
cdo ymonmean $tmp_file $monmean_file
rm $tmp_file

mkdir -p $(dirname $yearmax_file)
cdo timmax $monmean_file $yearmax_file
mkdir -p $(dirname $yearmin_file)
cdo timmin $monmean_file $yearmin_file

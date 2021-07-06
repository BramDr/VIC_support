#!/bin/bash
in_models="GFDL-ESM4adj_historical IPSL-CM6A-LRadj_historical MPI-ESM1-2-HRadj_historical MRI-ESM2-0adj_historical UKESM1-0-LLadj_historical GFDL-ESM4adj_ssp126 IPSL-CM6A-LRadj_ssp126 MPI-ESM1-2-HRadj_ssp126 MRI-ESM2-0adj_ssp126 UKESM1-0-LLadj_ssp126 GFDL-ESM4adj_ssp370 IPSL-CM6A-LRadj_ssp370 MPI-ESM1-2-HRadj_ssp370 MRI-ESM2-0adj_ssp370 UKESM1-0-LLadj_ssp370 GFDL-ESM4adj_ssp585 IPSL-CM6A-LRadj_ssp585 MPI-ESM1-2-HRadj_ssp585 MRI-ESM2-0adj_ssp585 UKESM1-0-LLadj_ssp585"
in_years1="$(seq 1970 2000)"
in_years2="$(seq 2020 2050)"
in_years3="$(seq 2070 2100)"
tmp_dir="./Tmp/"

for in_model in $in_models; do
  in_dir="../Indus_5min/"
  
  out_file2="./Saves/tas_daily_"$in_model"_ydaymean_2020_2050.nc"
  out_file3="./Saves/tas_daily_"$in_model"_ydaymean_2070_2100.nc"
  
  in_files1=""
  for in_year in $in_years1; do
    in_file=$(find $in_dir -type f -name "*tas_daily_"$in_model"*"$in_year".nc")
    in_files1="$in_files1 $in_file"
  done
  in_files2=""
  for in_year in $in_years2; do
    in_file=$(find $in_dir -type f -name "*tas_daily_"$in_model"*"$in_year".nc")
    in_files2="$in_files2 $in_file"
  done
  in_files3=""
  for in_year in $in_years3; do
    in_file=$(find $in_dir -type f -name "*tas_daily_"$in_model"*"$in_year".nc")
    in_files3="$in_files3 $in_file"
  done
  
  if [ "$in_files1" != "" ]; then
    echo $in_model "1970_2000"
    
    out_file1="./Saves/tas_daily_"$in_model"_ydaymean_1970_2000.nc"
    tmp_file1="$tmp_dir/$(basename $out_file1)"
    
    if [ ! -f $tmp_file1 ]; then
      mkdir -p $(dirname $tmp_file1)
      cdo mergetime $in_files1 $tmp_file1
    fi
    
    mkdir -p $(dirname $out_file1)
    cdo ydaymean $tmp_file1 $out_file1
  fi
  
  if [ "$in_files2" != "" ]; then
    echo $in_model "2020_2050"
    
    out_file2="./Saves/tas_daily_"$in_model"_ydaymean_2020_2050.nc"
    tmp_file2="$tmp_dir/$(basename $out_file2)"
    
    if [ ! -f $tmp_file2 ]; then
      mkdir -p $(dirname $tmp_file2)
      cdo mergetime $in_files2 $tmp_file2
    fi
    
    mkdir -p $(dirname $out_file2)
    cdo ydaymean $tmp_file2 $out_file2
  fi
  
  if [ "$in_files3" != "" ]; then
    echo $in_model "2070_2100"
    
    out_file3="./Saves/tas_daily_"$in_model"_ydaymean_2070_2100.nc"
    tmp_file3="$tmp_dir/$(basename $out_file3)"
    
    if [ ! -f $tmp_file3 ]; then
      mkdir -p $(dirname $tmp_file3)
      cdo mergetime $in_files3 $tmp_file3
    fi
    
    mkdir -p $(dirname $out_file3)
    cdo ydaymean $tmp_file3 $out_file3
  fi
  
  rm $tmp_file1
  rm $tmp_file2
  rm $tmp_file3
done

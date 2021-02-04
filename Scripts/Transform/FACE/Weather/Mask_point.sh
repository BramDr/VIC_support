#!/bin/bash

in_dir="/lustre/backup/WUR/ESG/data/CLIMATE_DATA/WATCH/WATCH_WFDEI_GRIDDED/daily/"
out_dir="./out/"
years=$(seq 1992 1997)
lat=33.0628
lon=-111.9826

for year in $years; do
  in_files=$(find $in_dir -type f -name "*$year.nc" -and ! -name "*_CRU_*")
  
  for in_file in $in_files; do
    
    out_name=$(basename $in_file)
    out_file=$out_dir/$out_name
    
    echo "$(basename $in_file) -> $(basename $out_file)"
    
    while [ ! -e $out_file ]; do
      mkdir -p $(dirname $out_file)
      ncks -d lon,$lon,$lon -d lat,$lat,$lat $in_file -O $out_file
    done
  done
done



echo "Finished masking"

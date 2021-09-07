#!/bin/bash

indir="../../../../Papers/3_CO2_fertilization_water_food_nexus/Model/Output/naturalized/"
outdir="../../../Data/Transformed/VIC/Indus/"
infiles=$(find $indir -type f -name "*_historical_*.nc")

for infile in $infiles; do
  eyear=$(sed "s+_naturalized.*++" <<< $(basename $infile))
  eyear=$(sed "s+.*_++" <<< $eyear)
  syear=$(sed "s+_"$eyear"_naturalized.*++" <<< $(basename $infile))
  syear=$(sed "s+.*_++" <<< $syear)

  outname=$(sed "s+.[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9].nc+_ydaymean.nc+" <<< $(basename $infile))
  outfile=$outdir"/"$outname
  tmpfile=$(sed "s+.nc+.tmp.nc+" <<< $outfile)
  
  if [ -f $outfile ]; then
    continue
  fi
  
  echo "$infile -> $outfile"
  
  mkdir -p $(dirname $outfile)
  ncks -d time,"$syear-01-01","$eyear-12-31" $infile $tmpfile
  cdo ydaymean $tmpfile $outfile
  
  rm -f $tmpfile
done

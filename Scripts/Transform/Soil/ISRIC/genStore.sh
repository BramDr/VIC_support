#!/bin/bash

scriptfile="./StoreSoilTextures.R"
gendir="./Gen"
step=15
lats=$(seq -90 $step 90)
lons=$(seq -180 $step 180)

for lat in $lats; do
  for lon in $lons; do
    genscriptname=$(sed "s+.R+_$lat\\_$lon.Rgen+" <<< $(basename $scriptfile))
    genscriptfile=$gendir"/"$genscriptname
    echo $genscriptfile
    
    mkdir -p $(dirname $genscriptfile)
    cp -rf $scriptfile $genscriptfile
    
    minlat=$lat
    minlon=$lon
    maxlat=$(($lat + $step))
    maxlon=$(($lon + $step))
    
    sed -i "s+extent.isel =.*+extent.isel = c($minlon, $maxlon, $minlat, $maxlat)+" $genscriptfile
  done
done

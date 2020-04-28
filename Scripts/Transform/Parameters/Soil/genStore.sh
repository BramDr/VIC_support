#!/bin/bash

scriptfile="/home/bram/Projects/VIC-WOFOST_Soil/Store_soils.Rgen"
gendir="/home/bram/Projects/VIC-WOFOST_Soil/Gen"
step=15
lats=$(seq -90 $step 90)
lons=$(seq -180 $step 180)

for lat in $lats; do
  for lon in $lons; do
    genscriptname=$(sed "s+.R+_$lat\\_$lon.R+" <<< $(basename $scriptfile))
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

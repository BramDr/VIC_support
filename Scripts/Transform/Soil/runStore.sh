#!/bin/bash

gendir="/home/bram/Projects/VIC-WOFOST_Soil/Gen"
genfiles=$(find $gendir -type f -name "*Store_soils_*")

for genfile in $genfiles; do
  Rscript $genfile
done

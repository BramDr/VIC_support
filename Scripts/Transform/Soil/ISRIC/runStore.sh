#!/bin/bash

gendir="./Gen"
genfiles=$(find $gendir -type f -name "*StoreSoilTextures_*")

for genfile in $genfiles; do
  Rscript $genfile
done

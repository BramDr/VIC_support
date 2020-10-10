#!/bin/bash

gendir="./Gen"
genfiles=$(find $gendir -type f -name "*StoreSoilChemical_*")

for genfile in $genfiles; do
  Rscript $genfile
done

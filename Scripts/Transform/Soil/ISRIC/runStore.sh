#!/bin/bash

gendir="./Gen"
genfiles=$(find $gendir -type f -name "*StoreSoil*")

for genfile in $genfiles; do
  Rscript $genfile
done

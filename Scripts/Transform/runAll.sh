#!/bin/bash

dir="./Parameters"
files=$(find $dir -type f -name "*CreateVICParameters_*.R")

origin=$(pwd)
for file in $files; do
  cd $(dirname $file)
  echo $file
  Rscript $(basename $file)
  cd $origin
done

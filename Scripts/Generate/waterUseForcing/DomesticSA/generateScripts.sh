#!/bin/bash

script1="./saveDomesticDemandTmp_SA.R"
script2="./saveDomesticDemand_SA.R"
gendir="./Gen/"

itirations=100
for itir in $(seq 1 $itirations); do
  
  genscript1name=$(basename $script1)
  genscript1name=$(sed "s+.R+_$itir.genR+" <<< $genscript1name)
  genscript1=$gendir$genscript1name
  
  mkdir -p $(dirname $genscript1)
  cp -rf $script1 $genscript1
  
  sed -i "s+PWD_PLACEHOLDER+$(pwd)+" $genscript1
  sed -i "s+COEF_PLACEHOLDER+Saves/domesticCoef_country_$itir.csv+" $genscript1
  sed -i "s+TMP_PLACEHOLDER+Saves/domesticDemandPC_30min_global_$itir.RDS+" $genscript1
  
  genscript2name=$(basename $script2)
  genscript2name=$(sed "s+.R+_$itir.genR+" <<< $genscript2name)
  genscript2=$gendir$genscript2name
  
  mkdir -p $(dirname $genscript2)
  cp -rf $script2 $genscript2
  
  sed -i "s+PWD_PLACEHOLDER+$(pwd)+" $genscript2
  sed -i "s+TMP_PLACEHOLDER+Saves/domesticDemandPC_30min_global_$itir.RDS+" $genscript2
  sed -i "s+OUT_PLACEHOLDER+Saves/domesticDemand_30min_global_$itir.RDS+" $genscript2
  
done
#!/bin/bash

echo "starting computations"
NOW=$(date +%F)
Rscript compute_chen.R 2>&1 | tee ../logs/chen_$NOW &

NOW=$(date +%F)
Rscript compute_natsoulis.R 2>&1 | tee  ../logs/natsoulis_$NOW &

NOW=$(date +%F)
Rscript compute_rosenwald.R 2>&1 | tee ../logs/rosenwald_$NOW &

NOW=$(date +%F)
Rscript compute_yeoh.R 2>&1 | tee ../logs/yeon_$NOW &

NOW=$(date +%F)
#Rscript compute_valk.R 2>&1 | tee ../logs/valk_$NOW &

NOW=$(date +%F)
#Rscript compute_vijver.R 2>&1 | tee ../logs/vijver_$NOW &

#systemctl suspend

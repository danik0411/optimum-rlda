#!/bin/bash

echo "starting computations"
NOW=$(date +%F)
Rscript time_chen.R 2>&1 | tee ../logs/time_chen_$NOW

NOW=$(date +%F)
#Rscript compute_natsoulis.R 2>&1 | tee  ../logs/natsoulis_$NOW &

NOW=$(date +%F)
#Rscript compute_rosenwald.R 2>&1 | tee ../logs/rosenwald_$NOW &

NOW=$(date +%F)
#Rscript compute_yeoh.R 2>&1 | tee ../logs/yeon_$NOW &

NOW=$(date +%F)
#Rscript compute_valk.R 2>&1 | tee ../logs/valk_$NOW &

NOW=$(date +%F)
#Rscript compute_vijver.R 2>&1 | tee ../logs/vijver_$NOW &

cd ../../project1_june16_p50/runs/
pwd
Rscript time_chen.R 2>&1 | tee ../logs/time_chen_$NOW &


#systemctl suspend

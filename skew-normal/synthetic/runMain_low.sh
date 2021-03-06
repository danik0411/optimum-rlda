#!/bin/bash

NOW=$(date +%F)

echo "starting computations for low alpha of 2 p20"
Rscript compute_alpha2_p20_low.R 2>&1 | tee log_alpha2_p20_low_$NOW.txt &

echo "starting computations for low alpha of 4 p20"
Rscript compute_alpha4_p20_low.R 2>&1 | tee log_alpha4_p20_low_$NOW.txt &

echo "starting computations for low alpha of 2 p20"
Rscript compute_alpha2_p50_low.R 2>&1 | tee log_alpha2_p50_low_$NOW.txt &

echo "starting computations for low alpha of 4 p20"
Rscript compute_alpha4_p50_low.R 2>&1 | tee log_alpha4_p50_low_$NOW.txt &

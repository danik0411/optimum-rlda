#!/bin/bash

NOW=$(date +%F)

echo "starting computations for high alpha of 2 p20"
Rscript compute_alpha2_p20_high.R 2>&1 | tee log_alpha2_p20_high_$NOW.txt &

echo "starting computations for high alpha of 4 p20"
Rscript compute_alpha4_p20_high.R 2>&1 | tee log_alpha4_p20_high_$NOW.txt

echo "starting computations for high alpha of 2 p50"
Rscript compute_alpha2_p50_high.R 2>&1 | tee log_alpha2_p50_high_$NOW.txt &

echo "starting computations for high alpha of 4 p50"
Rscript compute_alpha4_p50_high.R 2>&1 | tee log_alpha4_p50_high_$NOW.txt

#!/bin/bash
# this code runs some files

NOW=$(date +%F)
Rscript dist9/synth_dist9_p20.R > "dist9/log_dist9_p20_$NOW" 2>&1 &

Rscript dist5/synth_dist5_p20.R > "dist5/log_dist5_p20_$NOW" 2>&1 &

Rscript dist2/synth_dist2_p20.R > "dist2/log_dist2_p20_$NOW" 2>&1 &

Rscript dist0.75/synth_dist075_p20.R > "dist0.75/log_dist075_p20_$NOW" 2>&1

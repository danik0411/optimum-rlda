# this file runs 

rm(list=ls(all=TRUE));

source("synth_applyAllEstimators.R");

qqq = new.env();
print(getwd());
PATH_PROJ=getwd();

environment( synth_applyAllEstimators ) = qqq;

# setting up the parameters
# define parameters of the run
sampleSize=seq(30,150,by=10);
sampleSize = c(sampleSize, 200, 250, 300);
p = 50;	   # 5, 20, 50, 150
mahDist = 0.75;
repetition=500;

print("total sample size is");
print(sampleSize);

gammaBase = 1000^(1/10);
gammaValues = gammaBase^(c(-10:10));

checkValue = 0; kappa = 1;

print(paste("mah distance is ", as.character(mahDist), ", feature size us " , as.character(p), sep="") );

for( k in 1:length(sampleSize) ) {

# write current sample size and apply all estimators
currentSampleSize = sampleSize[k];

# store data in tempFileName
tempPrefile = paste(PATH_PROJ, "/temp/temp_" , sep="");
tempFileName = paste( PATH_PROJ , "/dist", as.character(mahDist), "/p", as.character(p), "/synth_p", as.character(p), "_sample", as.character(currentSampleSize) , ".RData" , sep="");

cat("\n\n");
print("running the applyAllEstimators");
synth_applyAllEstimators( currentSampleSize , mahDist , p, repetition , gammaValues , checkValue , kappa , tempFileName );

} 

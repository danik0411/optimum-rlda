# this file runs 

rm(list=ls(all=TRUE));

source("synth_applyAllEstimators.R");
source("plotAllFigures.R");

qqq = new.env();
setwd("..");
PATH_PROJ=getwd();

environment( synth_applyAllEstimators ) = qqq;
environment( plotAllFigures ) = qqq;

# setting up the parameters
# define parameters of the run
sampleSize=seq(30,150,by=10);
sampleSize = c(sampleSize, 200, 250, 300);
featureSize = (5);
mahDistance = 0.75;
repetition=500;

print("total sample size is");
print(sampleSize);

gammaBase = 1000^(1/10);
gammaValues = gammaBase^(c(-10:10));

checkValue = 0; kappa = 1;

mahDist = mahDistance[i];
print(paste("running the data for mah distance of " , as.character(mahDist) , sep=""));

for( k in 1:length(sampleSize) ) {

# write current sample size and apply all estimators
currentSampleSize = sampleSize[k];

# store data in tempFileName
tempPrefile = paste(PATH_PROJ, "/temp/temp_" , sep="");
tempFileName = paste( PATH_PROJ , "/dist", as.character(mahDist), "/synth_p", as.character(featureSize), "_sample", as.character(currentSampleSize) , ".RData" , sep="");

cat("\n\n");
print("running the applyAllEstimators");
synth_applyAllEstimators( currentSampleSize , mahDist[i] , featureSize[j] , repetition , gammaValues , checkValue , kappa , tempFileName );

} 

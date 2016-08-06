# this file runs 

rm(list=ls(all=TRUE));

source("skew_applyAllEstimators.R");

qqq = new.env();

PATH_PROJ=getwd();

environment( skew_applyAllEstimators ) = qqq;

# setting up the parameters
# define parameters of the run
#sampleSize=seq(30,100,by=10);
sampleSize=c(300,250,200);
featureSize = 20;
repetition=500;

#sampleSize = seq(30,50,by=10);
#featureSize = 20;
#repetition=5;

alphav = 2; # it should be {1,2,4}

gammaBase = 100^(1/20);
gammaValues = gammaBase^(c(-10:24));

checkValue = 0; kappa = 1;

estimatorsON = c(TRUE,TRUE,TRUE,TRUE,TRUE); #double,cv,loo,plugin,true

print(paste("running the data for alpha of " , as.character(alphav) , sep=""));

for( j in 1:length(featureSize) ) {

alphavv = array(alphav, dim = featureSize[j]);	# it should be {1,2,4}

for( k in 1:length(sampleSize) ) {

# write current sample size and apply all estimators
currentSampleSize = sampleSize[k];

# store data in tempFileName
tempPrefile = paste(PATH_PROJ, "/temp/temp_" , 
				as.character(alphav) , "a_", sep="");

tempFileName = paste( tempPrefile , 
		      as.character(currentSampleSize), "s_" ,
		      as.character(featureSize[j]) , "p_" , sep="");

print("running the applyAllEstimators");
skew_applyAllEstimators( currentSampleSize , alphavv , featureSize[j] , repetition , gammaValues , checkValue , kappa , tempFileName );

} # for sampleSize

} # for featureSize

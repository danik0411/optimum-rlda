# this file is intended to run the bhatta file data
# p50 rep 500

rm(list=ls(all=TRUE));

word="natsoulis";
namesOfEstimators = c("Double" , "CV" , "LOO" , "Plugin" , "True");
namesOfVARS = c("Error" , "Time");
numberOfEstimators = length(namesOfEstimators);
numberOfVARS = length(namesOfVARS);

thisFilePath = getwd();
setwd("..");
PATH_PROJ = getwd();

source("src/applyAllEstimators.R");
source("src/plotAllFigures.R");
source("src/moveData.R");

env_allEstimators = new.env();

environment( applyAllEstimators ) = env_allEstimators;
environment( plotAllFigures ) = env_allEstimators;
environment( moveTempToFinal ) = env_allEstimators;

data_filename = "../data/natsoulis/natsoulis_p150.RData";

outputFileName = sprintf( "%s/results/%s/%sCompData.RData" , PATH_PROJ , word , word );

gammaBase = 1000^(1/10);	  gammaValues = gammaBase^(c(-10:10)); # from 0.1 till 251
sampleSize = seq(30,100,by=10);
repetition = 200;

print("total sample size are")
print(sampleSize);

checkValue = 0; kappa=1;

setwd("./src");

tempFileNames = array( 0 , dim=c(length(sampleSize)));

load(data_filename);

# apply estimators for every sampleSize and save them to temp files
for( i in 1:length(sampleSize) ) {

currentSampleNumbers = sampleNumbers[i,,];

# tempfile := PATH/temp/new_word.._currentSampleSize
# tempfile := PATH/temp/new_cancer_30
tempFile = sprintf("%s/temp/%s.RData" , as.character(PATH_PROJ), paste(word , sampleSize[i] , sep="_" ) );
print(dim(Data));
applyAllEstimators( Data , currentSampleNumbers , gammaValues , sampleSize[i] , repetition , checkValue , kappa , tempFile );
tempFileNames[i] = tempFile;
}
#setwd( PATH_PROJ );

## gathering data from temp files to outputFileName
#moveTempToFinal( tempFileNames , outputFileName ,word , namesOfVARS , namesOfEstimators , repetition , gammaValues , sampleSize );
#varsToSave = load(outputFileName);

## path where resutls will be stored
#resultsPrefile = sprintf("results/%s/%s_" , word , word );

## plot the data111
#plotAllFigures( outputFileName , resultsPrefile );

#setwd(thisFilePath);

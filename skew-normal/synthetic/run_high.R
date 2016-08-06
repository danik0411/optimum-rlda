rm(list=ls(all=TRUE));

source("main_test.R");

myEnv2 = new.env();

environment ( applyAllEstimators ) = myEnv2;

# in this pathfiles you can easily change the overall path by writing "/path_to_save/"
path_simulationResults = "simulatedResults/";	#path where you want to save files, do not forget backslash
path_sampleNumbers = "sampleNumbers/";		# path from where you read sampleNumbers, do not forger backshash

names_sampleNumbers = list.files( path_sampleNumbers );

for( i in 1:length(names_sampleNumbers) ) {

filename = sprintf( "%s%s", path_sampleNumbers, names_sampleNumbers[i]);

name = strsplit( names_sampleNumbers[i] ,"_" );
index = grep( pattern="\\." , name[[1]] );
appendName = gsub( "\\..*" , "" , name[[1]][index] );
appendName = sprintf( "%s%s" , "_" , appendName );
print(appendName);
applyAllEstimators( filename , path_simulationResults , appendName );

}
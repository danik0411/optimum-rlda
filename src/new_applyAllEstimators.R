# read and analyse sonar.txt

applyAllEstimators = function( estimatorsON , Data , sampleNumbers , gammaValues , sampleSize , repetition , checkValue=0, kappa=1 , filename , pathToSave = "" ) {

# i will add true/false statements for each estimator, so that only
# true labeled estimators will compute the stuff

# filename is the filename (including .ext) will be used to save all data for further analysis
# pathToSave is the path relative to getwd() to save all data, if you want to use absolute, then use absolute PATH

startTime = proc.time();

source("double.R");
source("CV.R");
source("plugin_rlda.R")
source("loo.R");
source("trueError.R");

myEnv = new.env();	# store functions in this environment

environment( CV ) = myEnv;
environment( estimateErrorDouble ) = myEnv;
environment( estimateTrueError ) = myEnv;
environment( LABELS_FROM_LDA_FUNCTION ) = myEnv;
environment( compareClassifierOutputs ) = myEnv;
environment( loo ) = myEnv;
environment( plugin_lda ) = myEnv;

print( sprintf("there are %.0f terms in gammaValues" , length(gammaValues)));
print( "sampleSize is:");
print( sampleSize ) ;
print( sprintf( "repetition is %.0f; checkValue is %.3f, kappa is %.3f" , repetition , checkValue , kappa ));

estimatorsName = c("Double" , "CV" , "LOO" , "Plugin" , "True");

rows = dim( Data )[1];
uns = dim( Data )[2];

print(dim(Data));

classes = c( 0 , 1 );

print(dim(Data));

dataWithLabels = Data;
dataWithoutLabels = Data[ -rows , ];

labelRow = Data[rows , ];

sample0 = which( labelRow %in% classes[1] );	# position of class0 in data
sample1 = which( labelRow %in% classes[2] );	# position of class1 in data

un0 = length( sample0 );
un1 = length( sample1 );
uns = c ( un0 , un1 );

r = un0 / un1 ;      # ratio class0 to class1
p = dim( dataWithoutLabels )[1];    # number of features

totalErrorDouble = array( 0,dim = c( repetition, length( gammaValues ) ));
totalErrorCV     = array( 0,dim = c( repetition, length( gammaValues ) ));
totalErrorLOO    = array( 0,dim = c( repetition, length( gammaValues ) ));
totalErrorPlugin = array( 0,dim = c( repetition, length( gammaValues ) ));
totalErrorTrue   = array( 0,dim = c( repetition, length( gammaValues ) ));

totalTimeDouble = array( 0 , dim = c( 1 , repetition ) );
totalTimeCV     = array( 0 , dim = c( 1 , repetition ) );
totalTimeLOO    = array( 0 , dim = c( 1 , repetition ) );
totalTimePlugin = array( 0 , dim = c( 1 , repetition ) );
totalTimeTrue   = array( 0 , dim = c( 1 , repetition ) );

print ( sprintf("started doing computations on sampleSize of %i at %s" , sampleSize , as.character(Sys.time()) ) );

n0 = length( sampleNumbers[[1,1]] );
n1 = length( sampleNumbers[[1,2]] );
ns = c( n0, n1 );

alpha0 = n0 / ( n0 + n1 );
alpha1 = n1 / ( n0 + n1 );
alphas = cbind ( alpha0 , alpha1 );

for( counter2 in 1:repetition ) {

shuffledClass0 = sampleNumbers[[ counter2 , 1 ]];
shuffledClass1 = sampleNumbers[[ counter2 , 2 ]];

# print( sprintf("[n0 n1] is [%i %i], the input is [%i %i]" , n0, n1, length(shuffledClass0) , length(shuffledClass1) ) );

X0 = dataWithoutLabels[ , shuffledClass0 ];
X1 = dataWithoutLabels[ , shuffledClass1 ];

x0 = rowMeans( X0 );	  x1 = rowMeans( X1 );
xs = cbind( x0 , x1 );

C = ( ( n0 - 1 ) * cov( t(X0) ) + ( n1 - 1 ) * cov( t(X1) ) ) / ( n0 + n1 - 2 );

C = as.matrix( C );

XX0 = dataWithLabels[ , shuffledClass0 ];
XX1 = dataWithLabels[ , shuffledClass1 ];

trnData = cbind( dataWithLabels[ ,  shuffledClass0 ] , dataWithLabels[ ,  shuffledClass1 ] );
tstData = cbind( dataWithLabels[ , -shuffledClass0 ] , dataWithLabels[ , -shuffledClass1 ] );

t1=0;t2=0;t3=0;t4=0;t5=0;t6=0;t7=0;t8=0;t9=0;t10=0;

if ( estimatorsON[1] == TRUE ){
t1 = proc.time();
errorDouble = estimateErrorDouble ( X0 , X1 , C , xs , ns , alphas , p , kappa , gammaValues, checkValue );
t2 = proc.time();
} else if( estimatorsON[1] == FALSE ) { errorDouble = array( 0 ,dim=length(gammaValues));}
else {print("check estimatorsON, something is incorrect there")};

if ( estimatorsON[2] == TRUE ){
t3 = proc.time();
errorCV = CV( XX0 , XX1 , 5 , 5 , kappa , gammaValues , checkValue );
t4 = proc.time();
} else if ( estimatorsON[2] == FALSE ) {
errorCV = array( 0 , dim = length(gammaValues));
} else {(print("check estimatorsON, something is incorrect there"));}


if ( estimatorsON[3] == TRUE ) {
t5 = proc.time();
errorLOO = loo( trnData , kappa , gammaValues , checkValue , classes , alphas );
t6 = proc.time();
} else if ( estimatorsON[3] == FALSE ) {
errorLOO = array( 0 , dim= length(gammaValues));
} else {print("check estimatorsON, something is wrong there");};

if ( estimatorsON[4] == TRUE ) {
t7 = proc.time();
errorPlugin = plugin_lda( X0 , X1 , C , xs , alphas , p , kappa , gammaValues , checkValue );
t8 = proc.time();
} else if ( estimatorsON[4] == FALSE ) {
errorPlugin = array(0 , dim= length(gammaValues));}
else { print("check for estimatorsON, somethign is wrong there");}


if( estimatorsON[5] == TRUE ){
t9 = proc.time();
errorTrue = estimateTrueError( trnData , tstData , kappa , gammaValues , checkValue , alphas );
t10 = proc.time();
} else if ( estimatorsON[5] == FALSE ){
errorTrue = array(0, dim=length(gammaValues));}
else { print("check for estimatorsON, something is wrong there");}

totalTimeDouble[ counter2 ] = t2[3] - t1[3];
totalTimeCV    [ counter2 ] = t4[3] - t3[3];
totalTimeLOO   [ counter2 ] = t6[3] - t5[3];
totalTimePlugin[ counter2 ] = t8[3] - t7[3];
totalTimeTrue  [ counter2 ] = t10[3] - t9[3];

totalErrorDouble[ counter2 , ] = errorDouble;
totalErrorCV    [ counter2 , ] = errorCV;
totalErrorLOO   [ counter2 , ] = errorLOO;
totalErrorPlugin[ counter2 , ] = errorPlugin;
totalErrorTrue  [ counter2 , ] = errorTrue;

}

print ( sprintf("finished computations on sampleSize of %i at %s" , sampleSize , as.character(Sys.time()) ) );

finishTime = proc.time();
print( sprintf("the process took %.3f seconds (%.3f hours)" , (finishTime[3] - startTime[3]) , (finishTime[3] - startTime[3] )/3600 ) );

totalFileName = sprintf( "%s%s" , pathToSave , filename );

print( sprintf( "saving results to %s" , totalFileName ) );

for ( i in 1:length(estimatorsON) ) {
if ( estimatorsON[i] == TRUE ){
   varToSave = grep( x=ls(all=TRUE , pattern="total") , pattern=estimatorsName[i] , value=TRUE );
   fileSaveName = paste(filename , estimatorsName[i] , sep="" );
   save(list=varToSave, file=fileSaveName);
   print( paste("saving to " , fileSaveName , sep=""));
}
}
#varToSave = ls( all=TRUE , patter = "total" );	   # save only variables with "total" in name

#save( list=varToSave , file=totalFileName );
cat("\n\n");

return(NULL);


}

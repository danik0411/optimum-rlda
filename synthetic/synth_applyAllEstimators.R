# synthetic data

synth_applyAllEstimators = function( sampleSize , dist , p , repetition , gammaValues , checkValue=0 , kappa=1 , outputFileNamePrefile ) {

library("mvtnorm");

source("../src/double.R");
source("../src/CV.R");
source("../src/plugin_rlda.R")
source("../src/loo.R");
source("synth_trueError.R");

myEnv = new.env();	# store functions in this environment

environment( CV ) = myEnv;
environment( estimateErrorDouble ) = myEnv;
environment( estimateTrueError ) = myEnv;
environment( LABELS_FROM_LDA_FUNCTION ) = myEnv;
environment( compareClassifierOutputs ) = myEnv;
environment( loo ) = myEnv;
environment( plugin_lda ) = myEnv;

print( sprintf("there %.0f gammaValues; p is %.0f; dist is %.2f; repetition is %.0f" , length(gammaValues) , p , dist , repetition ));
print( "sampleSize is" );
print(sampleSize);

startTime = proc.time();

classes = c(0,1);

# defining the properties of Gaussian distribution
sigma = array( 0.1 , dim=c( p , p ) );
diag( sigma ) = 1;
x0 = array( 1 , dim=c( p , 1 ) );    # rowMeans of something
x1 = -x0;      	 	      	     # rowMeans of something

temp = t ( x0 - x1 ) %*% solve( sigma ) %*% ( x0 - x1 );
a = sqrt ( dist / temp );	# mean value

meanVector = rep( a , p );	# vector with means

means = cbind( meanVector , -meanVector );

r = 1 ;  	 # ratio of class0 samples to class1 samples
alphas=c(0.5,0.5);

totalErrorDouble = array( -1 , dim=c(repetition , length(gammaValues) ));
totalErrorCV     = array( -1 , dim=c(repetition , length(gammaValues) ));
totalErrorLOO    = array( -1 , dim=c(repetition , length(gammaValues) ));
totalErrorPlugin = array( -1 , dim=c(repetition , length(gammaValues) ));
totalErrorTrue   = array( -1 , dim=c(repetition , length(gammaValues) ));

totalTimeDouble = array( -1 , dim=c( repetition) );
totalTimeCV     = array( -1 , dim=c( repetition) );
totalTimeLOO    = array( -1 , dim=c( repetition) );
totalTimePlugin = array( -1 , dim=c( repetition) );
totalTimeTrue   = array( -1 , dim=c( repetition) );

# print( means );
# print( (sigma) );

n = sampleSize;
n0 = n/2;
n1 = n/2;
ns = c( n0 , n1);

print ( sprintf("starting computations on sampleSize of %i at %s" , n , as.character(Sys.time()) ) );

for ( j in 1:repetition ) {

X0_unlabeled = t( rmvnorm( n0 , mean = meanVector , sigma = sigma ) );
X1_unlabeled = t( rmvnorm( n1 , mean = -meanVector , sigma = sigma ) );

# print( X0_unlabeled );
# print( X1_unlabeled );

X0_labeled = rbind( X0_unlabeled , 0 );
X1_labeled = rbind( X1_unlabeled , 1 );

# starting computing things

x0 = as.matrix( rowMeans( X0_unlabeled ) , ncol=1);
x1 = as.matrix( rowMeans( X1_unlabeled ) , ncol=1);

xs = cbind( x0, x1);

C = ( ( n0 - 1 ) * cov( t(X0_unlabeled) ) + ( n1 - 1 ) * cov( t(X1_unlabeled) ) ) / ( n0 + n1 - 2 );
C = as.matrix(C);

trnData = cbind( X0_labeled , X1_labeled );

t1=0; t2=0; t3=0; t4=0; t5=0; t6=0; t7=0; t8=0; t9=0; t10=0;

t1 = proc.time();
errorDouble = estimateErrorDouble ( X0_unlabeled , X1_unlabeled , C , xs , ns , alphas , p , kappa , gammaValues, checkValue );
t2 = proc.time();

t3 = proc.time();
errorCV = CV( X0_labeled , X1_labeled , 5 , 5 , kappa , gammaValues , checkValue );
t4 = proc.time();

t5 = proc.time();
errorLOO = loo( trnData , kappa , gammaValues , checkValue , classes , alphas );
t6 = proc.time();

t7 = proc.time();
errorPlugin = plugin_lda( X0_unlabeled , X1_unlabeled , C , xs , alphas , p , kappa , gammaValues , checkValue );
t8 = proc.time();

t9 = proc.time();
errorTrue = estimateTrueError( X0 , X1 , sigma , C , means , xs , alphas , p , kappa , gammaValues , checkValue );
t10 = proc.time();

totalTimeDouble[j] = t2[3] - t1[3];
totalTimeCV    [j] = t4[3] - t3[3];
totalTimeLOO   [j] = t6[3] - t5[3];
totalTimePlugin[j] = t8[3] - t7[3];
totalTimeTrue  [j] = t10[3] - t9[3];

totalErrorDouble[j,] = errorDouble;
totalErrorCV    [j,] = errorCV;
totalErrorLOO   [j,] = errorLOO;
totalErrorPlugin[j,] = errorPlugin;
totalErrorTrue  [j,] = errorTrue;

}

print ( sprintf("finished computations on sampleSize of %i at %s" , n , as.character(Sys.time()) ) );

finishTime = proc.time();
print( sprintf("the process took %.3f seconds (%.3f hours)" , (finishTime[3] - startTime[3]) , (finishTime[3] - startTime[3] )/3600 ) );

varToSave = ls( all=TRUE , patter = "total" );	   # save only variables with "total" in name

print(paste("saving to file ", outputFileNamePrefile));
save( list=varToSave , file=outputFileNamePrefile );
cat("\n\n");

return(NULL);
}

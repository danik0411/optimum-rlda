# i wanna do the thing

# i will create an array of sample numbers, and then use them for analysis

rm(list=ls(all=TRUE))	# clear all environments

load("Data.RData");
Data = t( Data );

Data = Data[ -1 ,  ];

kappa = 1;
checkValue = 0;
repetition = 500;

rows = dim( Data )[1];
uns = dim( Data )[2];
orgClasses = unique( Data [ rows , ] );

classes = c( 0, 1 );

Data[ rows , Data[ rows , ] == orgClasses[1] ] = classes[1];
Data[ rows , Data[ rows , ] == orgClasses[2] ] = classes[2];

trueLabels = Data[rows , ];

class0 = which( trueLabels %in% classes[1] );
class1 = which( trueLabels %in% classes[2] );

un0 = length( class0 );
un1 = length( class1 );
uns = un0 + un1;

r = un0 / un1;

sampleSize = seq( 30 , 120 , by=10 );

sampleNumbers = array( list() , dim=c( length(sampleSize) , repetition , length(classes) ) );

for ( i in 1:length(sampleSize) ) {

    n1 = floor ( sampleSize[i] / ( 1 + r ) );
    n0 = sampleSize[i] - n1;

    for ( j in 1:repetition ) {

    	sample0 = sample( class0 , n0 );
	sample1 = sample( class1 , n1 );
    	
    	sampleNumbers[[ i , j , 1 ]] = sample0;
    	sampleNumbers[[ i , j , 2 ]] = sample1;
    }
}

# save( list = "sampleNumbers" , file="cancer_sampleNumbers.RData" );
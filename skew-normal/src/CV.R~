CV<-function(X1,X2,Folds=5,R=5, kappa, gammaValues, checkValue) {
#X1 is the data matrix for class 1 (p x n1 matrix )
#X2 is the data matrix for class 2 (p x n2 matrix )
#Folds is number of folds. default is 5	
#R is number of repeatations. default is 5	

n1 = dim(X1)[2]
n2 = dim(X2)[2]
rows = dim(X1)[1];
num.samp.fold = floor((n1+n2)/Folds)
#true.labels = c(rep(0,n1),rep(1,n2))
Data = cbind(X1,X2) 
true.labels = array(Data[rows,] , dim=c(1,dim(Data)[2] ));
x = 1:(n1+n2)

output = 0 * c( 1:length(gammaValues) );

for ( counter1 in 1:length( gammaValues ) ) {
    
      	CV.err=0
	gamma = gammaValues[counter1];
	for (j in 1:R) { #repeat R times
 		label.diff=0
 		y=sample(x)  #sampling without replacement
 		for (i in 1:Folds) { #look at each fold
   			if (i!=Folds){
  				y1=y[((((i-1)*num.samp.fold)+1):(i*num.samp.fold))]
  			} else {
   			y1=y[((((i-1)*num.samp.fold)+1):length(x))]	
  			}

  			CV.Data.Train=Data[,-y1] #holding out one fold for test
  			CV.Data.Test=Data[,y1]   #settig the held out fold as test data
   			classified.label=LABELS_FROM_LDA_FUNCTION(CV.Data.Train, CV.Data.Test, kappa, gamma, checkValue) #Write this function. The input is the Train Data with one fold held out and the test data and the output is a vector of 1's and 2's showing the label of test data as determined by the surrogate classifier (meaning the classifier designed on the current training data)
			compared = true.labels[1, y1];
  			label.diff=label.diff+sum(abs(classified.label-compared))

  		}	
  	CV.err=CV.err+label.diff/length(true.labels)
	}

	Cv.err=CV.err/R
	output[counter1] = Cv.err;

}
return(output)
}

LABELS_FROM_LDA_FUNCTION = function ( trnData, tstData, kappa=1, gamma=1 , checkValue = 0 ) {

	# p feature , trnX1

	p = dim(trnData)[1];	# with class

	trnX0 = trnData[ c(1:(p-1)) , trnData [ p , ] == 0 ];
	trnX1 = trnData[ c(1:(p-1)) , trnData [ p , ] == 1 ];

	x0 = as.matrix(rowMeans(trnX0));
	x1 = as.matrix(rowMeans(trnX1));

	tstX = tstData[ 1:(p-1) , ];	# tstData without sample number and true class
	tstN = dim ( tstX )[2];
#	print( paste("data size is ", dim(trnX0)[1] , " " , dim(trnX0)[2] , sep=""));
	p = dim(trnX0)[1];		# without class and first row (sample number);

	n0 = dim ( trnX0 )[2];
	n1 = dim ( trnX1 )[2];

	C = ( ( n0 - 1 ) * cov( t(trnX0) ) + ( n1 - 1 ) * cov( t(trnX1) ) ) / ( n0 + n1 - 2 );
	C = as.matrix(C);

	H = as.matrix(solve( diag(p) + gamma * C ));

	# calculating summing and multiplier coefficients
	mulConst = kappa * H%*%( x0 - x1 );
	sumConst = - t( x0 + x1 ) %*% mulConst / 2;

	output = 0 * c(1:tstN) - 1;

	for ( i in 1:tstN ) {
		x = as.matrix(tstX[ , i ]);	# inputData
		value = t(x) %*% mulConst + sumConst;

		if ( value > checkValue ) {
			output[i] = 0;}
		else {output[i] = 1;}
	}

	return ( output );

}

compareClassifierOutputs = function( estimatedOutput , trueOutput , trueLabel ) {
	elementsInTrue = trueOutput[ trueOutput == trueLabel ];
	elementsInEstd = estimatedOutput[ trueOutput == trueLabel ];

	output = sum( abs( elementsInTrue - elementsInEstd ) ) / length( elementsInTrue );

	return( output );
	
}

# this file calculates the true error of the data given into it. firstly it trains using training data, then apply the classifier on the testing data and the find the error

estimateTrueError = function ( trnData , tstData , kappa , gammaValues , checkValue , alphas ) {
	
	error = 0 * c( 1:length(gammaValues) );
	alpha0 = alphas[1];
	alpha1 = alphas[2];

	for ( i in 1:length(gammaValues) ) {
    	    gamma = gammaValues[i];
    	    estimatedOutput = LABELS_FROM_LDA_FUNCTION ( trnData , tstData, kappa , gamma , checkValue );
    	    trueOutput = t( as.matrix( tstData [ dim(tstData)[1] , ] ) );

     	    e1 = compareClassifierOutputs ( estimatedOutput , trueOutput , 0 );
    	    e2 = compareClassifierOutputs ( estimatedOutput , trueOutput , 1 );
    	    error[ i ] = alpha0 * e1 + alpha1 * e2;

}

	return( error );

}


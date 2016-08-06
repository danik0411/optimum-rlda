# this file calculates the true error of the data given into it. firstly it trains using training data, then apply the classifier on the testing data and the find the error

estimateTrueError = function ( trnX0 , trnX1 , tstX0 , tstX1 , C , kappa , gammaValues , checkValue , alphas ) {

	p = dim( trnX0 )[1];

	# tst data
	tstX = cbind( tstX0, tstX1 );

	tst_n0 = dim( tstX0 )[2];
	tst_n1 = dim( tstX1 )[2];

	# trn data
	x0 = as.matrix(rowMeans(trnX0));
	x1 = as.matrix(rowMeans(trnX1));

	trn_n0 = dim( trnX0 )[2];
	trn_n1 = dim( trnX1 )[2];
	
	# rlda classifier

	true0 = array( 0 , dim = tst_n0 );
	true1 = array( 1 , dim = tst_n1 );
	trueLabels = c( true0 , true1 );

	estimatedLabels = array(-1, dim=(tst_n0+tst_n1) );
	value = array( -1 , dim=(tst_n0 + tst_n1) );
	
	error = array( -1 , dim=length(gammaValues) );

	alpha0 = alphas[1];
	alpha1 = alphas[2];

	for ( i in 1:length(gammaValues) ) {
    	    	gamma = gammaValues[i];

	    	Ip = diag(p);
	    	H = as.matrix( solve(Ip + gamma*C) );


		# w = kappa * t( x - (x0+x1)/2) %*% H %*% (x0 - x1)
		# w = kappa * t( x ) %*% H %*% (x0 - x1) - kappa * t(x0-x1)/2 %*% H %*% (x0 - x1)
		# w = t ( x ) %*% k + a
		# k = kappa * H %*% (x0 - x1)
		# a = -t(x0+x1)/2 %*% k

		mulConst = kappa * H %*% ( x0 - x1 );
		sumConst = - t ( x0 + x1 ) %*% mulConst / 2;

		for ( j in 1:(tst_n0+tst_n1) ) {
			tst_x = as.matrix( tstX[,j] );
			value[j] = t(tst_x) %*% mulConst + sumConst; # rlda value
		}

#		print("printing trueLabeles");
#		print(trueLabels)
#		print("printing values");
#		print(value);

		estimatedLabels = value;
		estimatedLabels [ value <= checkValue ] = 1;
		estimatedLabels [ value >  checkValue ] = 0;

#		print("printtin estimatedLables");
#		print(estimatedLabels);

		est0 = estimatedLabels [ trueLabels == 0];
		est1 = estimatedLabels [ trueLabels == 1];

		
		e0 = sum( abs( true0 - est0 ) ) / tst_n0;
		e1 = sum( abs( true1 - est1 ) ) / tst_n1;

		#print( paste( "e0 and e1 are " , as.character(e0) , " and " , as.character(e1) , sep = ""));

    	    	error[ i ] = alpha0 * e0 + alpha1 * e1;
		#print( paste("current error is " , as.character(error[i]), sep=""));


	}

	return( error );

}


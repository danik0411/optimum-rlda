estimateErrorDouble = function ( X0 , X1 , C , xs , ns , alphas , p , kappa , gammaValues , checkValue ){

# data should be without real class label

n0 = ns[1];   	   n1 = ns[2];

x0 = xs[ , 1 ];	   x1 = xs[ , 2 ];

den = ( n0 + n1 - 2 );
d1 = p / den;

totalError = 0 * c( 1:length(gammaValues) );

for ( j in 1:length( gammaValues ) ) {

	gamma = gammaValues[j];
	H = as.matrix( solve ( diag ( p ) + gamma * C ) );

	tr = sum ( diag ( H ) );
	d2 = tr / den;
	delta = ( p - tr ) / ( 1 - d1 + d2 );

	err = -1 * c(1:2);
	error = 0;

    	diff = as.matrix( x0 - x1 , ncol = 1 );

	myD = t( diff ) %*% H %*% C %*% H %*% ( diff );
	myGG = t( diff ) %*% H %*% ( diff ) / 2;

	for ( i in 1:2 ) {

	      	myG = (-1)^(i-1)*myGG;

		a = ( (-1)^i * myG + delta / gamma / ns[i] + (-1)^(i+1) * checkValue / kappa ) / sqrt( ( 1 + delta / den )^2 * myD ) ;
		err[i] = pnorm( a );
	}

	totalError[ j ] = err[1]*alphas[1] + err[2]*alphas[2];

}

return(totalError);

}
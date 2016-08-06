plugin_lda = function( X0 , X1 , C , xs , alphas , p , kappa , gammaValues , checkValue ) {

x0 = xs[ , 1];	       x1 = xs[ , 2];

totalError = c(1:length(gammaValues)) * 0;

for ( i in 1:length( gammaValues ) ) {

    gamma = gammaValues[i];
    H = as.matrix( solve ( diag( p ) + gamma * C ) );

    err = c ( -1 , -1 ) ;

    diff = as.matrix( x0 - x1 , ncol = 1 );

    myD = t( diff ) %*% H %*% C %*% H %*% ( diff );
    myGG = t( diff ) %*% H %*% diff / 2;

    for ( j in 1:2 ) {
    	myG = (-1)^(j-1) * myGG;

	a = ( (-1)^j * myG + (-1)^(j+1) * checkValue / kappa ) / ( sqrt( myD ) );

	err[j] = pnorm(a);

    }

    totalError[i] = err[1]*alphas[1] + err[2] * alphas[2];

}

return( totalError );

}
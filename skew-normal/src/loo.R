# leave one out

loo = function ( data , kappa , gammaValues , checkValue , classes , alphas ) {

rows = dim( data )[1];
p = rows - 1;

ns = dim( data )[2];
estimatedOutput = array( -1 , dim=c( ns , length(gammaValues) ) );
trueOutput = data[ rows, ];

error = array( -1 , dim=c( 1 , length(gammaValues) ) );

for ( i in 1:ns ) {

loo_data = data[ , -i ];

x = as.matrix( data[ c(1:(rows-1)) , i ] );

X0 = loo_data [ -rows , loo_data[ rows , ] == classes[1] ];

X1 = loo_data [ -rows , loo_data[ rows , ] == classes[2] ];

x0 = as.matrix( rowMeans( X0 ) );
x1 = as.matrix( rowMeans( X1 ) );

n0 = dim( X0 )[2];
n1 = dim( X1 )[2];

C = ( ( n0 - 1 ) * cov( t(X0) ) + ( n1 - 1 ) * cov( t(X1) ) ) / ( n0 + n1 - 2 );
C = as.matrix(C);

for ( j in 1:length( gammaValues ) ) {

    gamma = gammaValues[j];

    H = as.matrix( solve( diag(p) + gamma * C ) );

    mulConst = kappa * H %*% ( x0 - x1 );
    sumConst = - t( x0 + x1 ) %*% mulConst / 2;

    value = t(x) %*% mulConst + sumConst;

    if ( value > checkValue ) {
       estimatedOutput[ i, j] = classes[1]; }
    else { estimatedOutput[ i , j ] = classes[2]; }

}

}

for ( i in 1 : length( gammaValues ) ) {

e1 = compareClassifierOutputs ( t( estimatedOutput[ , i ] ) , trueOutput , classes[1] );
e2 = compareClassifierOutputs ( t( estimatedOutput[ , i ] ) , trueOutput , classes[2] );
error[ i ] = alphas[1] * e1 + alphas[2] * e2;

}

return( error );

}

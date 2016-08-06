# this file will analyse computed data for cancer

plotAllFigures = function( loadfile , prefile ) {

# loadfile is the path to the file with results
# prefile is the string that will be written before the figureName
# do not worry about the cancer_totaError...
# the code will automatically understand that is should search for cancer, sonar or synth, it analyses the words ErrorDouble , TimeDoube and etc

source("src/myPlotFunction.R");

plot_env = new.env();

environment( myPlotFunction ) = plot_env;

varList = load(loadfile);

allCols = c("red" , "purple" , "cyan" , "magenta" , "orange" , "blue" , "green" , "gold" , "grey" , "black" )

t1 = grep(pattern="ErrorDouble" , varList , value=TRUE);
t2 = grep(pattern="ErrorCV"     , varList , value=TRUE);
t3 = grep(pattern="ErrorLOO"    , varList , value=TRUE);
t4 = grep(pattern="ErrorPlugin" , varList , value=TRUE);
t5 = grep(pattern="ErrorTrue"   , varList , value=TRUE);

d1 = grep(pattern="TimeDouble" , varList , value=TRUE);
d2 = grep(pattern="TimeCV"     , varList , value=TRUE);
d3 = grep(pattern="TimeLOO"    , varList , value=TRUE);
d4 = grep(pattern="TimePlugin" , varList , value=TRUE);
d5 = grep(pattern="TimeTrue"   , varList , value=TRUE);

td = eval(parse(text=t1));
tc = eval(parse(text=t2));
tl = eval(parse(text=t3));
tp = eval(parse(text=t4));
tt = eval(parse(text=t5));

dd = eval(parse(text=d1));
dc = eval(parse(text=d2));
dl = eval(parse(text=d3));
dp = eval(parse(text=d4));
dt = eval(parse(text=d5));

minGammaPosDouble = apply( td , c(3,1) , which.min );
minGammaPosCV     = apply( tc , c(3,1) , which.min );
minGammaPosLOO    = apply( tl , c(3,1) , which.min );
minGammaPosPlugin = apply( tp , c(3,1) , which.min );
minGammaPosTrue   = apply( tt , c(3,1) , which.min );

optTruePerRepDouble = array( -1 , dim=dim(minGammaPosDouble) );
optTruePerRepCV     = array( -1 , dim=dim(minGammaPosDouble) );
optTruePerRepLOO    = array( -1 , dim=dim(minGammaPosDouble) );
optTruePerRepPlugin = array( -1 , dim=dim(minGammaPosDouble) );

for( i in 1:length(sampleSize) ) {
     for( j in 1:repetition ) {

optTruePerRepDouble[i,j] = tt[ j , minGammaPosDouble[i,j] , i];
optTruePerRepCV[i,j]     = tt[ j , minGammaPosCV    [i,j] , i];
optTruePerRepLOO[i,j]    = tt[ j , minGammaPosLOO   [i,j] , i];
optTruePerRepPlugin[i,j] = tt[ j , minGammaPosPlugin[i,j] , i];

     }
}

meanOptTrueDouble = rowMeans( optTruePerRepDouble );
meanOptTrueCV     = rowMeans( optTruePerRepCV     );
meanOptTrueLOO    = rowMeans( optTruePerRepLOO    );
meanOptTruePlugin = rowMeans( optTruePerRepPlugin );

meanErrorDouble = t( apply( td , 3 , colMeans ) );
meanErrorCV     = t( apply( tc , 3 , colMeans ) );
meanErrorLOO    = t( apply( tl , 3 , colMeans ) );
meanErrorPlugin = t( apply( tp , 3 , colMeans ) );
meanErrorTrue   = t( apply( tt , 3 , colMeans ) );

meanTimeDouble = rowMeans( dd );
meanTimeCV     = rowMeans( dc );
meanTimeLOO    = rowMeans( dl );
meanTimePlugin = rowMeans( dp );
meanTimeTrue   = rowMeans( dt );

print("starting drawing the plots");

estimatorsText = c("doubleAssym" , "CV" , "loo" , "plugin" );

myPlotFunction(	type="eps" , 
		filename=sprintf("%soptimalTrueError.eps" , prefile),
		sampleSize,
		rbind( meanOptTrueDouble , meanOptTrueCV , meanOptTrueLOO , meanOptTruePlugin ),
		legText=estimatorsText,
		cols=allCols[1:length(estimatorsText)],
		title="Expected Optimal True Error by Estimators",
		xlabel="Sample Size",
		ylabel="Error",
		legPosition="top",
		horiz=TRUE ,
		width=10,
		height=10 );

myPlotFunction( type="eps",
		filename=sprintf("%stotalDouble.eps", prefile),
		gammaValues,
		meanErrorDouble,
		legText=as.character(sampleSize),
		cols=allCols[1:length(sampleSize)] , 
		title="DoubleAssym Against Gamma Value",
		xlabel="Gamma Value",
		ylabel="Error",
		legPosition="top",
		width=10,
		height=10,
		legRows=5 );

myPlotFunction( type="eps",
		filename=sprintf("%stotalCV.eps", prefile),
		gammaValues,
		meanErrorCV,
		legText=as.character(sampleSize),
		cols=allCols[1:length(sampleSize)] , 
		title="CV Against Gamma Value",
		xlabel="Gamma Value",
		ylabel="Error",
		legPosition="top",
		width=10,
		height=10,
		legRows=5 );

myPlotFunction( type="eps",
		filename=sprintf("%stotalLOO.eps", prefile),
		gammaValues,
		meanErrorLOO,
		legText=as.character(sampleSize),
		cols=allCols[1:length(sampleSize)] , 
		title="loo Against Gamma Value",
		xlabel="Gamma Value",
		ylabel="Error",
		legPosition="top",
		width=10,
		height=10,
		legRows=5 );

myPlotFunction( type="eps",
		filename=sprintf("%stotalPlugin.eps", prefile),
		gammaValues,
		meanErrorPlugin,
		legText=as.character(sampleSize),
		cols=allCols[1:length(sampleSize)] , 
		title="Plugin Against Gamma Value",
		xlabel="Gamma Value",
		ylabel="Error",
		legPosition="top",
		width=10,
		height=10,
		legRows=5 );

myPlotFunction( type="eps",
		filename=sprintf("%stotalTrue.eps", prefile),
		gammaValues,
		meanErrorTrue,
		legText=as.character(sampleSize),
		cols=allCols[1:length(sampleSize)] , 
		title="True Against Gamma Value",
		xlabel="Gamma Value",
		ylabel="Error",
		legPosition="top",
		width=10,
		height=10,
		legRows=5 );

myPlotFunction( type="eps",
		filename=sprintf("%smeanTime.eps", prefile),
		sampleSize,
		rbind( meanTimeDouble , meanTimeCV , meanTimeLOO , meanTimePlugin ),
		legText=estimatorsText,
		cols=allCols[1:length(estimatorsText)],
		title="Average Time Spent by Estimator on Each Sample",
		xlabel="Sample Size",
		ylabel="Seconds, (s)",
		legPosition="top",
		width=10,
		height=10,
		horiz=TRUE );

return(NULL);

}

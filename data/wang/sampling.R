# i wanna do the thing

# i will create an array of sample numbers, and then use them for analysis

rm(list=ls(all=TRUE))	# clear all environments

uns = read.delim(file="WangBreast2005_22215x276.txt", header = FALSE, sep = ",", nrows=1)
un0=as.numeric(uns[1]) #first n0 samples are from one class
un1=as.numeric(uns[2]) #first n1 samples are from one class
totalData = read.delim(file="WangBreast2005_22215x276.txt", header = FALSE, sep = "\t", skip=2);
unchangedTotalData = totalData;
print("class of totalData is");
print(class(totalData));
totalData = as.matrix(totalData);
print("after converting to matrix, class of total data is");
print(class(totalData));

print("size of totalData before removing rows with zeros is");
print(dim(totalData));
# find and remove rows that contain zero values
nonZeroRows = apply(totalData,1, function(row) all(row == 0) );
totalData = totalData[ !nonZeroRows , ];
print("size of totalData after removing rows with zeros is");
print(dim(totalData));

# apply ttest to estimate features
p.val.vec=apply(totalData, 1, function(totalData) {
	t.test(x=totalData[1:un0],y=totalData[(un0+1):(un0+un1)])$p.value})
index.smallest.p.values=sort(p.val.vec, index.return=TRUE)$ix
#Lets take D=10 best features. You can take 50 and/or 100
D=50;
Data=totalData[index.smallest.p.values[1:D],];

classes = c( 0, 1 );
trueLabels = cbind( array(classes[1], dim=c(1,un0) ) , array(classes[2],dim=c(1,un1)) );
#print( dim(trueLabels));
#print( dim(Data));
Data = rbind(Data, trueLabels);

kappa = 1;
checkValue = 0;
repetition = 500;

rows = dim( Data )[1];
uns = dim( Data )[2];

class0 = which( trueLabels %in% classes[1] );
class1 = which( trueLabels %in% classes[2] );

un0 = length( class0 );
un1 = length( class1 );
uns = un0 + un1;

r = un0 / un1;

sampleSize = seq( 30 , 100 , by=10 );

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

save( list = c("sampleNumbers" , "Data") , file="wang_p50.RData" );

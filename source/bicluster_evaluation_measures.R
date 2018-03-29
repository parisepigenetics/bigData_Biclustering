###Constancy by row measure (Santamaria et al.2007)###
constancy.byRow <- function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]    
B=0    
for( i in 1:(n-1)){
    for (j in (i+1):n){
        A=0
        for(k in 1:m){
            A=A+(bic[i,k]-bic[j,k])**2
            }
        B=B+sqrt(A)
    }
}
return(B/n)
}

###Constancy by column measure (Santamaria et al.2007)###
constancy.byColumn <- function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]    
B=0    
for( i in 1:(m-1)){
    for (j in (i+1):m){
        A=0
        for(k in 1:n){
            A=A+(bic[k,i]-bic[k,j])**2
            }
        B=B+sqrt(A)
    }
}
return(B/m)
}


###Overall constancy (Santamaria et al.2007)###
constancy.overall <- function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]    
Cr=constancy.byRow(bic)
Cc=constancy.byColumn(bic)
return((n*Cr+m*Cc)/(n+m))
}


### Additive coherence by column(Santamaria et al.2007)###
coherence.additive.byColumn <-function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]
##Bicluster transformation##    
for(i in 1:n){
	for(j in m:1){
		if (i != 1){
			bic[i,j]=bic[i,j]-bic[i-1,j]
		}
	}
}
return(constancy.byColumn(bic))
}


### Additive coherence by row(Santamaria et al.2007)###
coherence.additive.byRow <-function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]
##Bicluster transformation##    
for(i in 1:m){
	for(j in n:1){
		if (i != 1){
			bic[j,i]=bic[j,i]-bic[j,i-1]
		}
	}
}
return(constancy.byRow(bic))
}


### Multiplicative coherence by row(Santamaria et al.2007)###
coherence.multiplicative.byRow <-function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]
##Bicluster transformation##    
for(i in 1:n){
	for(j in m:1){
		if ((j != 1)&&(bic[i,j-1]!=0)){
			bic[i,j]=bic[i,j]/bic[i,j-1]

		}
	}
}
return(constancy.byRow(bic))
}

### Multiplicative coherence by column(Santamaria et al.2007)###
coherence.multiplicative.byColumn <-function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]
##Bicluster transformation##    
for(j in 1:m){
	for(i in n:1){
		if ((i != 1)&&(bic[i-1,j]!=0)){
			bic[i,j]=bic[i,j]/bic[i-1,j]

		}
	}
}
return(constancy.byColumn(bic))
}


### Coherence by row(Santamaria et al.2007)###
coherence.byRow <-function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]
##Bicluster transformation##    
for(i in 1:n){
	for(j in m:1){
		if (j == 1)
			if(bic[i,j]>1)
				bic[i,j]=1
			else
				bic[i,j]=-1

		else
			if(bic[i,j]>bic[i,j-1])
				bic[i,j]=1
			else
				bic[i,j]=-1
	}	

}
return(constancy.byRow(bic))
}


### Coherence by column(Santamaria et al.2007)###
coherence.byColumn <-function(bic) {
n=dim(bic)    [1]
m=dim(bic)    [2]
##Bicluster transformation##    
for(j in 1:m){
	for(i in n:1){
		if (i == 1)
			if(bic[i,j]>1)
				bic[i,j]=1
			else
				bic[i,j]=-1

		else
			if(bic[i,j]>bic[i-1,j])
				bic[i,j]=1
			else
				bic[i,j]=-1
	}	

}
return(constancy.byRow(bic))
}





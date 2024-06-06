rdirichlet<- function(n,alpha) {
	r <- matrix(0,n,length(alpha))
	for(i in 1:length(alpha)) {
		r[,i] <- rgamma(n,alpha[i],1)
	}
	rowSum <- apply(r,1,sum)
	r <- apply(r,2,'/',rowSum)
	return(r)
}
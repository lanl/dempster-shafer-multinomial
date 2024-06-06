rdirichlet<- function(n,alpha) {
	r <- matrix(0,n,length(alpha))
	for(i in 1:length(alpha)) {
		r[,i] <- rgamma(n,alpha[i],1)
	}
	rowSum <- apply(r,1,sum)
	r <- apply(r,2,'/',rowSum)
	return(r)
}

# Data
#N <- c(125, 18, 20, 34)
# Smaller data
N <- c(25, 3, 4, 7)
M <- 1e6
eZ <- matrix(0,nrow=M,ncol=2)
for(i in 1:M) {
	while(TRUE) {
		z <- rdirichlet(1, c(1, N))
		z <- z[-1]
		lb <- max(4*z[1]-2, 4*z[4])
		ub <- min(1-4*z[2], 1-4*z[3])
		if(lb < ub) {
			break
		}
	}
	eZ[i,] <- c(lb, ub)
}

save(eZ, file='ds.RData')

probs <- 0:1000/1000
ds.cdf <- apply(eZ, 2, quantile, probs)
ds.cdf <- cbind(probs, ds.cdf)
write.table(ds.cdf, file='dsresults.txt', row.names=FALSE)
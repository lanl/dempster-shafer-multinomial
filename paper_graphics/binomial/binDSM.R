# Draw Z_L and Z_U for binomial p via rejection sampling.
# Hugely inefficent.
drawz_rs <- function(N1, N2, M) {
	Z <- matrix(0, nrow=M, ncol=2)
	for(i in 1:M) {
		U1 <- 1
		U2 <- 0
		while(max(U1) > min(U2)) {
			U1 <- runif(N1)
			U2 <- runif(N2)
		}
		Z[i,] <- c(max(U1), min(U2))
	}
	return(Z)
}

# Draw Z_L and Z_U as two betas
drawz_2b <- function(N1, N2, M) {
	Z <- matrix(0, nrow=M, ncol=2)
	for(i in 1:M) {
		Z[i,1] <- rbeta(1, N1, N2+1)
		#while(Z[i,1] > Z[i,2]) {
		#	Z[i,2] <- rbeta(1, N1+1, N2)
		#}
		# Next one should be the first order statistic on the reduce range
		Z[i,2] <- Z[i,1] + rbeta(1,1,N2) * (1-Z[i,1])
	}
	return(Z)
}
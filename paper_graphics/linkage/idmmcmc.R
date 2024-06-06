logpost <- function(phi, N, alpha) {
	if (phi < 0) {
		lp <- -Inf
	} else if (phi > 1) {
		lp <- -Inf
	} else {
		lp <- (alpha[1]+N[1]-1) * log((2+phi)/4)
		lp <- lp + (alpha[2]+alpha[3]+N[2]+N[3]-2) * log((1-phi)/4)
		lp <- lp + (alpha[4]+N[4]-1) * log(phi/4)
	}
	return(lp)
}

mnmcmc <- function (N, alpha, M, B, ss) {
	phi <- numeric(B+M)
	phi[1] <- .5

	for(i in 2:(B+M)) {
		# Just do something easy for now.
		phiCand <- phi[i-1] + rnorm(1, sd=ss)
		acc.ratio <- exp(logpost(phiCand, N, alpha) - logpost(phi[i-1], N, alpha))
		if (runif(1) < acc.ratio) {
			phi[i] <- phiCand
		} else {
			phi[i] <- phi[i-1]
		}
	}
	phi <- phi[(B+1):(B+M)]
	return(phi)
}

pemp <- function(q, x) {
	p <- sum(x < q) / length(x)
}

# Data
#N <- c(125, 18, 20, 34)
# Smaller data
N <- c(25, 3, 4, 7)

phiquant <- NULL
phicdf <- NULL
alpha <- NULL
acc.rate <- NULL
ix <- 0
s <- 1
grd <- (0:10)/10
M <- 1e6
B <- 1e4
probs <- 0:1000/1000
quants <- 0:1000/1000
quants <- matrix(quants, nrow=length(quants))
for(i in 1:length(grd)) {
	for(j in 1:length(grd)) {
		for(k in 1:length(grd)) {
			for(l in 1:length(grd)) {
				alphaCand <- grd[c(i,j,k,l)]
				if (sum(alphaCand) > s) {
					next
				} # if
				print(alphaCand)
				ix <- ix+1
				alpha[[ix]] <- alphaCand
				phi <- mnmcmc(N, alphaCand, M, B, .2)
				phiquant[[ix]] <- quantile(phi, probs)
				phicdf[[ix]] <- apply(quants, 1, pemp, phi)
				acc.rate[[ix]] <- sum(diff(phi) != 0) / (M-1)
			}
		}
	}
}
save(phiquant, phicdf, acc.rate, alpha, file='idm.RData')
#save.image(file='idm.RData')
#pdf('cdfs.pdf')
#plot(phiquant[[1]], probs, type='l')
#for(i in 2:length(alpha)) {
#	lines(phiquant[[i]], probs)
#}
#for(i in 1:length(alpha)) {
#	if (1 %in% alpha[[i]]) {
#		lines(phiquant[[i]], probs, col=2)
#	}
#}
#dev.off()
#
#phiquantm <- matrix(0, nrow=length(probs), ncol=length(alpha))
#for(i in 1:length(alpha)) {
#	phiquantm[,i] <- phiquant[[i]]
#}
#
#upper.cdf <- apply(phiquantm,1,min)
#lower.cdf <- apply(phiquantm,1,max)
#idm.cdf <- cbind(probs,lower.cdf, upper.cdf)
#write.table(idm.cdf, file='idmresults.txt', row.names=FALSE)
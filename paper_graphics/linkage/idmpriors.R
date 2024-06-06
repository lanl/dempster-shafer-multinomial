phi2theta <- function(phi) {
	theta <- c(2+phi, 1-phi, 1-phi, phi)/4
}

grd <- (0:1)/1
phi <-seq(.01,.99, by=.01)
dphi <- NULL
alpha <- NULL
s <- 1
ix <- 0
for(i in 1:length(grd)) {
	for(j in 1:length(grd)) {
		for(k in 1:length(grd)) {
			for(l in 1:length(grd)) {
				alphaCand <- grd[c(i,j,k,l)]
				if (sum(alphaCand) > s) {
					next
				}
				ix <- ix+1
				alpha[[ix]] <- alphaCand
				theta <- phi2theta(phi)
				theta <- matrix(theta,nrow=length(phi))
				alphaCand <- matrix(alphaCand, nrow=length(alphaCand), ncol=length(phi))
				alphaCand <- t(alphaCand)
				dphi[[ix]] <- apply(theta^(alphaCand-1), 1, prod)
			}
		}
	}
}

dphim <- matrix(0, nrow=length(phi), ncol=length(dphi))
for(i in 1:length(dphi)) {
	dphim[,i] <- dphi[[i]]
}
load('idm.RData')
load('ds.RData')
quants <- 0:1000/1000
quants <- matrix(quants, nrow=length(quants))

phicdfm <- matrix(0, nrow=length(quants), ncol=length(phicdf))
for(i in 1:length(phicdf)) {
	phicdfm[,i] <- phicdf[[i]]
}
cdf.idm <- cbind(quants, apply(phicdfm,1,min), apply(phicdfm,1,max))

pemp <- function(q, x) {
	p <- sum(x < q) / length(x)
}
cdf.ds <- cbind(quants, apply(quants, 1, pemp, eZ[,1]), apply(quants, 1, pemp, eZ[,2]))

postscript('linkagesmall.ps',width=8, height=3, horizontal=FALSE)

par(mfrow=c(1,3))
plot(cdf.idm[,1], cdf.idm[,3], type='l', col='red', lty=2, xlab=expression(phi), ylab=expression(F(phi)))
lines(cdf.idm[,1], cdf.idm[,2], col='blue', lty=2)
lines(cdf.ds[,1], cdf.ds[,2], col='red')
lines(cdf.ds[,1], cdf.ds[,3], col='blue')

plot(cdf.idm[,1], cdf.idm[,3]-cdf.idm[,2], type='l', col='black', lty=2, ylim=c(0,0.2), xlab=expression(phi[0]), ylab=expression(paste('r{',phi,'<',phi[0],'}',sep='')))
lines(cdf.ds[,1], cdf.ds[,2]-cdf.ds[,3], col='black')

plot(cdf.ds[,1], cdf.ds[,2]-cdf.ds[,3], col='black', type='l', ylim=c(0,0.2), xlab=expression(phi[0]), ylab=expression(paste('r{',phi,'=',phi[0],'}',sep='')))
lines(cdf.idm[,1], rep(0, length(cdf.idm[,1])), col='black', lty=2)

dev.off()
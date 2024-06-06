source('barycentric.R')

# General file info
setEPS()
u <- .6

## Key
postscript(file='plots/key.eps')
t1 <- c(1,0,0)
t2 <- c(0,1,0)
t3 <- c(0,0,1)
bplot(t1,t2,t3,type='n',
	  plab=expression(theta[1]),
	  qlab=expression(theta[2]),
	  rlab=expression(theta[3]),
	  axlabloc='middle', label.cex=3, lwd=1.5)

# grid lines
gl <- seq(0,1,by=.2)
for(i in gl) {
	blines(c(i,i),c(0,1-i),c(1-i,0),col='darkgray',lty=2, lwd=1.5)
	blines(c(0,1-i),c(i,i),c(1-i,0),col='darkgray',lty=2, lwd=1.5)
	blines(c(1-i,0),c(0,1-i),c(i,i),col='darkgray',lty=2, lwd=1.5)
}
baxis(1, at=gl, labels=gl, cex=1.5)
baxis(2, at=gl, labels=gl, cex=1.5)
baxis(3, at=gl, labels=gl, cex=1.5)

# Example point
P <- c(.5,.3,.2)
bpoints(P[1], P[2], P[3], pch=19, col='red')
blines(c(P[1],P[1]),c(1-P[1],P[2]),c(0,P[3]), col='red', lty=1)
blines(c(0,P[1]),c(P[2],P[2]),c(1-P[2],P[3]), col='red', lty=1)
blines(c(1-P[3],P[1]),c(0,P[2]),c(P[3],P[3]), col='red', lty=1)

legend(x='topright', legend=c('(.5, .3, .2)'), col=2, pch=19, cex=2)
dev.off()
##################################################################

## iDSM regions for a given u

# X=1 region
postscript(file='plots/idsm_x=1.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab='X=1',
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)

t1 <- c(1,u,u)
t2 <- c(0,0,1-u)
t3 <- c(0,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
dev.off()

# X=2 region
postscript(file='plots/idsm_x=2.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab='X=2',
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)

t1 <- c(u,u,0,0)
t2 <- c(1-u,0,u,1)
t3 <- c(0,1-u,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1.0, cex=3)
baxis(2, at=u, labels='U', srt=0, adj=0.5, cex=3)
baxis(3, at=1-u, label='1-U', srt=0, adj=0, cex=3)
dev.off()

# X=3 region
postscript(file='plots/idsm_x=3.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab='X=3',
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)

t1 <- c(0,u,0)
t2 <- c(0,0,u)
t3 <- c(1,1-u,1-u)
bpolygon(t1,t2,t3,col='red')
baxis(3, at=1-u, label='1-U', srt=0, adj=0, cex=3)
dev.off()

##################################################################

## pdsm regions for a given u

# X=1, pi=(1,2,3)
postscript(file='plots/pdsm_x=1_123.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=1, ', pi ,'=(1,2,3)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
	  
t1 <- c(1,u,u)
t2 <- c(0,0,1-u)
t3 <- c(0,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
dev.off()

# X=1, pi=(1,3,2)
postscript(file='plots/pdsm_x=1_132.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=1, ', pi ,'=(1,3,2)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
	  
t1 <- c(1,u,u)
t2 <- c(0,0,1-u)
t3 <- c(0,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
dev.off()

# X=1, pi=c(2,1,3)
postscript(file='plots/pdsm_x=1_213.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=1, ', pi ,'=(2,1,3)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
t1 <- c(1,u,0,1-u)
t2 <- c(0,0,u,u)
t3 <- c(0,1-u,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1, cex=3)
baxis(2, at=u, labels='U', srt=0, adj=.5, cex=3)
baxis(3, at=1-u, labels='1-U', srt=0, adj=0, cex=3)
dev.off()

# X=1, pi=c(3,1,2)
postscript(file='plots/pdsm_x=1_312.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=1, ', pi ,'=(3,1,2)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
t1 <- c(1,1-u,0,u)
t2 <- c(0,0,1-u,1-u)
t3 <- c(0,u,u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
baxis(2, at=1-u, labels='1-U', srt=0, adj=.5, cex=3)
baxis(3, at=u, labels='U', srt=0, adj=0, cex=3)
dev.off()

# X=1, pi=(2,3,1)
postscript(file='plots/pdsm_x=1_231.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=1, ', pi ,'=(2,3,1)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
	  
t1 <- c(1,1-u,1-u)
t2 <- c(0,0,u)
t3 <- c(0,u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1, cex=3)
dev.off()

# X=1, pi=(3,2,1)
postscript(file='plots/pdsm_x=1_321.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=1, ', pi ,'=(3,2,1)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
	  
t1 <- c(1,1-u,1-u)
t2 <- c(0,0,u)
t3 <- c(0,u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1, cex=3)
dev.off()

# X=2, pi=(1,2,3)
postscript(file='plots/pdsm_x=2_123.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=2, ', pi ,'=(1,2,3)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0, u,   u,   0)
t2 <- c(1, 1-u, 0,   u)
t3 <- c(0, 0,   1-u, 1-u)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1.0, cex=3)
baxis(2, at=u, labels='U', srt=0, adj=0.5, cex=3)
baxis(3, at=1-u, label='1-U', srt=0, adj=0, cex=3)
dev.off()

# X=2, pi=(3,2,1)
postscript(file='plots/pdsm_x=2_321.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=2, ', pi ,'=(3,2,1)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0, 1-u, 1-u, 0)
t2 <- c(1, u,   0,   1-u)
t3 <- c(0, 0,   u,   u)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1.0, cex=3)
baxis(2, at=1-u, labels='1-U', srt=0, adj=0.5, cex=3)
baxis(3, at=u, label='U', srt=0, adj=0, cex=3)
dev.off()

# X=2, pi=(2,1,3)
postscript(file='plots/pdsm_x=2_213.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=2, ', pi ,'=(2,1,3)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0, 0, 1-u)
t2 <- c(1, u, u)
t3 <- c(0, 1-u, 0)
bpolygon(t1,t2,t3,col='red')
baxis(2, at=u, labels='U', srt=0, adj=0.5, cex=3)
dev.off()

# X=2, pi=(2,3,1)
postscript(file='plots/pdsm_x=2_231.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=2, ', pi ,'=(2,3,1)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0, 0, 1-u)
t2 <- c(1, u, u)
t3 <- c(0, 1-u, 0)
bpolygon(t1,t2,t3,col='red')
baxis(2, at=u, labels='U', srt=0, adj=0.5, cex=3)
dev.off()

# X=2, pi=(1,3,2)
postscript(file='plots/pdsm_x=2_132.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=2, ', pi ,'=(1,3,2)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0, 0, u)
t2 <- c(1, 1-u, 1-u)
t3 <- c(0, u, 0)
bpolygon(t1,t2,t3,col='red')
baxis(2, at=1-u, labels='1-U', srt=0, adj=0.5, cex=3)
dev.off()

# X=2, pi=(3,1,2)
postscript(file='plots/pdsm_x=2_312.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=2, ', pi ,'=(3,1,2)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0, 0, u)
t2 <- c(1, 1-u, 1-u)
t3 <- c(0, u, 0)
bpolygon(t1,t2,t3,col='red')
baxis(2, at=1-u, labels='1-U', srt=0, adj=0.5, cex=3)
dev.off()

# X=3, pi=c(3,1,2)
postscript(file='plots/pdsm_x=3_312.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=3, ', pi ,'=(3,1,2)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0,0,1-u)
t2 <- c(0,1-u,0)
t3 <- c(1,u,u)
bpolygon(t1,t2,t3,col='red')
baxis(3,at=u,labels='U',srt=0, adj=0, cex=3)
dev.off()

# X=3, pi=c(3,2,1)
postscript(file='plots/pdsm_x=3_321.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=3, ', pi ,'=(3,2,1)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0,0,1-u)
t2 <- c(0,1-u,0)
t3 <- c(1,u,u)
bpolygon(t1,t2,t3,col='red')
baxis(3,at=u,labels='U',srt=0, adj=0, cex=3)
dev.off()

# X=3, pi=c(1,2,3)
postscript(file='plots/pdsm_x=3_123.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=3, ', pi ,'=(1,2,3)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0,0,u)
t2 <- c(0,u,0)
t3 <- c(1,1-u,1-u)
bpolygon(t1,t2,t3,col='red')
baxis(3,at=1-u,labels='1-U',srt=0, adj=0, cex=3)
dev.off()

# X=3, pi=c(2,1,3)
postscript(file='plots/pdsm_x=3_213.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=3, ', pi ,'=(2,1,3)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0,0,u)
t2 <- c(0,u,0)
t3 <- c(1,1-u,1-u)
bpolygon(t1,t2,t3,col='red')
baxis(3,at=1-u,labels='1-U',srt=0, adj=0, cex=3)
dev.off()

# X=3, pi=(1,3,2)
postscript(file='plots/pdsm_x=3_132.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=3, ', pi ,'=(1,3,2)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0,0,u,u)
t2 <- c(0,1-u,1-u,0)
t3 <- c(1,u,0,1-u)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
baxis(2, at=1-u, label='1-U', srt=0, adj=.5, cex=3)
baxis(3,at=1-u,labels='1-U',srt=0, adj=0, cex=3)
dev.off()

# X=3, pi=(2,3,1)
postscript(file='plots/pdsm_x=3_231.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(paste('X=3, ', pi ,'=(2,3,1)',sep='')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)	  
t1 <- c(0,0,1-u,1-u)
t2 <- c(0,u,u,0)
t3 <- c(1,1-u,0,u)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1, cex=3)
baxis(2, at=u, label='U', srt=0, adj=.5, cex=3)
baxis(3,at=u,labels='U',srt=0, adj=0, cex=3)
dev.off()

##################################################################

## condition pDSM regions for a given u and X=1

# X=1, pi=(1,2,3) (1,3,2)
postscript(file='plots/cond_pdsm_x=1_123_132.eps')
bplot(1/3,1/3,1/3,type='n',
	  #plab=expression(paste(pi %in%, '{(1,2,3); (1,3,2)}',sep='')),
	  plab=expression(pi %in% group('{','(1,2,3); (1,3,2)', '}')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
	  
t1 <- c(1,u,u)
t2 <- c(0,0,1-u)
t3 <- c(0,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
dev.off()

# X=1, pi=c(2,1,3)
postscript(file='plots/cond_pdsm_x=1_213.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(pi %in% group('{','(2,1,3)', '}')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
t1 <- c(1,u,0,1-u)
t2 <- c(0,0,u,u)
t3 <- c(0,1-u,1-u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1, cex=3)
baxis(2, at=u, labels='U', srt=0, adj=.5, cex=3)
baxis(3, at=1-u, labels='1-U', srt=0, adj=0, cex=3)
dev.off()

# X=1, pi=c(3,1,2)
postscript(file='plots/cond_pdsm_x=1_312.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(pi %in% group('{','(3,1,2)', '}')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
t1 <- c(1,1-u,0,u)
t2 <- c(0,0,1-u,1-u)
t3 <- c(0,u,u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
baxis(2, at=1-u, labels='1-U', srt=0, adj=.5, cex=3)
baxis(3, at=u, labels='U', srt=0, adj=0, cex=3)
dev.off()

# X=1, pi=(2,3,1) (3,2,1)
postscript(file='plots/cond_pdsm_x=1_231_321.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab=expression(pi %in% group('{','(2,3,1); (3,2,1)', '}')),
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)
	  
t1 <- c(1,1-u,1-u)
t2 <- c(0,0,u)
t3 <- c(0,u,0)
bpolygon(t1,t2,t3,col='red')
baxis(1, at=1-u, labels='1-U', srt=0, adj=1, cex=3)
dev.off()

##################################################################

## dDSM set
postscript(file='plots/ddsm_x=1.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab='X=1',
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)

bpolygon(c(1,u,u), c(0,0,1-u), c(0,1-u,0),col='red')
#bpolygon(c(0,1-u,0), c(1,u,u), c(0,0,1-u),col='red')
#bpolygon(c(0,1-u,0), c(0,0,1-u), c(1,u,u),col='red')
baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
#baxis(2, at=u, labels='U', srt=0, adj=.5, cex=3)
#baxis(3, at=u, labels='U', srt=0, adj=0, cex=3)
dev.off()

postscript(file='plots/ddsm_x=2.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab='X=2',
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)

#bpolygon(c(1,u,u), c(0,0,1-u), c(0,1-u,0),col='red')
bpolygon(c(0,1-u,0), c(1,u,u), c(0,0,1-u),col='red')
#bpolygon(c(0,1-u,0), c(0,0,1-u), c(1,u,u),col='red')
#baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
baxis(2, at=u, labels='U', srt=0, adj=.5, cex=3)
#baxis(3, at=u, labels='U', srt=0, adj=0, cex=3)
dev.off()

postscript(file='plots/ddsm_x=3.eps')
bplot(1/3,1/3,1/3,type='n',
	  plab='X=3',
	  qlab='',
	  rlab='',
	  axlabloc='corner', label.cex=3, lwd=1.5)

#bpolygon(c(1,u,u), c(0,0,1-u), c(0,1-u,0),col='red')
#bpolygon(c(0,1-u,0), c(1,u,u), c(0,0,1-u),col='red')
bpolygon(c(0,1-u,0), c(0,0,1-u), c(1,u,u),col='red')
#baxis(1, at=u, labels='U', srt=0, adj=1, cex=3)
#baxis(2, at=u, labels='U', srt=0, adj=.5, cex=3)
baxis(3, at=u, labels='U', srt=0, adj=0, cex=3)
dev.off()
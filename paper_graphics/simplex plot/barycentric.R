## points in bary coordinates
bpoints <- function(p,q,r,...){
  ## p+q+r should be 1
  s <- p+q+r
  if(mean(s) != 1) {
  	print('Rescaling something!')
  }
  h <- sqrt(3)/2
  y <- (p/s-.5)*h
  x <- (r-q)/2/s
  points(x,y,...)
}

blines <- function(...,type='l'){
  bpoints(...,type=type)
}

bpolygon <- function(p,q,r,...){
  ## p+q+r should be 1
  s <- p+q+r
  if(mean(s) != 1) {
  	print('Rescaling something!')
  }
  h <- sqrt(3)/2
  y <- (p/s-.5)*h
  x <- (r-q)/2/s
  polygon(x,y,...)
}

bplot <- function(p,q,r, type='p',
                  plab=deparse(substitute(p)),
                  qlab=deparse(substitute(q)),
                  rlab=deparse(substitute(r)),
                  axlabloc='corner',label.cex=1.0,
                  ...){
  par(mar=c(3,4,4,4), pty='s')
  h <- sqrt(3)/2
  plot(c(0,1,-1,0)/2, h*c(1,-1,-1,1)/2,
       type='l', pty='s',
       bty='n', axes=FALSE,
       xlab='', ylab='',
       ...)
  f <- 1.27
  if(grepl('corner',axlabloc)) {
	  text(f*c(0,-.5,.5), f*c(h/2,-h/2,-h/2),
    	   c(plab,qlab,rlab), xpd=TRUE,cex=label.cex)
  }
  if(grepl('middle',axlabloc)) {
  	text(f*c(-.5*3/5,0,.5*3/5), f*c(0,-h/2,0), c(plab, qlab, rlab), xpd=TRUE,cex=label.cex)
  }
  bpoints(p,q,r, type=type, ...)
}

baxis <- function(side, at=NULL, labels=TRUE, tick=TRUE, line=2, srt=NULL, ...){
  ## side 1,2,3 = left, bottom, right
  h <- sqrt(3)/2
  dx <- sin(pi/3)
  dy <- cos(pi/3)
  # Fixed an error here that draw the theta2 axis backwards -- EARL 
  #x <- switch(side, c(-1,0), c(-1,+1), c(0,1))/2
  x <- switch(side, c(-1,0), c(1,-1), c(0,1))/2
  y <- switch(side, c(-1,1), c(-1,-1), c(1,-1))*h/2
  segments(x[1],y[1],x[2],y[2], ...)
  if (is.null(at)) at <- pretty(0:1)
  xt <- x[1] + at*diff(x)
  yt <- y[1] + at*diff(y)
  if (tick){
    delx <- .02*switch(side, -dx, +0, +dx)
    dely <- .02*switch(side, +dy, -1, +dy)
    segments(xt, yt, xt+delx, yt+dely)
  }
  if (length(labels)){
    if (is.logical(labels)) labels <- if (labels) at else ''
    lh <- strheight('A')
    delx <- (line)*lh*switch(side, -dx, +0, +dx)
    dely <- (line)*lh*switch(side, +dy, -1, +dy)
    if (is.null(srt)) srt <- switch(side, 60, 0, -60)
    text(xt+delx,yt+dely,labels, srt=srt, xpd=TRUE, ...)
  }
}

rdirichlet<- function(n,alpha) {
  r <- matrix(0,n,length(alpha))
  for(i in 1:length(alpha)) {
    r[,i] <- rgamma(n,alpha[i],1)
  }
  rowSum <- apply(r,1,sum)
  r <- apply(r,2,'/',rowSum)
  return(r)
}
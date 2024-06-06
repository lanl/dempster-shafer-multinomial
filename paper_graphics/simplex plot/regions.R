## points in bary coordinates
bpoints <- function(p,q,r,...){
  ## p+q+r should be 1
  s <- p+q+r
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
  h <- sqrt(3)/2
  y <- (p/s-.5)*h
  x <- (r-q)/2/s
  polygon(x,y,...)
}

bplot <- function(p,q,r, type='p',
                  plab=deparse(substitute(p)),
                  qlab=deparse(substitute(q)),
                  rlab=deparse(substitute(r)),
                  ...){
  par(mar=c(3,4,4,4), pty='s')
  h <- sqrt(3)/2
  plot(c(0,1,-1,0)/2, h*c(1,-1,-1,1)/2,
       type='l', pty='s',
       bty='n', axes=FALSE,
       xlab='', ylab='',
       ...)
  f <- 1.27
  text(f*c(0,-.5,.5), f*c(h/2,-h/2,-h/2),
       c(plab,qlab,rlab), xpd=TRUE, ypd=TRUE)
  bpoints(p,q,r, type=type, ...)
}

baxis <- function(side, at=NULL, labels=TRUE, tick=TRUE, line=2, srt=NULL, ...){
  ## side 1,2,3 = left, bottom, right
  h <- sqrt(3)/2
  dx <- sin(pi/3)
  dy <- cos(pi/3)
  x <- switch(side, c(-1,0), c(-1,+1), c(0,1))/2
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
    text(xt+delx,yt+dely,labels, srt=srt, xpd=TRUE, ypd=TRUE, ...)
  }
}


## tests
plab='(1, 0, 0)'
qlab='(0, 1, 0)'
rlab='(0,0,1)'
p <- c(1,0,0,1,0,1,1)
q <- c(0,1,0,1,1,0,1)
r <- c(0,0,1,0,1,1,1)

bplot(p,q,r, pch=c('P','Q','R','r','p','q','O'), plab=plab,qlab=qlab,rlab=rlab)
q <- seq(0,1,length=51)
blines(0,q,1-q, col='red')
blines(1,q,1-q, col='red')

baxis(1)
baxis(2)
baxis(3)

## figs for paper
plab='(1,0,0)'
qlab='(0,1,0)  '
rlab='  (0,0,1)'

postscript(file='poly1.ps', width=3.5, height=3.5)
u <- 3/4
p <- c(1,u,u)
q <- c(0,0,1-u)
r <- 1-p-q
bplot(1,1,1, type='n', plab=plab,qlab=qlab,rlab=rlab)
bpolygon(p,q,r, col='red',border=TRUE,lwd=2)
baxis(1, at=u, labels='(U,1-U,0)', srt=0, adj=1)
baxis(3, at=1-u, labels='(U,0,1-U)', srt=0, adj=0)
mtext(expression(pi(1,2,3) == list(1,2,3, or, 1,3,2)), line=2)
dev.off()

postscript(file='poly2.ps', width=3.5, height=3.5)
p <- c(1,1-u,1-u)
q <- c(0,0,u)
r <- 1-p-q
bplot(1,1,1, type='n', plab=plab,qlab=qlab,rlab=rlab)
bpolygon(p,q,r, col='red',border=TRUE,lwd=2)
baxis(1, at=1-u, labels='(1-U,U,0)', srt=0, adj=1)
baxis(3, at=u, labels='(1-U,0,U)', srt=0, adj=0)
mtext(expression(pi(1,2,3) == list(3,2,1, or, 2,3,1)), line=2)
dev.off()

postscript(file='poly3.ps', width=3.5, height=3.5)
p <- c(1,1-u,0,u)
q <- c(0,0,1-u,1-u)
r <- 1-p-q
bplot(1,1,1, type='n', plab=plab,qlab=qlab,rlab=rlab)
bpolygon(p,q,r, col='red',border=TRUE,lwd=2)
baxis(1, at=u, labels='(U,1-U,0)', srt=0, adj=1)
baxis(2, at=u, labels='(0,1-U,U)', srt=0)
baxis(3, at=u, labels='(1-U,0,U)', srt=0, adj=0)
mtext(expression(pi(1,2,3) == list(2,1,3)), line=2)
dev.off()

postscript(file='poly4.ps', width=3.5, height=3.5)
p <- c(1,u,0,1-u)
q <- c(0,0,u,u)
r <- 1-p-q
bplot(1,1,1, type='n', plab=plab,qlab=qlab,rlab=rlab)
bpolygon(p,q,r, col='red',border=TRUE,lwd=2)
baxis(1, at=1-u, labels='(1-U,U,0)', srt=0, adj=1)
baxis(2, at=1-u, labels='(0,U,1-U)', srt=0)
baxis(3, at=1-u, labels='(U,0,1-U)', srt=0, adj=0)
mtext(expression(pi(1,2,3) == list(3,1,2)), line=2)
dev.off()

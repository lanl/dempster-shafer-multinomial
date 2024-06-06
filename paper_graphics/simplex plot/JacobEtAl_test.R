rm(list=ls())
source('barycentric.R')

z <-rdirichlet(10000, c(1,1,1))

t2 <- z[,2] / (1-z[,1])
t3 <- z[,3] / (1-z[,1])
hist(t2)
hist(t3)

hist(t2+t3)
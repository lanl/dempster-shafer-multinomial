## This script reproduces the results for the linkage model.

################################################################
# This code is derivative of the linkage model experiment from 
# https://github.com/pierrejacob/dempsterpolytope
# by Pierre Jacob et al.
# 
# The original code was released under
# GNU GENERAL PUBLIC LICENSE
# Version 3, 29 June 2007
# 
# Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
# 
# As is required by the GNU 3 license, this derivative work is also released
# under GNU 3.
################################################################

rm(list = ls())
set.seed(2)
library(dempsterpolytope)
library(doParallel)
library(doRNG)
library(abind)
registerDoParallel(cores = detectCores()-2)
graphsettings <- set_custom_theme()
theme_set(ggthemes::theme_tufte(ticks = TRUE))
theme_update(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), 
             axis.title.x = element_text(size = 25, margin = margin(20, 0, 0, 0), hjust = 1), 
             axis.title.y = element_text(size = 25, angle = 90, margin = margin(0, 20, 0, 0), vjust = 1), 
             legend.text = element_text(size = 20), 
             legend.title = element_text(size = 20), title = element_text(size = 30), 
             strip.text = element_text(size = 25), strip.background = element_rect(fill = "white"), 
             legend.position = "bottom")


# define A and b such that theta = A phi + b where phi is in the interval (0,1)
A <- c(1/4, -1/4, -1/4, 1/4)
b <- c(1/2, 1/4, 1/4, 0)

# data 
K <- 4
# counts = c(25, 3, 4, 7)
counts = c(25, 6, 2, 7)

# The following function is internal to the package, but has not been exported
# to the namespace.  I'm just going to source it from it's original file.
# source("../../R/sample_meeting_times.R")

## 1. Dirichlet DSM model.

# function to sample n times from Dirichlet(alpha) distribution
# (taken verbatim from the 'gtools' package)
rdirichlet <- function (n, alpha){
  l <- length(alpha)
  x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  sm <- x %*% rep(1, l)
  x/as.vector(sm)
}

# ## function to generate samples from the Dirichlet DSM (DDSM) model 
sample_ddsm <- function(x, nsim = 1e4, batch = nsim*100){
  # Rejection algorithm for Dirichlet DSM
  # x: 4 dimensional data vector
  # nsim: total samples wanted
  # batch: sample size per batch

  sample_phi <- function(n, data = x){
    z = rdirichlet(n, alpha = c(data, 1))
    s_ = cbind(4*pmax(z[, 1]-1/2, z[, 4]), 1 - 4*pmax(z[, 2], z[, 3]))
    return(s_[s_[,2]>s_[,1],])
  }

  s_ <- sample_phi(batch, data = x)
  ns = nrow(s_)
  if (ns == 0){
    stop('Acceptance rate too low, change batch size')
  }
  count = 0
  while(ns < nsim){
    s_ <- rbind(s_, sample_phi(batch, data = x))
    ns <- nrow(s_)
    count = count + 1
  }

  return(s_[1:nsim,])
}

sample_phi <- function(n, data = x){
  z = rdirichlet(n, alpha = c(data, 1))
  s_ = cbind(4*pmax(z[, 1]-1/2, z[, 4]), 1 - 4*pmax(z[, 2], z[, 3]))
  return(s_[s_[,2]>s_[,1],])
}

n_samples = 112500
dDSM_times = c()
dDSM_rejection_rates = c()
for(exp_num in 1:30){
  print(paste("Running the Dirichlet DSM model for the", exp_num, "time"))
  temp_time = proc.time()

  s_ = sample_phi(data = counts, n = n_samples)

  final_time = proc.time() - temp_time

  dDSM_times = c(dDSM_times, final_time[1])
  dDSM_rejection_rates = c(dDSM_rejection_rates, nrow(s_)/n_samples)
}

summary(dDSM_times)
sd(dDSM_times)
summary(dDSM_rejection_rates)
sd(dDSM_rejection_rates)


## draw 1e4 feasible sets
phi_intervals_ddsm = sample_ddsm(x = counts, nsim = 112500)
## show empirical lower/upper CDF
plot(ecdf(phi_intervals_ddsm[, 1]), col = 'red',
     main = 'Dirichlet DSM upper and lower CDF, nsim = 1e4',
     xlab = 'phi0', ylab = 'P(phi < phi0)', xlim = c(0, 1))
plot(ecdf(phi_intervals_ddsm[, 2]), col = 'blue', add = T)

## let's choose a burn-in of 50, based on the above plot
burnin <- 50
nchains <- 250
niterations <- 500

sDSM_times = c()
sDSM_rejection_rates = c()
for(exp_num in 1:30){
  print(paste("Running the Simplex DSM model for the", exp_num, "time"))
  temp_time = proc.time()
  burnin <- 50
  nchains <- 250
  niterations <- 500

  acomb <- function(...) abind(..., along=1)
  etas <- foreach(irep = 1:nchains, .combine = 'acomb') %dorng% {
    samples_gibbs <- dempsterpolytope::gibbs_sampler(niterations, counts)
    samples_gibbs$etas[(burnin+1):niterations,,]
  }
  dim(etas)
  
  ##
  ## define segment constraints in dimension K-1
  constr <- matrix(0, nrow = 2, ncol = K-1)
  rhs <- c(-b[1], A[1] + b[1])
  constr[1,1] <- -1
  constr[2,1] <- 1
  dir <- c("<=", "<=")
  for (j in 2:(K-1)){
    row_constr_ <- rep(0, K-1)
    row_constr_[1] <- -A[j]/A[1]
    row_constr_[j] <- 1
    rhs <- c(rhs, b[j] - b[1] * A[j] / A[1], -(b[j] - b[1] * A[j] / A[1]))
    constr <- rbind(constr, row_constr_, -row_constr_, deparse.level = 0)
    dir <- c(dir, "<=", "<=")
  }
  segment_constr <- list(constr = constr, dir = dir, rhs = rhs)
  
  intersects_with_segment <- function(eta, segment_constr){
    K_ <- dim(eta)[1]
    categories <- 1:K_
    # # the constraints are on the first K-1 coordinates
    # # the first ones say that the feasible set is within the simplex
    constrA <- matrix(rep(1, K_-1), ncol = K_-1)
    constrA <- rbind(constrA, diag(-1, K_-1, K_-1))
    constrb <- c(1, rep(0, K_-1))
    # then the extra constraints come from etas
    for (d in categories){
      for (j in setdiff(categories, d)){
        if (is.finite(eta[d,j])){
          # cccc (wA wB wC ... )' = 0
          ccc <- rep(0, K_)
          ccc[d] <- -eta[d, j]
          ccc[j] <- 1
          cc <- ccc - ccc[K_]
          constrb <- c(constrb, -ccc[K_])
          constrA <- rbind(constrA, matrix(cc[1:(K_-1)], nrow = 1))
        } else {
          # if eta is infinite, no constraint
        }
      }
    }
    polytope_constr_ <- list(constr = constrA, rhs = constrb, dir = rep("<=", nrow(constrA)))
    test_constr <- segment_constr
    test_constr$constr <- rbind(test_constr$constr, polytope_constr_$constr)
    test_constr$rhs <- c(test_constr$rhs, polytope_constr_$rhs)
    test_constr$dir <- c(test_constr$dir, polytope_constr_$dir)
    ## make H representation (H for ?)
    h <- rcdd::makeH(test_constr$constr, test_constr$rhs)
    ## try to find V representation (V for Vendetta or Vertex?)
    v <- rcdd::q2d(rcdd::scdd(rcdd::d2q(h))$output)
    intersects <- (dim(v)[1] != 0)
    phis_ <- c(NA, NA)
    if (intersects){
      seg <- v[,-c(1,2)]
      seg <- cbind(seg, 1-rowSums(seg))
      phis_ <- apply(seg, 1, function(v) (v[1] - b[1]) / A[1])
      phis_ <- (sort(phis_))
    }
    phis_
  }
  
  intersections_ <- foreach(iter = 1:dim(etas)[1], .combine = rbind) %dopar% {
    intersects_with_segment(etas[iter,,], segment_constr)
  }
  
  dim(intersections_)
  ## acceptance rate
  print(mean(!is.na(intersections_[,1])))
 
  
  final_time = proc.time() - temp_time
  
  sDSM_times = c(sDSM_times, final_time[1])
  sDSM_rejection_rates = c(sDSM_rejection_rates, mean(!is.na(intersections_[,1])))
  
  ## retain feasible sets that intersect with segment constraint 
  intersections_ <- intersections_[!is.na(intersections_[,1]),]
  # if(exp_num==2) browser()
}

summary(sDSM_times)
summary(sDSM_rejection_rates)




# Kentaro's independence experiment made into a regular R script for parallelization.
## compiled by: Alexander C. Murph

#devtools::install_github("pierrejacob/dempsterpolytope@a67326c")
library(ggplot2)
library(tidyverse)
library(foreach)
library(pracma)
library(parallel)
library(dbplyr)
library(dempsterpolytope)
library(CVXR)
library(quadprog)
setwd("~/ds-work")
source("kentaro_functions.R")

sim_num = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if(is.na(sim_num)) sim_num = 1

## Dirichlet Inference Functions
set.seed(sim_num)


n.ds1=20
n.unif1=30
burn_in=10

sim_category   = (sim_num-1)%%4 + 1
divisions_list = c(2,3,6,8)


divisions1 = c(2,3,6,8)
#max.division1  = Reduce(Lcm,divisions1) 
max.division1 = divisions_list[sim_category]

x.unif1=rbeta(n.unif1,1,1)
y.unif1=rbeta(n.unif1,1,1)

x.group1=factor((x.unif1 %/% (1/max.division1))+1, levels = 1:max.division1)
y.group1=factor((y.unif1 %/% (1/max.division1))+1, levels = 1:max.division1)
u.counts1=table(tibble(x.group=x.group1,y.group=y.group1))

#Next we generate polytopes based on Jacob and Gong (2021) and perform a similar hypothesis test.
ptm <- proc.time()
ds_test1 = function(counts , niterations,burn_in, plot  = FALSE, time = F){
  
  result = list()
  gibbs_results <- dempsterpolytope::gibbs_sampler(niterations, counts)
  eta <- gibbs_results$etas[niterations,,]
  eta_converted <- dempsterpolytope::etas2cvxpolytope(eta)
  
  
  #Dist from data to center
  center = counts
  center = (center + (1/length(center)))/ (sum(counts) + 1)
  l2.data = sqrt(sum((center - rep(1/length(center)))^2))
  
  
  center_b = center[- length(center)] 
  
  cvx_cord = list()
  l2.max = c()
  constr_list = list()
  l2.min = c()
  
  for (iter in burn_in:niterations){
    cvx <- dempsterpolytope::etas2cvxpolytope(gibbs_results$etas[iter,,])
    cvx_cord[[iter - burn_in + 1]] = cvx$vertices_barcoord
    constr_list[[iter - burn_in + 1]] = cvx$constr
    
    #distance from max vertex to data
    current_max =  (cvx$vertices_barcoord -   matrix(rep(1, dim(cvx$vertices_barcoord )[1]), ncol = 1 )%*% center)^2
    current_max = max(sqrt(apply(current_max, 1,sum)))
    l2.max = c(l2.max, current_max)
    
    
    #distance from min to data
    x_hat = CVXR::Variable(length(center_b))
    objective = CVXR::Minimize(sum(( x_hat - center_b)^2  ))
    constraint1 = cvx$constr$constr %*% x_hat <= cvx$constr$rhs
    problem <- CVXR::Problem(objective, constraints = list(constraint1))
    opt_result <- solve(problem, verbose = F, solver = 'ECOS')
    l2.min = c(l2.min,sqrt(abs(opt_result$value)))
    
    
  }
  
  if(plot == T){
    hist(l2.max)
    abline(v = l2.data, col ='red')
    
    hist(l2.min)
    abline(v = l2.data, col ='red')
  }
  
  result$upper.pvalue = mean(l2.data <= l2.max)
  result$lower.pvalue = mean(l2.data <= l2.min)
  
  return(result)
}

counts = as.vector(u.counts1)
niterations = n.unif1 + burn_in
resul = ds_test1(counts, niterations, burn_in)
resul$upper.pvalue
resul$lower.pvalue
final_timeit = proc.time() - ptm
print(paste("runtime for sDSM was,", final_timeit[1]))

temp_data = data.frame(runtime = final_timeit[1], model = 'sDSM', divisions = max.division1, sim_num = sim_num)
write.csv(temp_data, file = paste('runtime_data/runtime_sDSM', sim_num, max.division1, '.csv', sep = '_'))



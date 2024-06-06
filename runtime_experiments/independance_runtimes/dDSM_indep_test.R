# Analysis of Dempster-Shafer Code
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
#registerDoParallel(cores=4)
set.seed(1968)
setwd("~/ds-work")

source("kentaro_functions.R")

sim_num = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if(is.na(sim_num)) sim_num = 1

## Dirichlet Inference Functions
set.seed(sim_num)

sim_category   = (sim_num-1)%%4 + 1
divisions1 = c(2,3,6,8)
#divisions1     = c(1:divisions_list[sim_category])
max.division1  = Reduce(Lcm,divisions1)

## Dirichlet Inference Functions
ptm <- proc.time()

n.ds1=20L #number of DS polytopes
n.unif1=30L #sample size
burn_in = 10L

x.unif1=rbeta(n.unif1,1,1)
y.unif1=rbeta(n.unif1,1,1)

x.group1=factor((x.unif1 %/% (1/max.division1))+1, levels = 1:max.division1)
y.group1=factor((y.unif1 %/% (1/max.division1))+1, levels = 1:max.division1)

u.counts1=table(tibble(x.group=x.group1,y.group=y.group1))

#First generating information needed for DS based on the unpublished work of Lawrence et al. (Liu) 
ds.particles1=ds.generate.particles(u.counts1,divisions1,n.ds1)

# Next define method that will be used for computing the particles aka polytopes and centers. 
# These functions are not to be used by itself but are a building block for future. Their 
# output is centered and scaled using the mean of $b^{-2 l}$ and standard deviation of $b^{-l}$, 
# where $l$ is the level. (Recall that $b^{2 l}$ is the number of cells. 
# Hellinger distance is only centered and not scaled because square root acts as a variance stabilizing transformation.)


ds.pval.particles1=ds.value.particles(ds.particles1,sim_category,weakening=F,Hellinger=F)
ds.pval.particles1$center
ds.pval.particles1$particles[[2]]
ds.pvalue(ds.pval.particles1)

final_timeit = proc.time() - ptm


print(paste("runtime for dDSM was,", final_timeit[1]))

temp_data = data.frame(runtime = final_timeit[1], model = 'dDSM', divisions = divisions1[sim_category], sim_num = sim_num)
write.csv(temp_data, file = paste('runtime_data/runtime_dDSM', sim_num, divisions1[sim_category], '.csv', sep = '_'))




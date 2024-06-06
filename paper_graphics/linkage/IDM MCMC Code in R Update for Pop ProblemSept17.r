#IDM Code with MCMC-Metropolis Hastings for Population Problem
#September 17, 2008
#Alyson's Updated code with Kari's modifications from June 12, 2007

###########################PROGRAMMING NOTES###########################
#FUNCTIONS
#idm
       ##Imprecise Dirichlet Model Function
       ###calls sampp1, sampp2, sampp3
       #sampp1
##Sample from p1
       ###########calls mh function
       #sampp2
       ##Sample from p2
       ###########calls mh function
       #sampp3
       ##Sample from p3
       ###########calls mh function
#mh
       ##Metropolis Hastings Algorithm
       ###Choose to keep current value of p or new value of p
#####################################################################
#VARIABLES AND PARAMETERS
#
#niter: number of iterations
#
#s: learning parameter which can be set at 1 or 2 for our purposes
#
#t1: the mean of the distribution for category 1 (unknown)
#t2: the mean of the distribution for category 2 (unknown)
#t3: the mean of the distribution for category 3 (unknown)
#(The fourth t is derived from 1 minus the sum of the other 
#three t’s)
########Use a grid of t’s
#
########Use MCMC for inference on probabilities
#p1: the first of four probabilities
#p2: the second of four probabilities
#p3: the third of four probabilities
#(The fourth probability is derived from 1 minus the sum of the other 
#three probabilities)
#
#p1n: the alternative candidate for the first probability sampled from #######normal in sampp1
#p2n: the alternative candidate for the second probability sampled from ######normal in sampp2
#p3n: the alternative candidate for the third probability sampled from ######normal in sampp3
#
#cur: current phi
#cand: candidate phi
#
#oldPost: Old Posterior (depending on old candidate of phi)
#newPost: New Posterior (depending on new candidate of phi)
#
#randUnif: randomly generated valued from uniform
#ratioPost: difference between posteriors 
#
######################################################################
######################PROGRAM STARTS HERE#############################
######################################################################
RetrieveT<-function(numIter,startPhi){
Get_ts<-read.table("C:\\Documents and Settings\\186978\\Desktop\\IDM for Chuanhai\\Gimme_t_by10_Sept17.dat", header=F, row.names=NULL)
#This data file (Gimme_t_by10_Sept17.dat) is at the roughest granularity for the grid of t's with the increment of 0.1
#There are two more data files for greater resolution on the grid 0.5 and 0.01:
#Gimme_t_by5_Sept17 increments by 0.5
#Gimme_t_by1_Sept16 increments by 0.01

if (startPhi > 1) return("phi must be less than 1 and greater than 0") 
if (startPhi < 0) {return("phi must be less than 1 and greater than 0")}else{
t1s<- Get_ts[,2]
t2s<- Get_ts[,3]
t3s<- Get_ts[,4]
t4s<- Get_ts[,5]
phi<-idm(startPhi,numIter,2,t1s,t2s,t3s)   
print(phi)
max.phi<-max(phi)
min.phi<-min(phi)
MaxMin<-cbind(max.phi,min.phi)
print(MaxMin)}#end If
}#End RetrieveT Function

#############################################################
idm <- function(startPhi,numIter,s,t1,t2,t3) {
sumT = t1 + t2 + t3
#if (sumT > 1) return("Ti must sum to 1") 

phi<- rep(0,numIter) #initializing p vectors 
phi[1] <- startPhi #Hard-coded starter value

for (i in 2:numIter) { 
phi[i] <- samp.phi (phi[i-1],s,t1,t2,t3) 
}#Assign probabilities to individual cells of the vector obtained from
#the relevant functions  

return(phi)
}#End IDM function

###############################################################
samp.phi <- function(phi,s,t1,t2,t3) { 
phi.cand <- rnorm(1,phi,0.1) #random generation from a normal distrib
       if (phi.cand < 0) return(phi)
	if (phi.cand > 1) return(phi)
oldPost <- ((((2+phi)/4)^(s*t1+124))* (((1-phi)/4)^((s*t2)+(s*t3)+36))* (((phi)/4)^(s*(1-t1-t2-t3)+33)))

newPost <- ((((2+phi.cand)/4)^(s*t1+124))* (((1-phi.cand)/4)^((s*t2)+(s*t3)+36))* (((phi.cand)/4)^(s*(1-t1-t2-t3)+33)))

return(mh(phi,oldPost,phi.cand,newPost)) 
}#End sampp1 function

###############################################################
mh <- function(cur,oldPost,cand,newPost) { 
if (newPost >= oldPost) return(cand)else{randUnif <- runif(1)
       #random generation from a uniform distribution 
       ratioPost <- newPost/oldPost}#End else
       			#ratio of new Posterior to old posterior
if (randUnif <= ratioPost){return(cand)}else{
       return(cur)}#End Else 
}#end mh function
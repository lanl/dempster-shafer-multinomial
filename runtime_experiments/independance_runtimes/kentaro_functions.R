# helper functions for kentaro_script.R
## compiled by: Alexander C. Murph

ds.value.sum = function(ds.particle, location.x,location.y, divisions,i.division, weakening = T, Hellinger = F){
  
  max.division=dim(ds.particle$pre.v)[1]
  division=divisions[i.division]
  
  pre.v0=ds.particle$pre.v0[i.division*weakening+1]
  pre.sum=sum(ds.particle$pre.v)+pre.v0
  
  v=ds.particle$pre.v/pre.sum
  v0=pre.v0/pre.sum
  
  r.division=max.division %/% division
  
  index.x=sapply(1:max.division,
                 function(i){ifelse((i-1)%/%(r.division) == (location.x-1)%/%(r.division),1,0)})
  
  index.y=sapply(1:max.division,
                 function(i){ifelse((i-1)%/%(r.division) == (location.y-1)%/%(r.division),1,0)})
  
  ds.value.sum=sum(v*(index.x%*%t(index.y)))
  
  
  drop.duplicates.i=(1:division)*r.division
  
  particle=ds.value.sum+v0*c(0,as.vector(index.x[drop.duplicates.i]%*%t(index.y[drop.duplicates.i])))
  
  if(Hellinger){value1=sqrt(particle)-1/division}
  else{value1=division*(particle-division^(-2))}
  
  return(value1)
}

ds.center.sum = function(count, location.x,location.y, divisions,i.division,  weakening = T, Hellinger = F){
  
  max.division=dim(count$counts)[1]
  division=divisions[i.division]
  
  r.division=max.division %/% division
  
  index.x=sapply(1:max.division,
                 function(i){ifelse((i-1)%/%(r.division) == (location.x-1)%/%(r.division),1,0)})
  
  index.y=sapply(1:max.division,
                 function(i){ifelse((i-1)%/%(r.division) == (location.y-1)%/%(r.division),1,0)})
  
  
  local.count=sum(count$counts*(index.x%*%t(index.y)))
  total.count=sum(count$counts)
  extra.count=count$count0[i.division*weakening+1]
  
  
  
  difference=ifelse(Hellinger,
                    ifelse(local.count==0,
                           exp(lgamma(total.count+extra.count)-lgamma(total.count+extra.count+0.5)-
                                 lgamma(0+extra.count+0.5)+lgamma(extra.count)-2*log(division)),
                           exp(lgamma(total.count+extra.count)-lgamma(total.count+extra.count+0.5)-
                                 lgamma(local.count+extra.count)+lgamma(local.count+extra.count+0.5)-2*log(division))+
                             exp(lgamma(total.count+extra.count)-lgamma(total.count+extra.count+0.5)-
                                   lgamma(local.count)+lgamma(local.count+0.5)+log1p(-division^(-2))))-
                      1/division,
                    division*((local.count+extra.count*(division^(-2))) / (total.count+extra.count)-
                                division^(-2)))
  
  return(difference)
}

ds.value.particles = function(ds.particles,i.division,weakening=T,Hellinger=F){
  n.ds = length(ds.particles$pre)
  max.division = dim(ds.particles$count$counts)[1]
  division = ds.particles$divisions[i.division]
  
  r.division=max.division %/% division
  
  drop.duplicates.i=(1:division)*r.division
  
  
  ds.center.vec = foreach(i.x = drop.duplicates.i,.combine=rbind)%:%
    foreach(i.y= drop.duplicates.i,.combine=rbind)%do%{
      ds.center.sum(ds.particles$count, i.x,i.y, 
                    ds.particles$divisions,i.division,
                    weakening,Hellinger)}
  
  ds.value.vec=lapply(1:n.ds,
                      function(i.list){
                        ds.center.vec = foreach(i.x= drop.duplicates.i,.combine=rbind)%:%
                          foreach(i.y = drop.duplicates.i,.combine=rbind)%do%{
                            ds.value.sum(ds.particles$pre[[i.list]], i.x,i.y, 
                                         ds.particles$divisions,i.division,
                                         weakening,Hellinger)}
                      }
  )
  
  return(list(center=ds.center.vec,particles=ds.value.vec))
}

ds.generate.particles = function(u.counts,divisions,n.ds=1000L,count.per.cell=3L){
  max.division = Reduce(Lcm,divisions) 
  n.divisions=length(divisions)
  total.count=sum(u.counts)
  
  weakening.list=as.integer(1+pmax(0,c(0,count.per.cell*divisions^2-total.count)))
  # "first 0 in pmax" stands for no weakening
  
  pre.list=foreach(i=1:n.ds)%do%{
    pre.v=apply(u.counts,c(1,2),function(n){rgamma(1,n)})
    
    pre.v0=cumsum(rgamma(n.divisions+1, diff(c(0,weakening.list))))
    #This is being made ready for various weakening - pre.v0[1] is usual
    
    return(list(pre.v=pre.v, pre.v0=pre.v0))
  }
  return(list(pre=pre.list,count=list(counts=u.counts,count0=weakening.list),divisions = divisions))
}

ds.pvalue = function(ds.pval.particles){
  level=length(ds.pval.particles$center)
  n.ds=length(ds.pval.particles$particles)
  
  #distance of center to origin
  l2.center=sum((ds.pval.particles$center)^2)
  
  #distance of particle to center
  l2.max=sapply(1:n.ds,function(i.list){
    colSums((ds.pval.particles$particles[[i.list]]-ds.pval.particles$center%*%matrix(1,1,level+1))^2)%>%max()
  })
  
  l2.min=sapply(1:n.ds,function(i.list){
    temp.mat=ds.pval.particles$particles[[i.list]]
    
    temp.mat1=temp.mat[,2:(level+1)]- temp.mat[,1]%*%matrix(1,1,level)
    temp.mat2=temp.mat[,1]-ds.pval.particles$center
    
    C1=t(temp.mat1)%*%temp.mat1
    d1=as.vector(t(temp.mat2)%*%temp.mat1)
    
    temp.min=quadprog(C1,d1, A=matrix(1,1,level), b=1, lb=rep(0,level), ub=rep(1,level))
    
    return(2*temp.min$fval+sum(temp.mat2^2))
  })
  
  return(list(pval.upper=mean(l2.center<l2.max),pval.lower=mean(l2.center<l2.min)))
  
}


# Analysis of Dempster-Shafer Code
## compiled by: Alexander C. Murph

setwd("/Users/murph/Documents/ds-work")
list.files("runtime_data/")

ds_data = NULL
for(file_name in list.files("runtime_data/")){
  temp_data   = read.csv(paste("runtime_data/", file_name, sep = ""))
  temp_data$X = NULL
  ds_data     = rbind(ds_data, temp_data)
}


dDSM_data = ds_data[which(ds_data$model == 'dDSM'),]
sDSM_data = ds_data[which(ds_data$model == 'sDSM'),]

summary(dDSM_data[which(dDSM_data$divisions==2),])
sd(dDSM_data[which(dDSM_data$divisions==2),]$runtime)
summary(dDSM_data[which(dDSM_data$divisions==3),])
sd(dDSM_data[which(dDSM_data$divisions==3),]$runtime)
summary(dDSM_data[which(dDSM_data$divisions==6),])
sd(dDSM_data[which(dDSM_data$divisions==6),]$runtime)
summary(dDSM_data[which(dDSM_data$divisions==8),])
sd(dDSM_data[which(dDSM_data$divisions==8),]$runtime)

summary(sDSM_data[which(sDSM_data$divisions==2),])
sd(sDSM_data[which(sDSM_data$divisions==2),]$runtime)
summary(sDSM_data[which(sDSM_data$divisions==3),])
sd(sDSM_data[which(sDSM_data$divisions==3),]$runtime)
summary(sDSM_data[which(sDSM_data$divisions==6),])
sd(sDSM_data[which(sDSM_data$divisions==6),]$runtime)
summary(sDSM_data[which(sDSM_data$divisions==8),])
sd(sDSM_data[which(sDSM_data$divisions==8),]$runtime)





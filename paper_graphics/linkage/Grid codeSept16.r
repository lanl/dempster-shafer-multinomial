#Kari's Grid code
#June 19, 2007

########################################################
######### Get values for t1, t2, t3 from Grid ##########
########################################################

#Completely cover a grid of possible t-values for t1, t2, t3, t4
#Constraints on values: t1 + t2 + t3 + t4 = 1
#Incrementing by a value of 0.01
MakeT<-function(){
t1<- 0
t2<- 0
t3<- 0

count<- 0

for (t1 in 0:100){#loop t1
t2UB <- 100-t1
	for (t2 in 0:t2UB){#loop t2

t3UB <- t2UB-t2
		for (t3 in 0:t3UB){#loop t3

			t4 <- 100-t1-t2-t3
			sum_t = t1+t2+t3+t4
			count<- count+1
			
			print(c(t1/100,t2/100,t3/100,t4/100,count,sum_t))
                  x<- cbind(count,t1/100,t2/100,t3/100,t4/100)		
		
#write(x, file = "Gimme_t.dat", ncolumns = if(is.character(x)) 1 else 5, append = TRUE, sep = "\t")

#write(x, file = "Gimme_tSept16.dat", append = TRUE)#, ncolumns = if(is.character(x)) 1 else 5, append = TRUE, sep = "\t")


						}#end for loop t3

					}#end for loop t2

			}#end for loop t1
}

T = 1000
O = matrix(0,ncol = T,nrow = 2)
fcoin = c(0.5,0.5)
bcoin = c(0.3,0.7)

S = matrix(c(0.99,0.01,0.01,0.99), ncol=2)
E = matrix(c(0.5,0.3,0.5,0.7), ncol = 2)

#GENERATE DATA

O[1,1] = sample(c(0,1),size=1,prob=c(0.5,0.5))

transition = function(x){
	
	change = sample(c(0,1),prob =c(0.99,0.01),size=1)
	if(change == 0){
		y = x
		}else{
			y = abs(1-x)
			}
	return(y)
	}

for(i in 2:T){
	O[1,i] = transition(O[1,i-1])

	}

emit = function(x){
	if(x == 0){
	y = sample(c(0,1),size=1,prob=fcoin )
	}else{
		y = sample(c(0,1),size=1,prob=bcoin)
		}
	
	}
	
	
O[2,] = sapply(O[1,],emit)

#FWD ALGORITHM

alpha = matrix(0,ncol=T+1,nrow =2)
alpha[,1] = log(0.5)


for(x in 2:(T+1)){

	ff = (exp(alpha[1,x-1])*0.99 + exp(alpha[2,x-1])*0.01)
	fb = (exp(alpha[1,x-1])*0.01 + exp(alpha[2,x-1])*0.99)
	
	if(O[2,x-1] == 0){
		alpha[1,x] = log(ff*fcoin[1])
		alpha[2,x] = log(fb*bcoin[1])
		}else{
			alpha[1,x] = log(ff*fcoin[2])
			alpha[2,x] = log(fb*bcoin[2])
			}
	
	}



#BWD ALGORITHM


beta = matrix(0,ncol=T+1,nrow =2)
beta[,1] = 0

for(x in T:1){
	beta[1,x] = log(exp(beta[1,x+1])*0.99*E[1,(O[x+1]+1)] + exp(beta[2,x+1])*0.01*E[2,(O[x+1]+1)])
	beta[2,x] = log(exp(beta[1,x+1])*0.01*E[1,(O[x+1]+1)] + exp(beta[2,x+1])*0.99*E[2,(O[x+1]+1)])
	
	
	}











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
#aesthetic change
O=t(O)


#FWD ALGORITHM

fwd=function(observed,fcoin,bcoin,change){

	#Making emissions vectors
	log.emissions.fair = log(matrix(.5,nrow=1,ncol=length(observed)))
	log.emissions.biased =  matrix(nrow=1,ncol=length(observed))
	log.emissions.biased[which(observed==0)] = log(bcoin[1])
	log.emissions.biased[which(observed==1)] = log(bcoin[2])
	
	#Initialization
	alpha = data.frame(matrix(nrow=(length(observed)+1),ncol=2))
	colnames(alpha,do.NULL=FALSE);colnames(alpha)=c("fair","biased")
	alpha[1,] = log(0.5)

	#Recursion
	for(flip in 1:length(observed)){
		alpha$fair[(flip+1)]=log.emissions.fair[flip]+log(sum(exp(alpha[flip,])*S[1,]))
		alpha$biased[(flip+1)]=log.emissions.biased[flip]+log(sum(exp(alpha[flip,])*S[2,]))
	}
	return(alpha)
}

#Backward algorithm

bwd=function(observed,fcoin,bcoin,change){

	#Making emissions vectors
	log.emissions.fair = log(matrix(.5,nrow=1,ncol=length(observed)))
	log.emissions.biased =  matrix(nrow=1,ncol=length(observed))
	log.emissions.biased[which(observed==0)] = log(bcoin[1])
	log.emissions.biased[which(observed==1)] = log(bcoin[2])
	
	#Initialization
	beta = data.frame(matrix(nrow=(length(observed)+1),ncol=2))
	colnames(beta,do.NULL=FALSE);colnames(beta)=c("fair","biased")
	beta[nrow(beta),] = log(1)

	#Recursion
	for(flip in length(observed):1){
		log.em=c(log.emissions.fair[flip],log.emissions.biased[flip])
		beta$fair[(flip)]=log(sum(exp(log.em+log(S[1,])+unlist(beta[flip+1,]))))
		beta$biased[(flip)]=log(sum(exp(log.em+log(S[2,])+unlist(beta[flip+1,]))))
	}
	return(beta)
}


f=fwd(O[,2],fcoin,bcoin,S)
b=bwd(O[,2],fcoin,bcoin,S)
#decoding
prob=exp(((f+b)-log(sum(exp(f[nrow(f),])))))

plot(O[,1],type="l")
lines(prob[,2],col="blue")
legend(775,.95,c("observed","hmm"),col=c("black","blue"),lwd=2,bty="n")






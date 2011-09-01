#ADG's toy HMM
#Set up
#We have two coins (zero and one), which have different probs of coming up heads
#We pick a coin at random and hold onto it for a while and then switch coins
#Our goal is to guess which coin we are tossing when


#Setting parameters
T = 1000 # nuber of attempts
O = data.frame(matrix(nrow = T,ncol = 2))
	colnames(O,do.NULL=FALSE);colnames(O)=c("state","observation")
coin.zero = c(0.5,0.5) #prob of tails, prob of heads from coin 0
coin.one = c(0.2,0.8) #prob of tails, prob of heads from coin 1
transitions = data.frame(matrix(c(0.99,0.01,0.01,.99), ncol=2,byrow=TRUE))
	colnames(transitions,do.NULL=FALSE);colnames(transitions)=c("to.0","to.1")
	rownames(transitions,do.NULL=FALSE);rownames(transitions)=c("from.0","from.1")

#GENERATE DATA
	#Getting states
	get.states=function(transitions,T){
		temp=numeric(T)
		temp[1] = sample(c(0,1),size=1,prob=c(0.5,0.5))
		for(s in 2:T){
			if(temp[s-1]==0){temp[s]=ifelse(sample(c(0,1),prob =transitions[1,],size=1),1,0)}
			if(temp[s-1]==1){temp[s]=ifelse(sample(c(0,1),prob =transitions[2,],size=1),1,0)}
		}
		return(temp)
	}

	#Getting observations
	get.obs=function(states,coin.zero,coin.one){
		temp=numeric(length(states))
		temp[which(states==0)]=sample(c(0,1),size=length(which(states==0)),prob=coin.zero,replace=TRUE)
		temp[which(states==1)]=sample(c(0,1),size=length(which(states==1)),prob=coin.one,replace=TRUE)
		return(temp)
	}

O$state=get.states(transitions,T)
O$observation=get.obs(O$state,coin.zero,coin.one)

#HMM
	#FWD ALGORITHM
	fwd=function(observed,coin.zero,coin.one,transitions){
		#Making emissions vectors
		log.emissions.coin.zero =  matrix(nrow=1,ncol=length(observed))
		log.emissions.coin.zero[which(observed==0)] = log(coin.zero[1])
		log.emissions.coin.zero[which(observed==1)] = log(coin.zero[2])
		log.emissions.coin.one =  matrix(nrow=1,ncol=length(observed))
		log.emissions.coin.one[which(observed==0)] = log(coin.one[1])
		log.emissions.coin.one[which(observed==1)] = log(coin.one[2])
	
		#Initialization
		alpha = data.frame(matrix(nrow=(length(observed)+1),ncol=2))
		colnames(alpha,do.NULL=FALSE);colnames(alpha)=c("coin.zero","coin.one")
		alpha[1,] = log(0.5)

		#Recursion
		for(flip in 1:length(observed)){
			alpha$coin.zero[(flip+1)]=log.emissions.coin.zero[flip]+log(sum(exp(alpha[flip,])*transitions[1,]))
			alpha$coin.one[(flip+1)]=log.emissions.coin.one[flip]+log(sum(exp(alpha[flip,])*transitions[2,]))
		}
		return(alpha)
	}

	#BWD ALGORITHM
	bwd=function(observed,coin.zero,coin.one,transitions){
		#Making emissions vectors
		log.emissions.coin.zero =  matrix(nrow=1,ncol=length(observed))
		log.emissions.coin.zero[which(observed==0)] = log(coin.zero[1])
		log.emissions.coin.zero[which(observed==1)] = log(coin.zero[2])
		log.emissions.coin.one =  matrix(nrow=1,ncol=length(observed))
		log.emissions.coin.one[which(observed==0)] = log(coin.one[1])
		log.emissions.coin.one[which(observed==1)] = log(coin.one[2])
		
		#Initialization
		beta = data.frame(matrix(nrow=(length(observed)+1),ncol=2))
		colnames(beta,do.NULL=FALSE);colnames(beta)=c("coin.zero","coin.one")
		beta[nrow(beta),] = log(1)
		#Recursion
		for(flip in length(observed):1){
			log.em=c(log.emissions.coin.zero[flip],log.emissions.coin.one[flip])
			beta$coin.zero[(flip)]=log(sum(exp(log.em+log(transitions[1,])+unlist(beta[flip+1,]))))
			beta$coin.one[(flip)]=log(sum(exp(log.em+log(transitions[2,])+unlist(beta[flip+1,]))))
		}
		return(beta)
	}

	#Running forward and backward
	f=fwd(O$observation,coin.zero,coin.one,transitions)
	b=bwd(O$observation,coin.zero,coin.one,transitions)
	#posterior decoding
	p.coin01=exp(((f+b)-log(sum(exp(f[nrow(f),])))))
	
	#to prove that our probs sum about 1
	print(c("max sum of probs is",max(rowSums(p.coin01)),"good enough?"))
	print(c("min sum of probs is",min(rowSums(p.coin01)),"good enough?"))


#Visual presentation
	get.avg.window=function(obs,wind.size){
		temp=numeric(length(obs)/wind.size)
			for(Window in 1:(length(obs)/wind.size)){temp[Window]=mean(obs[c(((Window-1)*wind.size+1):((Window-1)*wind.size+10))])}
		return(temp)
	}
	plot(O[,1],type="l",xlab="Flip number",ylab="prob coin one")
	lines(p.coin01[,2],col="blue")
	obs.per.window=10
	points(c(1:1000)[c(1:1000)%%obs.per.window==round(mean(c(0,obs.per.window=10)))],get.avg.window(O[,2],obs.per.window),pch=18,cex=.5,col="red")
	mtext(sprintf("percent heads in a window of length %s",obs.per.window),side=4,col="red")
	legend(775,.95,c("true","hmm"),col=c("black","blue"),lwd=2,bg="white")


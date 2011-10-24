#' Backward Algorithm for Hidden Markov Model (HMM)
#' @param observed observed states
#' @param coin.zero probability of observing a one with coin zero: c(p, 1-p)
#' @param coin.one probability of observing a one with coin one: c(p, 1-p)
#'  (example of an extra line)
#' @param transitions transition matrix for HMM. See details.
#' @details The transition matrix should be a stochastic matrix.
#' This is the probability of switching states. 
#' @return beta, a two-dimensional list giving backward probabilities for
#'  each state at each site, beta$coin.zero for coin zero and beta$coin.one
#'  for coin one
#' @examples
#'  obs = rbinom(100, prob=0.8)
#'  coin.z = c(.5,.5)
#'  coin.o = c(.7,.3)
#'  trans = matrix(c(.8,.2,.2,.8), byrow=TRUE)
#'  bwd(obs, coin.z, coin.o, trans)

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

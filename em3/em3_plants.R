data(PlantGrowth)
pg<-subset(PlantGrowth,group!='ctrl')
pg$group=factor(pg$group)

pg1=  pg[pg$group=='trt1',]
pg2=  pg[pg$group=='trt2',]

hist(pg$weight)
lines(density(pg1$weight), col='blue')
lines(density(pg2$weight), col='red')

boxplot(weight~group, data=pg)

# data weights from mixture of two treatments
DATA = pg$weight
UNK = pg$group

# functions to test things out if groups are known ... ie full data

L_kn<-function(P, D=pg){
	dat1 = D[D$group == "trt1","weight"]
	lik1 = sum(dnorm(dat1, P[2], sqrt(P[4]), log=T))

	dat2 = D[D$group == "trt2","weight"]
	lik2 = sum(dnorm(dat2, P[3], sqrt(P[5])), log=T)

	total_lik = lik1+lik2
	trunc_lik = max(-700, total_lik)
	return(trunc_lik)

	}

maxL<-function(P,D=pg){
	#returns new parameter estimates given old pars P, data D, and an expecation function
	f<-function(X){L_kn(X,D=pg)}
	o<-optim(P,f,  method="L-BFGS-B", lower=rep(0,5), upper=c(1,rep(Inf,4)), control=c(pgtol=-1))
	o
	}



# our model is that the weights are drawn either from one treatment or another 
# but treatment identity (col 2) is unknown
PAR = c(tau=0.5, mu1=10, mu2=2.5, sigma1=1, sigma2=1.2) 

# try out with 'full data'
maxL(PAR,DATA)	# damn, doesn't work...


#E 
# 
# calculates the conditional expectation of the log likelihood for the complete data, (E_{UNK|DATA,PAR}) log L(PAR|DATA, UNK) given observed data (D) and parameter estimates (P)
# 
E<-function(P, D){
	# calculates p(D | P)
	
	#find the conditional 
	prob_g1 = P[1]*dnorm(D, P[2], sqrt(P[4]))
	total_prob = P[1]*dnorm(D, P[2], sqrt(P[4]))+ (1-P[1])*dnorm(D, P[3], sqrt(P[5]))
	Ep = prob_g1 / total_prob
	
	total_lik 	= log(prod(Ep*dnorm(D, P[2], sqrt(P[4])) + (1-Ep)*dnorm(D, P[3], sqrt(P[5]))))

	trunc_lik = max(-700, total_lik)
	return(trunc_lik)
	}

# M 
M<-function(P,D){
	#returns new parameter estimates given old pars P, data D, and an expecation function
	f<-function(X){E(X,D)}
	o<-optim(P,f,  method="L-BFGS-B", lower=rep(0,5), upper=c(1,rep(Inf,4)), control=c(pgtol=-1))
	o
	}
M(PAR,DATA)	

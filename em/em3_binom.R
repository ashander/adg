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

# our model is that the weights are drawn either from one treatment or another but treatment identity (col 2) is unknown
PAR = c(tau=0.5, mu1=4.5, mu2=5.5, sigma1=1, sigma2=1.2) 

#E 
# 
# calculates the conditional expectation of the log likelihood for the complete data, (E_{UNK|DATA,PAR}) log L(PAR|DATA, UNK) given observed data (D) and parameter estimates (P)
# 
E<-function(P, D){
	# calculates p(D | P)
	
	#find the conditional 
	
	}




# M 

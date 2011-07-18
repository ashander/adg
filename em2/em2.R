data(faithful)
attach(faithful) 
#Assumes two gausian populations of erruptions
W = waiting
# Guess of the parameters: 
s = c(p=0.5, mu1=50, mu2=90, sigma1=30, sigma2=30) 
 
 
# EXPECTATION STEP
expectation_step <- function(parameters, observed){
	# yields Q(\theta, \theta^(k)) as in Chang
  	W <- observed
  	s <- parameters
 		
	lik_type_1 = s[1]*dnorm(W, s[2], sqrt(s[4]))
	lik_type_2 = (1-s[1])*dnorm(W, s[3], sqrt(s[5]))
	sum_of_lik = lik_type_1 + lik_type_2
 	Ep = lik_type_1/sum_of_lik
   
   	Eloglik <- sum(dnorm(W, s[2], sqrt(s[4]), log=TRUE)+log(Ep)+
                    dnorm(W, s[3], sqrt(s[5]), log=TRUE)+log(1-Ep))

 	return(-Eloglik) # return the expected negative log likelihood
}


# MAXIMIZATION STEP 
maximization_step <- function(parameters, observed){
  W <- observed
  ## maximize likelihood over parameters
  maximize_me <- function(X){
  	expectation_step(X, W)
  }
  # get the function to maximize:
  # maximize it: 
	o <- optim(parameters, maximize_me)
  # return the optimum:
	o$par
}

# TESTING:
	expectation_step(s, W)
	maximization_step(s, W)

iter = function(W, s) { 

	s1 <- maximization_step(s,W)
	for (i in 1:length(s)) { 
		if (abs(s[i]-s1[i]) > 0.001) { 
				s=s1 
				iter(W,s) 
			} else s1  
			}
			s1
}

iter(W,s)	

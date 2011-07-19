data(faithful)
attach(faithful) 
#Assumes two gausian populations of erruptions
W = waiting
# Guess of the parameters: 
s = c(p=0.5, mu1=50, mu2=90, sigma1=30, sigma2=30) 
 
 
# EXPECTATION STEP
expectation_step <- function(observed, parameters){
  W <- observed
  s <- parameters
 
	probability_type_1 = s[1]*dnorm(W, s[2], sqrt(s[4]))
	sum_of_probs = s[1]*dnorm(W, s[2], sqrt(s[4]))+
                 (1-s[1])*dnorm(W, s[3], sqrt(s[5]))
	Ep = probability_type_1/sum_of_probs
}


# MAXIMIZATION STEP 
maximization_step <- function(W, missing, s){

  ## maximize likelihood over parameters
  maximize_me <- function(observed, missing_data){
    W <- observed
    Ep <- missing_data
    function(s){
      loglik <- sum(dnorm(W, s[2], sqrt(s[4]), log=TRUE)+log(Ep)+
                    dnorm(W, s[3], sqrt(s[5]), log=TRUE)+log(1-Ep))
      -loglik
    }
  }
  # get the function to maximize:
	f <- maximize_me(W, missing) 
  # maximize it: 
	o <- optim(s, f)
  # return the optimum:
	o$par
}

# TESTING:
	missing <- expectation_step(W,s)
  s <- maximization_step(W,missing, s)


# Abstract version
iter = function(W, s) { 
	missing <- expectation_step(W,s)
  s <- maximization_step(W,missing)
	for (i in 1:length(s)) { 
		if (abs(s[i]-s1[i]) > 0.001) { 
				s=s1 
				iter(W,s) 
			} else 
          s1  
}
iter(W,s)	

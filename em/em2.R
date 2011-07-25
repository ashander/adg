data(faithful)
attach(faithful) 
#Assumes two gaussian populations of eruptions
W = waiting
# Guess of the parameters: 
s <- c(p=0.5, mu1=50, mu2=90, sigma1=30, sigma2=30) 
 
 
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
  minimize_me <- function(observed, missing_data){
    W <- observed
    Ep <- missing_data
    function(s){
      if(s[1] <= 0 || s[1] >= 1) 
        loglik <- -Inf
      else {
      loglik <- sum(dnorm(W, s[2], sqrt(s[4]), log=TRUE)+log(Ep) +
                    dnorm(W, s[3], sqrt(s[5]), log=TRUE)+log(1-Ep) ) +
                sum(log(s[1]*dnorm(W, s[2], sqrt(s[4]))+
                    (1-s[1])*dnorm(W, s[3], sqrt(s[5]))))
                    ## The log likelihood of observing Ep given s[1] 
      }
      -loglik
    }
  }
  # get the function to maximize:
  f <- minimize_me(W, missing) 
  # maximize it: 
  o <- optim(s, f)
  # return the optimum:
  print(o$value)
  o$par
}

# TESTING:
#  missing <- expectation_step(W,s)
#  s <- maximization_step(W,missing, s)
j <- 0
# Abstract version
iter = function(W, s) { 
  j <- j+1
  print(j)
 missing <- expectation_step(W,s)
  s1 <- maximization_step(W,missing,s)
  for (i in 1:length(s)) { 
    if (abs(s[i]-s1[i]) > 1e-16) { 
        s=s1 
        iter(W,s) 
    } else {
           return(s1)
    }
  }
}
#out <- iter(W,s) 
#print(out)


#s <- c(p=0.5, mu1=50, mu2=90, sigma1=30, sigma2=30) 
s <- c(0.3507784, 54.2179838, 79.9088649, 29.8611799, 35.9824271)
for(i in 1:10){
  print(s)
  missing <- expectation_step(W,s)
  s <- maximization_step(W,missing,s)
}





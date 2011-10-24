\name{bwd}
\alias{bwd}
\title{Backward Algorithm for Hidden Markov Model (HMM)}
\usage{
  bwd(observed, coin.zero, coin.one, transitions)
}
\arguments{
  \item{observed}{observed states}

  \item{coin.zero}{probability of observing a one with coin
  zero: c(p, 1-p)}

  \item{coin.one}{probability of observing a one with coin
  one: c(p, 1-p) (example of an extra line)}

  \item{transitions}{transition matrix for HMM. See
  details.}
}
\value{
  beta, a two-dimensional list giving backward
  probabilities for each state at each site, beta$coin.zero
  for coin zero and beta$coin.one for coin one
}
\description{
  Backward Algorithm for Hidden Markov Model (HMM)
}
\details{
  The

  transition matrix should be a stochastic matrix. This is
  the probability of switching states.
}
\examples{
obs = rbinom(100,1, prob=0.8)
coin.z = c(.5,.5)
coin.o = c(.7,.3)
trans = matrix(c(.8,.2,.2,.8), byrow=TRUE)
bwd(obs, coin.z, coin.o, trans)
}


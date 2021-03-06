library(ggplot2)
library(qualityTools)

#-----------------------------------------
# 1. Exponential distribution
# create a vector of w exponential waiting times with lambda = lam

set.seed(50)

wait <- function(w,lam){
  if(w <= 0)stop("w should be positive")
  if(lam <= 0)stop("lam should be positive")
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}
w <- wait(10000,5)

# Plot histogram of simulated w along with density.

lambda=5
scale <- length(w)/10
x <- seq(min(w),max(w),0.1)
y <- dexp(x,lambda)*scale
qplot(w, binwidth = .1) + geom_line(aes(x,y,color='red'))

qqPlot(w)
mean(w)

# mean(w) is 0.2 equal to 1/rate. It's exponential distributed.

#--------------------------------------------------------------

# 2. Poisson distribution
# create a vector of exponential waiting times which total t <= Max with lambda = lam

wait.until <- function(Max,lam){
  if(Max <= 0)stop("Max should be positive")
  if(lam <= 0)stop("lam should be positive")
  set.seed(50)
  time = 0
  a = NULL
  while(time < Max){
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(c(a[1:(length(a)-1)],"time"=time-inter))
}
# seed test:
wait.until(5,2)
wait.until(6,2)

# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# Remove "set.seed" in function wait.until()

wait.until <- function(Max,lam){
  if(Max <= 0)stop("Max should be positive")
  if(lam <= 0)stop("lam should be positive")
  time = 0
  a = NULL
  while(time < Max){
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(a[1:(length(a)-1)])
}

poi.test <- function(rep, Max, lam){
  if(rep <= 0)stop("rep should be positive")
  n=rep
  a = NULL
  for(i in 1:rep){
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}
p <- poi.test(10000,5,2)

# Plot histogram of simulated p along with its density.
# Since the density of p is very close to density of poisson distribution,
# we can say p is probably posson distributed.
# And since mean(p) = var(p), we can prove it is poisson distributed.

lambda=mean(p)
scale <- length(p)
x <- seq(min(p),max(p),1)
y <- dpois(x,lambda)*scale
qplot(p, binwidth = 1) + geom_line(aes(x,y,color='red'))

var(p)
mean(p)

## Since the main idea is to explore the relationship 
## between exponential distribution and poisson distribution
## we can demonstrate the connection between the mean and variance of poisson distribution
## and the parameter of exponential distribution
## that is, mean(p)=var(p)=Max*lam, where Max measures the cumulative time
## and lam is the parameter for exponential distribution that generated the time intervels

# Another way to check if it is poisson distributed is using KS test.
rp <- rpois(10000,lambda)
ks.test(p,rp)

#--------------------------------------------------------------------

# 3. Gamma distribution
# now simulate the waiting time for k events to occur with lambda = lam
# The output include each waiting time and "time": total waiting time.

wait.for <- function(k, lam){
  if(k <= 0) stop("k should be positive")
  if(lam <= 0) stop("lam should be positive")
  set.seed(50)
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    a=c(a,inter)
    count = count + 1
    time = time+inter
  }
  return(c(a,"time"=time))
}
# seed test:
wait.for(5,2)
wait.for(6,2)

# Simulate gamma test to show total waiting time is gamma distributed. 
# first redefine function wait.for(), remove set.seed()

wait.for <- function(k, lam){
  if(k <= 0) stop("k should be positive")
  if(lam <= 0) stop("lam should be positive")
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)
    count = count + 1
    time = time+inter
  }
  return(time)
}

gam.test <-function(rep, k, lam ){
  if(rep <= 0)stop("rep should be positive")
  a=NULL
  for (i in 1:rep){
    t = wait.for(k,lam)
    a = c(a,t)
  }
  return(a)
}
g <- gam.test(10000,4,2)

# Plot histogram of simulated g along with its density.
# First according to histogram, we can see g is probably gamma distributed.
# then we calculate mean(g)/var(g) equal to lambda.
# So it proves g is gamma distributed.

x <- seq(min(g),max(g),0.1)
y <- dgamma(x,4,2)*(length(g)/10)
qplot(g, binwidth = 0.1) + geom_line(aes(x,y,color='red'))

mean(g)
var(g)
mean(g)/var(g)

## Relationship between exponential and gamma:
lambda <- mean(g)/var(g)
alpha <- mean(g)^2/var(g)
## for a gamma distribution with parameters of alpha and lambda
## alpha is equal to the number of events occurred
## and lambda is equal to the parameter of the exponential distribution
## that generates the time intervals between events

# Another way to check if it is gamma distributed is using KS test.
rg <- rgamma(10000,mean(g)^2/var(g),mean(g)/var(g))
ks.test(g,rg)

#--------------------------------------------------------------------
# 4. Conclusion:
# In waiting time example, the waiting time divided by k events are expotential distributed,
# the number of k events are poisson distributed,
# and total waiting time are gamma distributed.

library(ggplot2)
library(qualityTools)

# create a vector of w exponential waiting times with lambda = lam

wait <- function(w,lam){
  set.seed(50)
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))
  }
  return(a)
}
w <- wait(2000,1)
hist(w)
qqPlot(w)


# create a vector of exponential waiting times which total t <= Max with lambda = lam

wait.until <- function(Max,lam){
  set.seed(50)
  time = 0
  a = NULL
  while(time < Max){
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(a[1:(length(a)-1)])
}
# seed test:
wait.until(5,1)
wait.until(6,1)


# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# Remove "set.seed" in function wait.until()
wait.until <- function(Max,lam){
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
  a = NULL
  for(i in 1:rep){
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}
p <- poi.test(10000,6,1)
hist(p)
var(p)
mean(p)
# From the histogram of p, we can see it's poisson distributed.
# And since mean(p) = var(p), we can prove it is poisson distributed.


# now simulate the waiting time for k events to occur with lambda = lam
# The output include each waiting time and "time": total waiting time.

wait.for <- function(k, lam){
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
wait.for(6,1)

# Simulate gamma test to show total waiting time is gamma distributed. 
# first redefine function wait.for(), remove set.seed()
wait.for <- function(k, lam){
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

gam.test <-function(rep, max.e, lam ){
  a=NULL
  for (i in 1:rep){
    t = wait.for(max.e,lam)
    a = c(a,t)
  }
  return(a)
}
g <- gam.test(10000,6,3)
hist(g)
mean(g)
var(g)
mean(g)/var(g)
# First according to histogram, we can see g is gamma distributed.
# then we calculate mean(g)/var(g) equal to lambda 3.
# So it proves g is gamma distributed.
# In other words, in waiting time example, the number of events
# is poisson distributed while total waiting time is gamma distributed.

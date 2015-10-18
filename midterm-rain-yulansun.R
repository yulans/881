# 1. Preparation:
# After check each month's data, I found "T" represents trace, "M" represents missing data,
# "----" represents no rain days. So I convert "----" and "M" to NA, convert "T" to 0. 
# So storm will be the sum of numbers seperated by NA.

filelist = list.files(pattern = "*.txt")
datalist = lapply(filelist, function(x)read.csv(x, skip = 2, stringsAsFactors = F)) 
t = do.call("rbind", datalist) 

colnames(t) <- 0:24
t[t=="----"] <- NA
t[t=="T   "] <- 0
t[t=="M   "] <- 0
t <- t[2:25]

# I change the data frame to a vector. I transpose t first so t can be unlisted by row.
t <- t(t)
t <- unlist(t)
t <- as.numeric(t)

# 2. Stormfinder function
# How this function works: if t[n] is NA, it jumps to t[n+1] to see if it is NA and so on. 
# If t[n] is a number, storm [j] = t[n]. Then if t[n+1] is still a number, 
# storm[j] = storm[j] + t[n+1] and so on. But if t[n+1] = NA, j = j+1. 
# It means storm[j] has finished. We will put numbers into storm[j+1] next.

# For convenience, at first I set up length of empty storm equal to length of t. 
# Of course length of storm should be less than length of t. In the end, 
# we will drop those extra 0. There's an advantage for doing this. In this rain gauge data, 
# sometimes there're "T" for several days but no rain. Since we convert "T" to 0, 
# there'll be 0 created by those sequential "T" too. So we can drop them at same time. 

stormfinder <- function(x){
  n = length(x)
  storm <- rep(0,n)
  j=1
  
  for (i in 1:n){
    if (is.na(x[i]) == TRUE) {
      i=i+1
    }
    else if (is.na(x[i]) == FALSE) {
      repeat {
        storm[j] = storm[j] + x[i];
        i=i+1;
        if (is.na(x[i])==TRUE)
          j=j+1
        break;
      }
    }
  }
  storm <- storm[storm > 0]
  return(storm)
}

# Try it
stormfinder(t)

# 3. Explore rain gauge data distribution
data <- stormfinder(t)
data <- data.frame(data)
colnames(data) <- "x"

mean(data$x) # 0.2831
var(data$x) # 0.2218

# Frist check the shape of data.
# Plot the histogram and density of the data.
library(ggplot2)

ggplot(data,aes(x)) + geom_histogram(aes(y=..density..), binwidth=.2, color="black",
                                  fill="white") + geom_density(alpha=0.2,fill="#FF6666")

# Using methods of moments, calculate lambda and alpha.
lambda=mean(data$x)/var(data$x)      # 1.2762
alpha=mean(data$x)^2/var(data$x)     # 0.3613

# plot the gamma distribution density with the alpha and lambda on the data histogram.
x <- seq(min(data$x),max(data$x),0.1)
y <- dgamma(x,shape=alpha,rate=lambda)*45
qplot(data$x, binwidth = 0.1) + geom_line(aes(x,y,color='red'))

# From plot we can see it looks like gamma distribution. 


# Using maximum likelihood method to calculate lambda and alpha.
x1 <- data$x
n <- length(data$x)

minus.likelihood <- function(theta) {-(n*theta[1]*log(theta[2])-n*lgamma(theta[1])+
                                         (theta[1]-1)*sum(log(x1))-theta[2]*sum(x1))}

max.likelihood <- nlminb(start=c(0.3613, 1.2762), obj = minus.likelihood)

MLE.alpha <- max.likelihood$par[1]    # 0.5462
MLE.lambda <- max.likelihood$par[2]   # 1.9291 

# 4. Conclusion
# Since lambda calculated by mean/var (1.2762) is close to MLE.lambda (1.9296), 
# we can draw the conclusion that rain gauge data in Boston Logan airport is also 
# gamma distributed. 



install.packages('Lock5Data')
library(Lock5Data)

# general statistics of the data
head(DecemberFlights)
str(DecemberFlights)
boxplot(DecemberFlights$Difference)
hist(DecemberFlights$Difference)
summary(DecemberFlights$Difference)

# Due to high number of outliers we are taking the median time instead of mean time 
mediantime <- median(DecemberFlights$Difference)
nrows <- length(DecemberFlights$Difference)

##ecdf
delay.ecdf <- ecdf(DecemberFlights$Difference)
plot(delay.ecdf)

#Confidence Band for Ecdf
alpha=0.05
Eps=sqrt(log(2/alpha)/(2*nrows))
grid<-seq(0,700, length.out = 1000)
lines(grid, pmin(delay.ecdf(grid)+Eps,1))
lines(grid, pmax(delay.ecdf(grid)-Eps,0))

#median estimation through non-parametric bootstrapping
B=3200
median.boot <- replicate(B, median(DecemberFlights$Difference[sample(1:nrows,size=nrows, 
                                                                 replace = TRUE)]))

# distribution of median values in the bootstrapped samples
hist(median.boot, density=20, breaks = 5, prob=TRUE, 
     xlab="sample medians", main="normal curve over histogram")

curve(dnorm(x, mean=mean(median.boot), sd=sd(median.boot)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

##Confidence Interval of the median value
se.boot<-sd(median.boot)
CI.normal<-c(mediantime-2*se.boot, mediantime+2*se.boot)

CI.pivotal<-2*mediantime-quantile(median.boot,probs = c(0.975, 0.025))

CI.quantile<-quantile(median.boot,probs = c(0.025, 0.975) )

# groupwise summary of data 
delta <- DecemberFlights[1:1000,]
summary(delta$Difference)
hist(delta$Difference)
united <- DecemberFlights[1001:2000,]
summary(united$Difference)
hist(united$Difference)
n<- 1000

#MLE estimate of median_delta - median_united
mu.hat_delta <- median(delta$Difference)
mu.hat_united <- median(united$Difference)

sigma.hat_delta <- sd(delta$Difference)
sigma.hat_united <- sd(united$Difference)

mu.hat = mu.hat_delta - mu.hat_united
mu.hat

sigma.hat <- sqrt(var(delta$Difference)/n +var(united$Difference)/n)  
sigma.hat

c(mu.hat-2*sigma.hat, mu.hat+2*sigma.hat)

#Parametric bootstrap
bootstrap_tau.hat <- vector()
for(i in 1:3200){
  X.hat = rnorm(n, mu.hat_delta, sigma.hat_delta)
  Y.hat = rnorm(n, mu.hat_united, sigma.hat_united)
  bootstrap_tau.hat[i]=mean(X.hat)-mean(Y.hat)
}

bootstrap_tau.hat_se = sd(bootstrap_tau.hat)
bootstrap_tau.hat_se

c(mu.hat-2*bootstrap_tau.hat_se,mu.hat+2*bootstrap_tau.hat_se)

#Wald test for difference of median
## H0: Difference in median is not significant i.e. median.delta - median.united = 0
## HA: Difference in median is significant i.e. median.delta - median.united != 0
z <- (mu.hat-0)/sigma.hat
pvalue <- 2*(pnorm(-abs(z)))
pvalue

#Reject null hypothesis

# Wilcox Test 
wilcox.test(delta$Difference, united$Difference,conf.int = T)

#Bayesian analysis
posterior_delta = rnorm(1000,mean = mu.hat_delta, sd = sigma.hat_delta/sqrt(n))
posterior_united = rnorm(1000,mean = mu.hat_united, sd = sigma.hat_united/sqrt(n))

posterior = posterior_delta - posterior_united
median(posterior)
quantile(posterior, c(0.025, 0.975))


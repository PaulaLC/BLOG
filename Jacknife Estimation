precip # Data
N <- length(precip) # Total Population
n <- 25 # Sample Size
s <- sample(precip, n) # Sample

k <- 25 # Number of subsamples
sJ <- matrix(nrow=n-1,ncol=k) # Subsamples Matrix
for(i in 1:k)  sJ[,i] <- s[-i]  
theta.i <- apply(sJ, 2, mean) 
theta <- mean(s) # Global Estimator
theta.Ji <- n*theta - (n-1)*theta.i # Pseudovalues
theta.J <- mean(theta.Ji) # Jacknife Estimator

var.J <- var(theta.Ji) # Var Jacknife Estimator
IC.up <- theta.J + qt(0.975, n-1)*sqrt(var.J/n) 
IC.low <- theta.J - qt(0.975, n-1)*sqrt(var.J/n)
real.theta <- mean(precip) # Real mean


library(ggplot2)
dt <- data.frame(s, real.theta, theta.J, IC.up, IC.low)
ggplot(dt, aes(s)) +
  geom_density(aes(y=..count..), col='grey', fill='grey') +
  geom_vline(xintercept = c(real.theta, theta.J, IC.up, IC.low), 
             color=c('#e41a1c',rep('#377eb8',3)), 
             linetype=c(rep('dashed',2), rep('solid',2)),size=1) +
  geom_text(aes(real.theta+2, 0.001, label = "Real Mean"),
            size = 4, hjust = 0, angle=90, vjust = 0, color='#e41a1c') +
  geom_text(aes(theta.J-2,0.001, label = "Jacknife Mean"), 
            size = 4, hjust = 0, angle=90, vjust = 0, color='#377eb8')

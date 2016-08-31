mu0 <- 0.15
lambda <- 0.2
N <- rep(0,100)
for (n in 1:100){
mu <- ifelse(N[n]==0, 0, mu0)
N[n+1] <- ifelse(runif(1)<=lambda/(lambda+mu), N[n]+1, N[n]-1)
}
plot(0:100, N, type="s", col="#1f78b4",
xlab="n", ylab="Number of clients in the system", main="M/M/1")

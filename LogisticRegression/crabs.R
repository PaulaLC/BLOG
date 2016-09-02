# Data
width <- rep((22:29),6)
col <- rep(c(rep(0,8),rep(1,8)),3)
y3 <- c(rep(0,16),rep(1,16),rep(2,16))
y <-  c(3,  3,  5,  9,  4,  2,  3,  0, 6,  7,  6,  9,  3,  2,  0,  0,
        3,  2,  8,  5,  4,  7,  5,  7, 2,  0,  1,  7,  4,  2,  5,  1,
        0,  0,  5,  7,  5, 11,  4,  5, 0,  2,  3,  2,  2,  0,  1,  1)
table1 <- xtabs(y ~ y3 + width + col)
rownames(table1) <- c("0: neither", "1: some", "2: many")
addmargins(table1,1)
# , , col = 0
# 
# width
# y3           22 23 24 25 26 27 28 29
# 0: neither  3  3  5  9  4  2  3  0
# 1: some     3  2  8  5  4  7  5  7
# 2: many     0  0  5  7  5 11  4  5
# Sum         6  5 18 21 13 20 12 12
# 
# , , col = 1
# 
# width
# y3           22 23 24 25 26 27 28 29
# 0: neither  6  7  6  9  3  2  0  0
# 1: some     2  0  1  7  4  2  5  1
# 2: many     0  2  3  2  2  0  1  1
# Sum         8  9 10 18  9  4  6  2

summary(table1)
# Call: xtabs(formula = y ~ y3 + width + col)
# Number of cases in table: 173 
# Number of factors: 3 
# Test for independence of all factors:
#   Chisq = 78.85, df = 37, p-value = 7.4e-05
# Chi-squared approximation may be incorrect

# Observed proportions
table2 <-addmargins(table1,1)
p.obs0 <- table2 [1,,]/table2[4,,]
p.obs1 <- table2 [2,,]/table2 [4,,]
p.obs2 <- table2 [3,,]/table2 [4,,]
p0<-cbind(t(p.obs0[,1]),t(p.obs0[,2]))
p1<-cbind(t(p.obs1[,1]),t(p.obs1[,2]))
p2<-cbind(t(p.obs2[,1]),t(p.obs2[,2]))
p.obs<-rbind(p0,p1,p2)
rownames(p.obs) <- c("p0","p1","p2")
colnames(p.obs) <- c(paste(22:29,"-0",sep=""),paste(22:29,"-1",sep=""))
round(p.obs,3)
# 22-0 23-0  24-0  25-0  26-0 27-0  28-0  29-0 22-1  23-1 24-1  25-1  26-1 27-1  28-1 29-1
# p0  0.5  0.6 0.278 0.429 0.308 0.10 0.250 0.000 0.75 0.778  0.6 0.500 0.333  0.5 0.000  0.0
# p1  0.5  0.4 0.444 0.238 0.308 0.35 0.417 0.583 0.25 0.000  0.1 0.389 0.444  0.5 0.833  0.5
# p2  0.0  0.0 0.278 0.333 0.385 0.55 0.333 0.417 0.00 0.222  0.3 0.111 0.222  0.0 0.167  0.5

# Visualization
plot(width[1:8], p.obs[1,1:8], pch="o", type="b", lwd=2,lty=3, col="seagreen2", xaxt="n",
      ylim=c(0,1), xlab="Width", ylab="Probability" )
axis(side=1, at=width[1:8], labels=width[1:8] )
points( width[1:8], p.obs[2,1:8], pch="1", type="b",lwd=2, lty=2, col="seagreen3" )
points( width[1:8], p.obs[3,1:8], pch="2", type="b", lwd=2,lty=1, col="seagreen4" )
points( width[1:8], p.obs[1,9:16], pch="o", type="b", lwd=2,lty=3, col="indianred2")
points( width[1:8], p.obs[2,9:16], pch="1", type="b", lwd=2,lty=2, col="indianred3" )
points( width[1:8], p.obs[3,9:16], pch="2", type="b", lwd=2,lty=1, col="indianred4" )
title("Observed proportions",
      cex.main=0.9)
legend( 25, 1, cex=0.8, legend=rep(c("0: neither", "1: some", "2: many"),2),
        lty=c(3,2,1), lwd=2,col=c(paste("seagreen",2:4),paste("indianred",2:4)), bty="n")

# Logistic model for trend satellites analysis
resp <- rbind(y[1:16],y[17:32],y[33:48])
rownames(resp) <- c("0: neither", "1: some", "2: many")
colnames(resp) <- c(paste(22:29,"-0",sep=""),paste(22:29,"-1",sep=""))

y0 <- resp[1,] 
noy0 <- apply(resp,2,sum)-y0   
rbind(y0, noy0) 
# 22-0 23-0 24-0 25-0 26-0 27-0 28-0 29-0 22-1 23-1 24-1 25-1 26-1 27-1 28-1 29-1
# y0      3    3    5    9    4    2    3    0    6    7    6    9    3    2    0    0
# noy0    3    2   13   12    9   18    9   12    2    2    4    9    6    2    6    2

a0 <- width[1:16]; c0 <- col[1:16]
m.bin <- glm( cbind(y0, noy0) ~ a0 + c0, family=binomial )
summary( m.bin )  
# Call:
#   glm(formula = cbind(y0, noy0) ~ a0 + c0, family = binomial)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.63329  -0.77533  -0.06258   0.59366   1.35916  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  10.2255     2.5576   3.998 6.39e-05 ***
#   a0           -0.4399     0.1011  -4.350 1.36e-05 ***
#   c0            0.7009     0.3539   1.980   0.0477 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 44.262  on 15  degrees of freedom
# Residual deviance: 12.558  on 13  degrees of freedom
# AIC: 53.183
# 
# Number of Fisher Scoring iterations: 4

with(m.bin, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
# [1] 1.304535e-07

# Libraries and colours
library(fda)
library(fda.usc)
library(RColorBrewer)
palette(brewer.pal(8,'Accent')) 
col.boys <- '#386CB075'; col.girls <- '#F0027F75'

sm.growth.girls <- with(growth, smooth.basisPar(argvals=age, y=hgtf, lambda=0.1))
sm.growth.boys <- with(growth, smooth.basisPar(argvals=age, y=hgtm, lambda=0.1))
 
par(mfrow=c(1,2)) # En diferentes gráficos
plot(sm.growth.girls$fd, xlab="age", ylab="height (cm)",
main="Girls' Growth",lty=1, lwd=2, ylim=c(75,200))
plot(sm.growth.boys$fd, xlab="age", ylab="height (cm)",
main="Boys' Growth",lty=1, lwd=2, ylim=c(75,200))
 
par(mfrow=c(1,1)) # En un mismo gráfico
plot(sm.growth.boys$fd, xlab="age", ylab="height (cm)",
main="Berkeley Growth Study data",
lty=1, lwd=2.5, col=col.boys)
lines(sm.growth.girls$fd, lty=1, lwd=2.5, col=col.girls)

Y <- c(rep(1,54), rep(0, 39))
X <- cbind(growth$hgtf, growth$hgtm)
Xdata <- fdata(t(X), argvals = growth$age)
basis <- create.pc.basis(Xdata, c(1,2))
basis.x <- list('X'=basis)
ldata <- list(X=Xdata, df=data.frame(Y))
 
model <- fregre.glm(Y ~ X, data=ldata, family=binomial, basis.x=basis.x)
summary.glm(model)

# Plot results
plot(model$linear.predictors, model$fitted.values,
main='Linear Predictors Vs. Fitted Values',
col=c(rep(col.girls, 54),rep(col.boys, 39)), pch=16)

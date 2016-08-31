library(sm)
library(ggplot2)
attach(trawl)
ind <- c(Zone==1, Year==1, !is.na(Depth))
x <- Depth[ind]
y <- Score1[ind]
df.xy <- data.frame(x=x, y=y)
ggplot(data=df.xy, aes(x=x, y=y)) +
geom_point(color='#006699', size=3.5) +
ggtitle('Trawl Data') +
theme(plot.title = element_text(face='bold', size=32)) +
theme(axis.title = element_text(face='bold', size=22))

smreg <- sm.regression(x,y, h = 5, model='no effect')
## Test of no.effect model: significance = 0.068
df <- data.frame(eval=smreg$eval.points, est=smreg$estimate, se=smreg$se, model.y=smreg$model.y)
df <- data.frame(df, lwr=df$model.y+2*df$se, upr=df$model.y-2*df$se)
 
ggplot() +
geom_point(data=df.xy, aes(x=x, y=y), color='#006699', size=3.5) +
geom_ribbon(data=df, aes(x=eval, ymin=lwr, ymax=upr), fill='#fc8d62', alpha=0.2)+
geom_line(data=df, aes(x=eval, y=est), col='#ff1493', size=1.2) +
ggtitle('Trawl - No Effect Test') +
theme(plot.title = element_text(face='bold', size=32)) +
theme(axis.title = element_text(face='bold', size=22))

st <- sig.trace(sm.regression(x,y,model='no.effect', display='none'), hvec=seq(5,20,length=10))
## Test of no.effect model: significance = 0.068
## Test of no.effect model: significance = 0.058
## Test of no.effect model: significance = 0.047
## Test of no.effect model: significance = 0.039
## Test of no.effect model: significance = 0.033
## Test of no.effect model: significance = 0.03
## Test of no.effect model: significance = 0.027
## Test of no.effect model: significance = 0.025
## Test of no.effect model: significance = 0.023
## Test of no.effect model: significance = 0.022
ggplot() +
geom_line(data=as.data.frame(st), aes(x=h, y=p), color='#006699', size=1.2) +
geom_hline(aes(yintercept = 0.05),color='#006699', lty=2, size=1, alpha=0.8) +
ggtitle('Significance Trace') +
theme(plot.title = element_text(face='bold', size=32)) +
theme(axis.title = element_text(face='bold', size=22))

mba <- read.table('mbagrade.dat', header = T)
library(ggplot2)
ggplot(data=mba, aes(x=GMAT, y=GPA)) +
  geom_point(color='#8da0cb', size=3) +
  ggtitle('MBA') +
  theme(plot.title = element_text(face='bold', size=22)) +
  theme(axis.title = element_text(face='bold', size=12))
  
library(sm)
smreg <- sm.regression(mba$GMAT,mba$GPA,h=40, model='linear')
df  <- data.frame(eval=smreg$eval.points, est=smreg$estimate, se=smreg$se, model.y=smreg$model.y)
df <- data.frame(df, lwr=df$model.y+2*df$se, upr=df$model.y-2*df$se)
 
ggplot() +
  geom_point(data=mba, aes(x=GMAT, y=GPA), color='#8da0cb', size=3) +
  geom_ribbon(data=df, aes(x=eval,  ymin=lwr, ymax=upr), fill='#fc8d62', alpha=0.2) +
  geom_line(data=df, aes(x=eval, y=est), col='#fc8d62', size=1.2) +
  ggtitle('MBA - Linear Test') +
  theme(plot.title = element_text(face='bold', size=22)) +
  theme(axis.title = element_text(face='bold', size=12))    

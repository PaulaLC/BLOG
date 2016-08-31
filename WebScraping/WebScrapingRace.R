library(rvest)
library(plyr)
library(ggplot2)
 
phases <- c("round-one","quarter-finals","semi-finals","final")
 
df <- data.frame()
for(phase in phases){
olympic <- read_html(paste0("http://www.sports-reference.com/olympics/summer/1936/ATH/mens-100-metres-",phase,".html"))
nodes <- html_nodes(olympic,"#page_content .table_container table")
tables <- html_table(nodes)
df_ <- ldply(tables,function(x) subset(x,select=1:6))
names(df_)[6] <- "time"
df_ <- na.omit(df_)
df_ <- df_[df_$time!="",]
df_$time <- gsub(",",".",df_$time)
df_$phase <- phase
df <- rbind(df_, df)
}
 
df$time <- gsub("w","",df$time)
df$phase <- factor(df$phase, levels=phases)

finalists <- df$Athlete[df$phase=="final"]
dfFinal <- df[df$Athlete %in% finalists,]
 
ggplot(data=dfFinal, aes(y=Athlete, x=time, col=Athlete, group=Athlete, label=Athlete)) +
geom_label(aes(fill = Athlete), colour = "white", fontface = "bold") +
facet_grid(phase~.)+
theme(plot.title = element_text(face="bold", size=14),
legend.position="none",
axis.text.y=element_blank(),
axis.ticks.y=element_blank()) +
ggtitle("Berlin 1936")

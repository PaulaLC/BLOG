library(ggplot2)
library(ggmap)
EQ <- read.csv("all_day.csv")
str(EQ)
EQ <- EQ[grepl("Ecuador", EQ$place),]
EcuadorMap <- qmap("ecuador", zoom=6)
EcuadorMap +
geom_point(aes(x=longitude, y=latitude, size=mag),
data=EQ, color="#ff7f00", alpha=0.7) +
labs(title="Ecuador Earthquakes",x="Longitude",y="Latitude",size="Magnitude")

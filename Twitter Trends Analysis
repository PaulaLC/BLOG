library(twitteR)
library(ggplot2)
library(RColorBrewer)
 
consumer_key <- 'xxxxxxxxxxxxx'
consumer_secret <- 'xxxxxxxxxxxxx'
access_token <- 'xxxxxxxxxxxxx'
access_secret <- 'xxxxxxxxxxxxx'
 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# Download Tweets
tweets <- searchTwitter("#26J", lang="es", n=1000)
 
# Convert to data frame
df <- twListToDF(tweets)
 
ggplot(df, aes(x=created)) +
geom_histogram(aes(y=..count..), fill="#1B9E77", color="#1B9E77", alpha="0.3", size=1.1) +
labs(x="Hour", y="Number of tweets / 1000", title = "Trends #26J") +
theme_minimal()

# Create variable for groups
df$group <- ifelse(grepl("PP", dt.text, ignore.case = T) | +
grepl("Rajoy", dt.text, ignore.case = T), "PP",
ifelse(grepl("PSOE", dt.text, ignore.case = T) | +
grepl("Sanchez", dt.text, ignore.case = T), "PSOE",
ifelse(grepl("CS", dt.text, ignore.case = T) | +
grepl("Rivera", dt.text, ignore.case = T), "CS",
ifelse(grepl("PODEMOS", dt.text, ignore.case = T) | +
grepl("Iglesias", dt.text, ignore.case = T), "Podemos", "Neutral"))))
 
# Set colours
color.df <- brewer.pal(5,"Set1")[c(5, 3, 4, 2, 1)]
 
ggplot(df, aes(x=created, fill=group, colour=group)) +
geom_density(aes(y=..count..*100),adjust=0.5, position="identity", alpha=0.15, size=1) +
labs(x="Hour", y="Number of tweets / 1000", title = "Trends #26J") +
scale_fill_manual(values=color.df) +
scale_colour_manual(values=color.df) +
theme_minimal()

ggplot(df, aes(group, fill=group)) +
geom_bar() +
scale_fill_manual(values=color.df) +
labs(x="Political Forces", y="Number of tweets / 1000", title = "Trends #26J") +
theme_minimal()

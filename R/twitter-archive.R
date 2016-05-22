# This code will process the tweets.csv file that comes with any Twitter user's archive.
# Find an explanation of the code at http://sbaldrich.github.io/blog/exploratory-analysis-of-a-twitter-archive/

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

tw <- read.csv("tweets.csv", stringsAsFactors = FALSE)

# Add a factor feature that describes each tweet as a "real" tweet, a reply, a mention or a mention to myself.
own.id <- 271769778 # yup, that's me.
tw$timestamp <- ymd_hms(tw$timestamp)
tw$type <- "tweet"
tw[(!is.na(tw$in_reply_to_status_id)),"type"] <- "reply"
tw[(!is.na(tw$in_reply_to_status_id) & (tw$in_reply_to_user_id == own.id)),"type"] <- "r.self"
tw[(!is.na(tw$retweeted_status_id)),"type"] <- "RT"
tw[(tw$type == "tweet") & grepl("@", tw$text), "type"] <- "mention"   #Not a bulletproof aproach but good enough for this analysis
tw$type <- as.factor(tw$type)
tw$type <- factor(tw$type, levels(tw$type)[5:1])

# Plot the number of tweets for all history
ggplot(tw, aes(timestamp)) +
geom_histogram(aes(fill = ..count..), bins = 60) +
xlab("Time") + ylab("Number of Tweets")

# Plot the number of tweets broken down by year
ggplot(tw, aes(year(timestamp))) +
geom_histogram(aes(fill = ..count..), breaks = seq(2010.5, 2016, by = 1)) +
xlab("Time") + ylab("Number of Tweets")

# Plot the number of tweets broken down by month
ggplot(data = tw, aes(x = month(timestamp, label = TRUE))) +
geom_bar(aes(fill = ..count.. )) +
xlab("Month") + ylab("Number of Tweets")

# Plot the number of tweets broken down by day
ggplot(data = tw, aes(x = wday(timestamp, label = TRUE))) +
geom_bar(aes(fill = ..count.. )) +
xlab("Day of Week") + ylab("Number of Tweets")

# Plot the number of tweets broken down by hour
tw$timeonly <- as.numeric(tw$timestamp - trunc(tw$timestamp, "days"))
class(tw$timeonly) <- "POSIXct"
ggplot(data = tw, aes(timeonly)) +
geom_histogram(aes(fill = ..count..)) +
theme(legend.position = "none") +
xlab("Time") + ylab("Number of tweets") +
scale_x_datetime(breaks = date_breaks("2 hours"), labels = date_format("%H:00"))

# Plot the number of tweets in all history divided by type
ggplot(tw, aes(timestamp, fill = type)) +
geom_histogram(bins = 60) +
xlab("Time") + ylab("Number of Tweets") +
scale_fill_brewer(palette = "Blues", direction = -1)

# Plot the proportion of tweets in all history divided by type
ggplot(tw, aes(timestamp, fill = type)) +
geom_histogram(position = "fill", bins = 60) +
xlab("Time") + ylab("% of Tweets") +
scale_fill_brewer(palette = "Blues", direction = -1)

# Plot the number of tweets in before March 2013 divided by type
tw_early <- filter(tw, timestamp <= ymd("2013 03 01", tz = "America/Bogota"))
ggplot(tw_early, aes(timestamp, fill = type))  +
geom_histogram(position = "fill", bins = 22) +
xlab("Time") + ylab("% of Tweets") +
scale_fill_brewer(palette = "Blues", direction = -1)

# Plot the distribution of the number of characters on all tweets
tw$charsintweet <- sapply(tw$text, nchar)
ggplot(data = filter(tw, type == "tweet"), aes(x = charsintweet)) +
geom_histogram(aes(fill = ..count..), binwidth = 8) +
xlab("Characters per Tweet") + ylab("Number of tweets")

# Preprocess tweets to analyze word frequencies
library(tm)
library(stringi)

tw <- mutate(tw, text = stri_trans_general(text, "Latin-ASCII"))
stopwords <- c(stopwords("spanish"), stopwords("english"))

corpus <- Corpus(VectorSource(filter(tw,type != "RT")$text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords)
corpus <- tm_map(corpus, PlainTextDocument)

dtm <- DocumentTermMatrix(corpus)
freq <- colSums(as.matrix(dtm))
word.freq <- data.frame(word = names(freq), freq = freq)

# Plot the 50 most commond words among tweets
ggplot(arrange(word.freq, desc(freq)) %>% top_n(50)) + aes(word, freq) +
geom_bar(stat="identity", aes(fill = freq)) +
theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none") +
xlab("Word") + ylab("Number of times")

# Plot a word cloud of words that appear at least 25 times

library(wordcloud)
set.seed(42) # The answer the ultimate question of Life, the Universe and Everything
pal <- brewer.pal(9, "Blues")
pal <- pal[-(1:4)]
wordcloud(word.freq$word, word.freq$freq, min.freq=25, color = pal)

# Remove handles and do the same analysis as before

handle.pattern <- "@[a-zA-Z0-9_]+"

corpus.nh <- Corpus(VectorSource(
  (filter(tw, type != "RT") %>%
    mutate(text=stri_replace_all(text, "", regex=handle.pattern)))$text
))
corpus.nh <- tm_map(corpus.nh, removePunctuation)
corpus.nh <- tm_map(corpus.nh, tolower)
corpus.nh <- tm_map(corpus.nh, trimws)
corpus.nh <- tm_map(corpus.nh, removeWords, stopwords)
corpus.nh <- tm_map(corpus.nh, PlainTextDocument)

dtm.nh <- DocumentTermMatrix(corpus.nh)
freq.nh <- colSums(as.matrix(dtm.nh))
word.freq.nh <- data.frame(word = names(freq.nh), freq = freq.nh)

ggplot(arrange(word.freq.nh, desc(freq)) %>% top_n(50)) + aes(word, freq) +
  geom_bar(stat="identity", aes(fill = freq)) +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position = "none") +
  xlab("Word") + ylab("Number of times")

wordcloud(word.freq.nh$word, word.freq.nh$freq, min.freq=25, color = pal)

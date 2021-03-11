library(rtweet)
library(tm)
library(ggplot2)
library(ggthemes)
library(tm)
library(qdap)
library(wordcloud)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(plyr)
library(dbplyr)


# Connecting APIs
api_key <- "HIDDEN"
api_secret_key <- "HIDDEN"
access_token <- "HIDDEN"
access_token_secret <- "HIDDEN"

token <- create_token(
  app = "HIDDEN",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)


# Options & Functions
options(stringsAsFactors = FALSE) 
Sys.setlocale('LC_ALL','C') 

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


rt <- search_tweets("seafood", geocode = lookup_coords("usa"), n = 15000,  header=TRUE, token = token)
min(rt$created_at)
max(rt$created_at)


stops <- c(stopwords("SMART"),
           "how","will","i","me","are","is","you","of","your","of","to"
           ,"what","a","were","have","but","im","am","food","day","great","night"
           ,"week","amp","lol","open","year","eating","find","restaurant",
           "brexitrelated","black","member","brexit","government",
           "tip","desi","weekend","fuck","ticket","prime","march","life"
           ,"wow","shit","industry","seafood","dob","people","tonight",
           "related","marcoup","makes","show","apology")


corpus<-Corpus(VectorSource(rt$text))
corpus<-tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removeWords, stops)
corpus<-tm_map(corpus, removePunctuation)

tweetTDM  <- TermDocumentMatrix(corpus)
tweetTDMm <- as.matrix(tweetTDM)

# Inspect word associations
associations <- findAssocs(tweetTDM, 'protein', 0.20)
associations

# Inspect word associations
associations <- findAssocs(tweetTDM, 'health', 0.20)
associations

# Inspect word associations
associations <- findAssocs(tweetTDM, 'pork', 0.20)
associations



# Inspect word associations
associations <- findAssocs(tweetTDM, 'sustainable', 0.10)
associations


# Get Row Sums & organize
tweetsTDMv <- sort(rowSums(tweetTDMm), decreasing = TRUE)
tweets_DF   <- data.frame(word = names(tweetsTDMv), freq = tweetsTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweets_DF$word,
          tweets_DF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

# End
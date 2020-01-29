### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

rt_combi = readRDS("twitter_data/rt_filt_censor.rds")

library(stringi) # For text manipulation
library(tidytext)
library(tm) # Framework for text mining
library(tidyr)
library(dplyr)
library(quanteda)
library(ggplot2)

#removing duplicates because, w/ no retweets, we will assume that large number of duplicates means bots
my_corpus <- rt_combi$text %>% 
  unique %>%
  VectorSource %>%
  VCorpus %>%
  corpus

twitter.td <- tidy(my_corpus)

# add search words to stop word list for word cloud
add_row(stop_words,word="censor",lexicon="custom") -> stop_words.custom
add_row(stop_words.custom,word="censorship",lexicon="custom") -> stop_words.custom
#add_row(stop_words.custom,word="shadowban",lexicon="custom") -> stop_words.custom

twitter.td %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words.custom) -> twitter.words

twitter.dfm <- dfm(my_corpus, remove=stop_words.custom$word)
textplot_wordcloud(twitter.dfm, max_words=100)

kwic(my_corpus, pattern="shadow") %>%
  textplot_xray()

# taking a long time ??
textplot_xray( # create multiple lexicon dispersion plots
  kwic(my_corpus, pattern = "shadow"),
  kwic(my_corpus, pattern = "censorship")
)

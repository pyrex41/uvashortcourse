### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

rt_combi = readRDS("twitter_data/rt_combi.rds")

library(tidyr)
library(dplyr)
library(stringi) # For text manipulation
library(tm) # Framework for text mining
library(tidytext) # Sentiment dictionaries
library(ggplot2)

#removing duplicates because, w/ no retweets, we will assume that large number of duplicates means bots
my_corpus <- rt_combi$text %>% 
  unique %>%
  VectorSource %>%
  VCorpus

tcorp = tidy(my_corpus)

twitter.words <- tcorp %>% # convert the corpus to document-word
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Plot top 15 most frequently used words in each category
twitter.words %>%  
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  facet_wrap(~sentiment, scales = "free_y") +
  ylim(0, 300) +
  labs(y = NULL, x = NULL) +
  coord_flip()


library(reshape2)
library(wordcloud)


twitter.words %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

twitter.words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 10)

# Now let's look at specific sentiment
twitter.words %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), 
                          "Positive", "Negative")) %>%
  ggplot(aes(reorder(sentiment, n), n)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()

# Bigrams
twitter.words.2 <- tcorp %>%
  tidytext::unnest_tokens(two_gram, text, token = "ngrams", n = 2) %>%
  count(two_gram, sort = TRUE) %>%
  top_n(20) %>%
  mutate(five_gram = reorder(two_gram, n)) %>%
  ggplot(aes(two_gram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()
twitter.words.2

dtm <- DocumentTermMatrix(my_corpus,
                          control = list(tolower = TRUE,
                                         stopwords = TRUE,
                                         removeNumbers = TRUE,
                                         removePunctuation = TRUE,
                                         stemming = TRUE))


m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = TRUE)
custom.stop <- rownames(as.data.frame(head(v, 1000)))

for (i in 1:length(df.world)) {
  df.world$text[i] <- removeWords(df.world$text[i], custom.stop)
}

# not using this yet
rt %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

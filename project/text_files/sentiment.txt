### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

data1 = readRDS("censor_filt.rds")

library(tidyr)
library(dplyr)
library(stringi) # For text manipulation
library(tm) # Framework for text mining
library(tidytext) # Sentiment dictionaries
library(ggplot2)

#removing duplicates because, w/ no retweets, we will assume that large number of duplicates means bots
data1$text %>% 
  unique %>%
  VectorSource %>%
  VCorpus %>%
  tidy -> my_corpus1

#custom stop
custom_stop_words <- stop_words
add_row(custom_stop_words, word='censorship', lexicon='custom') -> custom_stop_words
add_row(custom_stop_words, word='https', lexicon='custom') -> custom_stop_words
add_row(custom_stop_words, word='t.co', lexicon='custom') -> custom_stop_words

my_corpus1 %>% # convert the corpus to document-word
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) -> twitter.words.sans

my_corpus1 %>% # convert the corpus to document-word
  unnest_tokens(word, text) %>%
  anti_join(stop_words) -> twitter.words1

# Plot top 15 most frequently used words in each category
twitter.words1 %>%
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
  ylim(0, 600) +
  labs(y = NULL, x = NULL) +
  coord_flip()


library(reshape2)
library(wordcloud)

# word cloud
twitter.words.sans %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# more funner word cloud
twitter.words1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red2", "green3"),
                   max.words = 100)

# Now let's look at specific sentiment
twitter.words1 %>%
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
 my_corpus1 %>%
  tidytext::unnest_tokens(two_gram, text, token = "ngrams", n = 2) %>%
  count(two_gram, sort = TRUE) %>%
  top_n(20) %>%
  mutate(two_gram = reorder(two_gram, n)) %>%
  ggplot(aes(two_gram, n)) +
  geom_col(fill = "red", show.legend = FALSE) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip()
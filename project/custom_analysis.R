### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

# be sure to set working directory first
rt_combi = readRDS("censor_filt.rds")


library(stm)
library(tidyr)
library(dplyr)
library(stringi) # For text manipulation
library(tm) # Framework for text mining
library(tidytext) # Sentiment dictionaries
library(ggplot2)

# cut down common words first
docs <- VCorpus(VectorSource(rt_combi$text))

# preprocess and create DTM
dtm <- DocumentTermMatrix(docs,
                          control = list(tolower = TRUE,
                                         stopwords = TRUE,
                                         removeNumbers = TRUE,
                                         removePunctuation = TRUE,
                                         stemming = TRUE))


# turn DTM into dataframe
dtm.m <- as.data.frame(as.matrix(dtm))
dtm.m$that <- NULL # fix weird stopword issue.

dtm.m.censorship <- dtm.m[dtm.m$censorship == 1,]
dtm.m.ban <- dtm.m[dtm.m$ban == 1,]

# function to get percentages of each associated word
map_percentages <- function(X,n) {
  X[1:n+1] * 100 / X[[1]]
}

dtm.m.censorship %>% colSums -> sums
sums[sums > 0] %>%
  sort(decreasing = TRUE)  %>% map_percentages(10) -> sum.perc
sum.perc



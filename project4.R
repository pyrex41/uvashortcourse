### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

rt_combi = readRDS("twitter_data/rt_filt_censor.rds")

rt_combi = readRDS("twitter_data/rt_filt_censor.rds")

library(stm)

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

dtm.m.shadowban <- dtm.m[dtm.m$shadowban == 1,]
dtm.m.censorship <- dtm.m[dtm.m$censorship == 1,]
dtm.m.ban <- dtm.m[dtm.m$ban == 1,]


cast_percentages <- function(X,n) {
  X[1:n+1] / X[[1]]
}

dtm.m.shadowban %>% colSums -> shadow.sums
shadow.sums[shadow.sums > 0] %>%
  sort(decreasing = TRUE) -> shadow.sums

shadow.sums %>% cast_percentages(10)

dtm.m.censorship %>% colSums ->censorship.sums
censorship.sums[censorship.sums > 0] %>%
  sort(decreasing = TRUE) -> censorship.sums

censorship.sums %>% cast_percentages(10)

dtm.m.ban %>% colSums ->ban.sums
ban.sums[ban.sums > 0] %>%
  sort(decreasing = TRUE) -> ban.sums

ban.sums %>% cast_percentages(10)

### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

rt_combi = readRDS("twitter_data/rt_filt_smcensor.rds")

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
m <- as.matrix(dtm)
v <- sort(colSums(m), decreasing = TRUE)
custom.stop <- rownames(as.data.frame(head(v, 1000)))

for (i in 1:length(rt_combi)) {
  rt_combi$text[i] <- removeWords(rt_combi$text[i], custom.stop)
}

temp <- textProcessor(documents = rt_combi$text, 
                      metadata = rt_combi)
#                      customstopwords = custom.stop)

# pre-process
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents
# prep documents in correct format
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# estimate w/ 3 topics
model <- stm(docs, vocab, 3, data = meta, seed = 15)

# plot model
plot(model)

#top words
labelTopics(model)

#example?
findThoughts(model, texts = meta$text, n = 1, topics = 1)

# plot
plot(model, type = "perspectives", topics = c(2,3)) # Topics #1 and #10

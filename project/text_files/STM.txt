### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

shadow_ban = readRDS("shadow_ban_filt.rds")
censor = readRDS("censor_filt.rds")

library(stm)

# cut down common words first
# rerun everything below this twice, replacing "shadow_ban" w/ "censor" in the next line:
corp = censor
docs <- VCorpus(VectorSource(corp$text))

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

for (i in 1:length(corp)) {
  corp$text[i] <- removeWords(corp$text[i], custom.stop)
}

temp <- textProcessor(documents = corp$text, 
                      metadata = corp,
                      customstopwords = custom.stop)

# pre-process
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents
# prep documents in correct format
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# estimate w/ 4 topics
model <- stm(docs, vocab, 3, data = meta, seed = 15)

# plot model
plot(model)

# estimate w/ 4 topics
model2 <- stm(docs, vocab, 5, data = meta, seed = 15)

# plot model
plot(model2)

#top words
labelTopics(model)
labelTopics(model2)

#example?
findThoughts(model, texts = meta$text, n = 1, topics = 1)

# plot
plot(model, type = "labels", topics = c(1,2,3))
plot(model2, type = "labels", topics = c(1,2,3,4,5)) 

plot(model, type = "hist",topics=c(1,2,3), n=3)
plot(model2, type = "hist", topics = c(1,2,3,4,5)) 

# This unit gives a brief overview of the `stm` (structural topic model) package. Please read the vignette for more detail.

# Structural topic model is a way to estimate a topic model that includes document-level meta-data. One can then see how topical prevalence changes according to that meta-data.

###############
### Setup R ###
###############

### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

### Load library packages
library(stm)

# The data we'll be using for this unit consists of all world constitutions.

# Load the data. Notice that we have the text of the articles in 'docs', along with some metadata in 'meta'.
load("materials2/course/world-constitutions.RData")
summary(df.world)
summary(nchar(df.world$text))

# STM has its own unique preprocessing functions and procedure, which I've coded below. Notice that we're going to use the `content` column, which contains all the text of the press releases.

# Pre-process
temp <- textProcessor(documents = df.world$text, metadata = df.world)
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents
# prep documents in correct format
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

### **Exercise 1**

# Read the help file for the `prepDocuments` function. Alter the code above (in 2.1) to keep only words that appear in at least 10 documents.

# We're now going to estimate a topic model with 15 topics by regressing topical prevalence on a year covariate. 
model <- stm(docs, vocab, 8, prevalence = ~ year, data = meta, seed = 15)

# Let's see what our model came up with! The following tools can be used to evaluate the model. 
# - `labelTopics` gives the top words for each topic. 
# - `findThoughts` gives the top documents for each topic (the documents with the highest proportion of each topic)

# Plot model
plot(model)

### Hmm. A lot of really common words here. A lot of noise. Let's think about how to reduce that. One thing that we could do is remove the most common words in the corpus.
# create corpus
docs <- tm::VCorpus(VectorSource(df.world$text))
docs
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

for (i in 1:length(df.world)) {
  df.world$text[i] <- removeWords(df.world$text[i], custom.stop)
}

temp <- textProcessor(documents = df.world$text, 
                      metadata = df.world,
                      customstopwords = custom.stop)
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents
# prep documents in correct format
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

model <- stm(docs, vocab, 25, prevalence = ~ year, data = meta, seed = 15)
plot(model)

# Top Words
labelTopics(model)
# Example Docs
findThoughts(model, texts = meta$text, n = 1, topics = 1)

# Let's plot some aspects of the model
plot(model, type = "perspectives", topics = c(5, 7)) # Topics #1 and #10

plot(model, type = "hist")

### **Exercise 2**

# Estimate other models using 5 and 40 topics, respectively. Look at the top words for each topic. How do the topics vary when you change the number of topics?
# Now look at your neighbor's model. Did you get the same results? Why or why not?

### **Exercise 3**

# Using the functions `labelTopics` and `findThoughts`, hand label the 15 topics. Hold these labels as a character vector called `labels`
# Now look at your neighbor's labels. Did you get the same results? Why or why not?

# We're now going to see how the topics compare in terms of their prevalence across time. 

# Corpus Summary
plot.STM(model, type = "summary", main = "")
# Estimate Covariate Effects
prep <- estimateEffect(1:25 ~ year, model, meta = meta, uncertainty = "Global", documents=docs)
summary(prep)

plot(prep, covariate = "year", topics = 25)
plot(prep, "year", method = "continuous", topics = 1:25)

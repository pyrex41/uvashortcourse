library(readtext)

substrRight <- function(x, n){
  sapply(x, function(xx)
    substr(xx, (nchar(xx)-n+1), nchar(xx))
  )
}

### Preprocess national constitutions
setwd("~/Dropbox/cope-crabtree/text analysis course/constitutions/world constitutions/")
files.world <- list.files("~/Dropbox/cope-crabtree/text analysis course/constitutions/world constitutions/")
years.world <- as.vector(gsub(".txt", "", substrRight(files.world, 8)))
countries.world <- gsub('.{9}$', '', files.world)

texts.world <- NULL
i <- 1
for (i in 1:length(files.world)) {
  texts.world[[i]] <- readtext::readtext(files.world[i])$text
}
texts.world <- unlist(texts.world)
texts.world <- gsub(pattern = "<.*>", replacement = "", x = texts.world)
texts.world <- textclean::add_comma_space(texts.world)
texts.world <- textclean::replace_html(texts.world)
texts.world <- textclean::replace_url(texts.world)
texts.world <- textclean::replace_white(texts.world)
texts.world <- textclean::replace_curly_quote(texts.world)
texts.world <- textclean::replace_symbol(texts.world)
texts.world <- textclean::replace_misspelling(texts.world)
texts.world <- textclean::replace_non_ascii(texts.world, replacement = "", remove.nonconverted = TRUE)
texts.world <- tolower(texts.world)

set.seed(1986)
sample.texts <- sample(texts.world, 6)
sample.texts[2]

texts.world <- gsub('[[:digit:]]+', '', texts.world)
texts.world <- gsub('signed', '', texts.world)
texts.world < str_replace_all(texts.world, "[^[:alnum:]]", " ")

df.world <- tibble::tibble(country = as.character(countries.world),
                              year = as.numeric(years.world),
                              text = as.character(texts.world))
head(df.world)

# df.world.articles <- strsplit(df.world$text, ". article")

save(df.world, file = "../world-constitutions.RData")

### Preprocess national constitutions
setwd("~/Dropbox/cope-crabtree/text analysis course/constitutions/state constitutions/")
files.states <- list.files("~/Dropbox/cope-crabtree/text analysis course/constitutions/state constitutions/")
years.states <- as.vector(gsub(".txt", "", substrRight(files.states, 8)))
states <- gsub('.{9}$', '', files.states)

texts.states <- NULL
i <- 1
for (i in 1:length(files.states)) {
  texts.states[[i]] <- readtext::readtext(files.states[i])$text
}
texts.states <- unlist(texts.states)
texts.states <- gsub(pattern = "<.*>", replacement = "", x = texts.states)
texts.states <- textclean::add_comma_space(texts.states)
texts.states <- textclean::replace_html(texts.states)
texts.states <- textclean::replace_url(texts.states)
texts.states <- textclean::replace_white(texts.states)
texts.states <- textclean::replace_curly_quote(texts.states)
texts.states <- textclean::replace_symbol(texts.states)
texts.states <- textclean::replace_non_ascii(texts.states, replacement = "", remove.nonconverted = TRUE)
texts.states <- tolower(texts.states)
head(texts.states)

texts.states <- gsub('[[:digit:]]+', '', texts.states)
texts.states <- gsub('signed', '', texts.states)
texts.states < gsub(' th ', '', texts.states)
texts.states < stringr::str_replace_all(texts.states, "[^[:alnum:]]", " ")
texts.states <- gsub("\\[[^\\]]*\\]", "", texts.states, perl = TRUE)

set.seed(1986)
sample.texts <- sample(texts.states, 6)
sample.texts[2]

df.states <- tibble::tibble(country = as.character(states),
                           year = as.numeric(years.states),
                           text = as.character(texts.states))
head(df.states)

table(df.states$year)

save(df.states, file = "../state-constitutions.RData")

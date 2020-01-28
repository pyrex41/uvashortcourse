### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

library(rtweet)

## authenticate via access token
token <- create_token(
  app = "research_uva_law",
  consumer_key = "fZtNt3Sw1o6wCJJLNVvjbTqXQ",
  consumer_secret = "0989iZLbmgiB0r8YUYO7xFcj3OLNNjc3wW0yT6NUrn3ZueTrwr",
  access_token = "26522197-DwVCEuz4IZDXU52K9WYbuhShvs7q8idtLKqyEZ0Hq",
  access_secret = "IXpGYVMxPEJvjPBTQMIi2YBMY6GXAbczPBxzYTntRQdaP")

## search query terms
search_query = "shadowban lang:en"
# from date
fd = "202001010000"
# to date
td = "202001270000"
# from my twttier developer settings
env_name <- "dev"
# where to save data; should create in working directory if doesn't exist
safedir = 'twitter_data'

# this works, and extracts the next_page token
rt1 <- search_30day(
  search_query, 
  n=100,
  fromDate = fd,
  toDate = td,
  env = "dev",
  parse = TRUE,
  token = token
)
np = attr(rt1, 'next_page')[[1]]


# So next step, went to github, looked inside search_30day function, saw it was really a 
# wrapper around the general search_tweets function passing some special params. So I
# tried to deconstruct and go that route: 
# pulled these functions out of github to deconstruct the search_30day function
# COPIED FROM GITHUB:
# =======================================================================
premium_api <- function(...) {
  dots <- c(...)
  if (length(dots) == 1) {
    dots <- dots[[1]]
  }
  path <- grep("30day|fullarchive", dots, value = TRUE)
  env_name <- grep("30day|fullarchive", dots, value = TRUE, invert = TRUE)
  list(path = path, env_name = env_name)
}
get_next_page <- function(x) {
  if (inherits(x, "response") || isTRUE("response" %in% names(attributes(x)))) {
    attr(x, "next_page")
  } else if (length(x) > 0 && isTRUE(inherits(x[[1]], "response"))) {
    lapply(x, attr, "next_page")
  } else if (is.null(names(x))) {
    lapply(x, "[[", "next")
  } else {
    x[["next"]]
  }
}

# =======================================================================
# my code now
rt_combi <- rt1

for (i in 1:2000) {
  np <- attr(rt_combi, 'next_page')[[1]]
  rt_new <- search_tweets(search_query,
                          fromDate = fd,
                          toDate = td,
                          premium = premium_api("30day", env_name),
                          parse = TRUE, n = 100,
                          safedir = safedir,
                          token=token,
                          'next'= np)
  np_new <- get_next_page(rt_new)
  rt_new <- tweets_with_users(rt_new)
  rt_combi <- rbind(rt_combi, rt_new)
  attr(rt_combi, 'next_page') <- np_new[[1]]
  Sys.sleep(2)
}

rt_combi <- search_tweets(search_query,
                          n=18000,
                          parse = TRUE,
                          safedir = safedir,
                          token=token)
saveRDS(rt_combi,paste(safedir,"/rt_combi.rds",sep=''))

rt_combi = readRDS("twitter_data/rt_combi.rds")

min_id = rt_combi$status_id %>% min

for (i in 1:5) {
  r_new <- search_tweets(search_query,
                         n=18000,
                         parse = TRUE,
                         safedir = safedir,
                         token=token,
                         max_id = min_id)
  rt_combi <- rbind(rt_combi, r_new)
  min_id <- rt_combi$status_id %>% min
}
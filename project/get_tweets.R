### Clear space
rm(list = ls())

### Clear terminal
cat("\014")

library(rtweet)

## authenticate via access token
## do not abuse lol; will need to redact if i ever start paying for access
token <- create_token(
  app = "research_uva_law",
  consumer_key = "fZtNt3Sw1o6wCJJLNVvjbTqXQ",
  consumer_secret = "0989iZLbmgiB0r8YUYO7xFcj3OLNNjc3wW0yT6NUrn3ZueTrwr",
  access_token = "26522197-DwVCEuz4IZDXU52K9WYbuhShvs7q8idtLKqyEZ0Hq",
  access_secret = "IXpGYVMxPEJvjPBTQMIi2YBMY6GXAbczPBxzYTntRQdaP")

## search query terms
search_query = "shadowban lang:en"
search_query2 = "censorship lang:en"

# from date
fd = "202001010000"
# to date
td = "202001270000"
# from my twttier developer settings
env_name <- "dev"
# where to save/backup data; should create in working directory if doesn't exist
safedir = 'twitter_data'

# these will need to run for a while, since every 18k tweets they have to wait for 15min
search_query %>%
  search_tweets(
     n=50000,
     parse = TRUE,
     safedir = safedir,
     token=token,
     retryonratelimit = TRUE) -> res

search_query2 %>%
  search_tweets(
    n=50000,
    parse = TRUE,
    safedir = safedir,
    token=token,
    retryonratelimit = TRUE) -> res2
# remove retweets
res[res$is_retweet == FALSE,] -> res_filter
res2[res2$is_retweet == FALSE,] -> res_filter2


saveRDS(res_filter,paste(safedir,"/shadow_ban_filt.rds",sep=''))
saveRDS(res_filter2,paste(safedir,"/censorship_filt.rds",sep=''))

# For future use: to use the search_30 or search_full_archive functionality.
# Need to pay to use more than 5k tweets / month
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
# my code now ;; PROBLEM SOLVED; see below
rt_combi <- rt1

for (i in 1:20) { # probably need a lot more than 20 iterations
  np <- attr(rt_combi, 'next_page')[[1]]
  rt_new <- search_tweets(search_query,
                          fromDate = fd,
                          toDate = td,
                          premium = premium_api("30day", env_name),
                          parse = TRUE, n = 100,
                          safedir = safedir,
                          token=token,
                          'next'= np) # <--- PROBLEM SOLUTION: have to put 'next' in quotes becuase next is also a function
  np_new <- get_next_page(rt_new)
  rt_new <- tweets_with_users(rt_new)
  rt_combi <- rbind(rt_combi, rt_new)
  attr(rt_combi, 'next_page') <- np_new[[1]]
  Sys.sleep(2) # due to twitter rate limits; adjust as needed depending on API access limits
}

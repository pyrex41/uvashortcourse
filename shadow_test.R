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
s1 = "shadowban lang:en"
# from date
fd = "202001010000"
# to date
td = "202001210000"
# from my twttier developer settings
env_name <- "dev"

# this works, and extracts the next_page token
rt1 <- search_30day(
  s1, 
  n=50,
  fromDate = fd,
  toDate = td,
  env = "dev",
  parse = TRUE
)
s1 = attr(rt1, 'next_page')[[1]]

# this doesn;t work; next / cursor not implemented
rt2 <- search_30day(
  s1, 
  n=100,
  fromDate = "202001010000",
  toDate = "202001210000",
  env = "dev",
  parse = TRUE,
  cursor = s1
)

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
# create this in working directory if not there
safedir = 'premium-20200124'

# this seems to work, for 1st page
r <- search_tweets(s1,
                   fromDate = fd,
                   toDate = td,
                   premium = premium_api("30day", env_name),
                   parse = FALSE, n = 100,
                   safedir = safedir,
                   token = token)
np <- get_next_page(r)
r <- tweets_with_users(r)
attr(r, 'next_page') <- np[[1]]

# But this this does not work!! cursor not recognized for premium apps... but it should:
# https://developer.twitter.com/en/docs/tweets/search/overview/premium
# see fundamental api characteristics header
r2 <- search_tweets(s1,
                    fromDate = fd,
                    toDate = td,
                    premium = premium_api("30day", env_name),
                    parse = FALSE, n = 100,
                    safedir = safedir,
                    token = token,
                    cursor = attr(r, 'next_page'))
np <- get_next_page(r2)
r2 <- tweets_with_users(r2)
attr(r2, 'next_page') <- np[[1]]

# this seems like it might twork but it doesn't access premium endpoint, and is limited to 9 days
rt3 <- search_tweets(
  s1, 
  n=100,
  include_rts=FALSE,
  fromDate = "202001010000",
  toDate = "202001210000",
  env = "dev",
  parse = TRUE,
  cursor = np[[1]]
)

## what to do !? Seems like the rtweet does not implement pagination at all for longer date-range searches
# I can try using python api?


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

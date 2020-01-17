library(rtweet)

## authenticate via access token
token <- create_token(
  app = "premium_app_cc",
  consumer_key = "kDbhxt4YrSflQ8KRDQNI7wgkJ",
  consumer_secret = "cKWjxM9KSKbNFf6y9KgblwPZm6FnOGm9goG5dFcWWgHejqrbZX",
  access_token = "6219402-R9CkaB4LrJ6gKIAju7GLaXj1r2LR8TGX95770bv4kH",
  access_secret = "IeSKlMzcCWzS2lNDvSUOyyTblvbojznnbHvKNuVgT7Iaw")

## search for 18000 tweets using the rstats hashtag
rt <- search_tweets(
  "#rstats", n = 18000, include_rts = FALSE
)

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

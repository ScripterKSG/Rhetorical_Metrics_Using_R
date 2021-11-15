install.packages("devtools")
devtools::install_github("mkearney/rtweet")

library(rtweet)
library(httr)

### Put project name, consumer and secret keys, access token and secret
### Tokens have been stripped away in this instance but are saved on my end

#appname <- ""
#key <- ""
#secret <- ""
#atoken <- ""
#asecret<- ""

### create token

#twitter_token <- create_token(
#  app = appname,
#  consumer_key = key,
#  consumer_secret = secret,
#  access_token = atoken,
#  access_secret = asecret)


### Save token in RDS< read token
saveRDS(twitter_token, file = "API")

readRDS(file = "API")

## search for 1000 tweets using the hololive hashtag
rt <- search_tweets("hololive", n = 1000)

## preview tweets data
rt

## preview users data
users_data(rt)

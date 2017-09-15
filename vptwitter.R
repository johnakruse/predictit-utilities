library(twitteR)
library(lubridate)

vp <- userFactory$new(screenName="vp", name="Mike Pence")

tue_market <- function() {
    last_tuesday <- as.POSIXct(Sys.time() - wday(Sys.time() + 4))
    hour(last_tuesday) <- 12

    vptweets <- userTimeline(vp, 300)
    vptweets <- twListToDF(vptweets)
    sum(vptweets$created > last_tuesday)
    }

fri_market <- function() {
    last_friday <- as.POSIXct(Sys.time() - wday(Sys.time() + 1))
    hour(last_friday) <- 12

    vptweets <- userTimeline(vp, 300)
    vptweets <- twListToDF(vptweets)
    sum(vptweets$created > last_friday)
    }


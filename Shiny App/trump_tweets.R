library(twitteR)

consumer_key <- "kgLaOf2CvcZsDtu3ZHNUb7zsC"
consumer_secret <- "6slMT02jt7RhbpDXone7k69PBGerx2UlkePXUzWcdHZTtPiJ5h"
access_token <- "1060246347096510464-fGsd5FgCqBQvGBEwUxNEM7KGGb6CyR"
access_secret <- "QZTw6yLnCEIWT67etKWTyN9OdUby1zwmKBp1ZAvqJgu2y"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

trump = userTimeline("realdonaldtrump", n = 200)

trump2 = twListToDF(trump)

trump2 = trump2 %>%
  select(text, created)

trump2$created = as.Date(trump2$created)

write.csv(trump2, "donald_trump.csv")

#----------------------------------------------------------------
#Example of how to use functions
#---------------------------------------------------------------
#loads and tidies data
url_example = "https://twitter.com/realDonaldTrump"
num = 25
my_machine = "windows"

# #needs docker
# my_data =
#   twitter_dynamic_download(url = url_example, num_scrolls = num, machine = my_machine) %>%
#   twitter_tidy_data()

#doesn't need docker
my_data = readRDS('tidy_data.rds')

#words we want to remove from lexicon
removed_words = c("trump", "grand", "like")

#graphs you can output
most_common_words(my_data, 10)
num_tweets_per_day(my_data, "bing", removed_words)
tweet_percent(my_data, "nrc", removed_words)
waffleFun(my_data, "nrc", removed_words, 50)
word_count_chart(my_data, "bing", removed_words, "fake", 10)


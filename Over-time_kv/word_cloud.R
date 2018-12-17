word_cloud = function(data, sent = c("positive", "negative")){
  data_sent = data %>%
    inner_join(get_sentiments("bing"))
  
  if (sent == "positive"){
    data_filt = data_sent %>%
      filter(sentiment == "positive")
  }else{
    data_filt = data_sent %>%
      filter(sentiment == "negative")
  }
  
  words = data_filt %>%
    group_by(word) %>%
    count(word)
  
  tidy_words <- words %>%
    anti_join(stop_words)
  
  tidy_words = tidy_words %>%
    filter(word != "pic.twitter.com" &
             word != "https" &
             word != "http" &
             word != "twitter.com" &
             word != "trump")
  
  wordcloud(words = tidy_words$word, freq = tidy_words$n, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.15,
            colors=brewer.pal(8, "Dark2"))
}
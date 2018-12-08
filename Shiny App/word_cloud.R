make_word_cloud = function(data){
  words = data %>%
    group_by(word) %>%
    count(word)

  tidy_words <- words %>%
    anti_join(stop_words)

  tidy_words = tidy_words %>%
    filter(word != "pic.twitter.com" &
             word != "https" &
             word != "http" &
             word != "twitter.com")

  wordcloud(words = tidy_words$word, freq = tidy_words$n, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.15,
            colors=brewer.pal(8, "Dark2"))
}

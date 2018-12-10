top_words_sent = function(data, lexicon){
  word_counts <- data %>%
    inner_join(get_sentiments(lexicon)) %>%
    # Count by word and sentiment
    count(word, sentiment)

  #find top words per sentiment
  top_words <- word_counts %>%
    group_by(sentiment) %>%
    top_n(20) %>%
    ungroup() %>%
    mutate(word = reorder(word, n))

  #bar chart of negative and positive words
  ggplot(top_words, aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Top words per sentiment", x = "Number", y = "Word")+
    facet_wrap(~sentiment, scales = "free") +
    coord_flip()
}

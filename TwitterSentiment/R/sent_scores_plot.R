sent_scores_plot = function(data, lexicon){
  word_counts_nrc = data %>%
  inner_join(get_sentiments(lexicon)) %>%
  #Count by word and sentiment
  count(word, sentiment)
  sent = word_counts_nrc %>%
    group_by(sentiment) %>%
    count()
  ggplot(sent, aes(sentiment, nn, fill = sentiment)) +
    geom_col() +
    xlab("Sentiment") +
    ylab("Number") +
    ggtitle("Total sentiment scores")
}

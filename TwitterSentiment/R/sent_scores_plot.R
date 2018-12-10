#Count by word and sentiment for "nrc" or "bing
sent_scores_plot = function(data, lexicon)
  {
    word_counts = 
      data %>%
        inner_join(get_sentiments(lexicon)) %>%
        count(word, sentiment)
    
    sent = 
      word_counts %>%
      group_by(sentiment) %>%
      count()
    
    ggplot(sent, aes(sentiment, nn, fill = sentiment)) +
      geom_col() +
      xlab("Sentiment") +
      ylab("Number") +
      ggtitle("Total sentiment scores")
}

histogram_of_scores = function(data, dropwords)
{   lexicon = "afinn"
    afinn_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "afinn" lexicon
    sentiment_afinn = 
      data %>%
      inner_join(afinn_lexicon)
    
    #Histogram of scores
    histogramGraph = 
      sentiment_afinn %>% 
      group_by(score) %>%
      ggplot(aes(x = score)) + geom_density(color = "black", fill = "indianred3",  alpha = 0.5 ) + 
      labs(title = "Histogram of Scores", x = "Score", y = "Density") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
  return(histogramGraph)

}


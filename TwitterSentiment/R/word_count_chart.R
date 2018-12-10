word_count_chart = function(data, lexicon = c("bing", "afinn", "nrc"), dropwords, lookup_word, top_n_words)
{
  lexicon = match.arg(lexicon)
  lex <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
  
  #Analysis Using "nrc" lexicon
  sent = 
    data %>%
    inner_join(lex)

#Provides dataset of word count
dataTest = pullWordCount(data = sent, word, lookup_word)

#Makes a visual graph of the word count
graph = 
  dataTest[1:top_n_words,] %>%
  ggplot(aes(x = n, y=n,label = word, size = n, col = n)) + 
  geom_text(position = "jitter", check_overlap = TRUE) +
  geom_label(data = dataTest[which(dataTest$word == lookup_word),], aes(x = n, y = n, label = word)) +
  scale_colour_gradient(low = "lightcoral", high = "royalblue3") +
  scale_x_continuous("Word Usage Count") +
  scale_y_continuous(("Word Usage Count")) +
  theme(rect = element_blank()) +
  theme(plot.background = element_rect(fill = "aliceblue"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
  labs(size = "Count by Size", col = "Count by Color") 
return(graph)
}

waffleFun <- function(data, dropwords, lexicon = c("bing", "afinn", "nrc"), num_Rows)
  {
    lexicon = match.arg(lexicon)
    if(lexicon == "afinn")
    {
      lex <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
      
      #Analysis of "bing" or "nrc lexicon
      sent = 
        data %>%
        inner_join(lex)
    
      #Data for Waffle Graph
      dataAfinnGrouped <- sent %>%
        group_by(score) %>%
        summarise(n = n())
      
      values = dataAfinnGrouped$n
      names = dataAfinnGrouped$score
      
      #Waffle graph 
      val_names <- sprintf("%s (%s)", names, percent(round(values/sum(values), 2)))
      names(values) <- val_names
      graph = 
        waffle(values, title = "Afinn Sentiments", rows = num_Rows, colors = c('gold', 'hotpink3', 'mediumturquoise', 'purple', 'midnightblue', 'mediumvioletred', 'plum', 'red', 'steelblue4', 'yellow')) + theme(plot.background = element_rect(fill = "snow1"))
    }else
    {
      lex <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
      
      #Analysis of "bing" or "nrc lexicon
      sent = 
        data %>%
        inner_join(lex)
      
      #Data for Waffle Graph
      dataGrouped <- sent %>%
        group_by(sentiment) %>%
        summarise(n = n())
      
      values = dataGrouped$n
      names = dataGrouped$sentiment
      
      #Waffle Graph
      val_names <- sprintf("%s (%s)", names, percent(round(values/sum(values), 2)))
      names(values) <- val_names
      graph = 
        waffle(values, title = paste(lexicon, "Sentiments"), rows = num_Rows, colors = c('gold', 'hotpink3', 'mediumturquoise', 'purple', 'midnightblue', 'mediumvioletred', 'plum', 'red', 'steelblue4', 'yellow')) + theme(plot.background = element_rect(fill = "snow1"))
    }
  return(graph)
  }
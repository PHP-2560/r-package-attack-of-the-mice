#waffleFun <- function(values, names, waffleTitle, numRows){
 # val_names <- c(names, round(values/sum(values), 2))
  #names(values) <- val_names
  #waffle(values, title = waffleTitle, rows = numRows, colors = c('gold', 'hotpink3', 'mediumturquoise', 'purple', 'midnightblue', 'mediumvioletred', 'plum', 'red', 'steelblue4', 'tan4')) + theme(plot.background = element_rect(fill = "snow1"))
#}

library("ggplot2")
library("RColorBrewer")

waffle_chart = function(data, dropwords, lexicon = c("nrc", "bing", "afinn")){
  # create waffleFun 
  waffleFun <- function(values, names, waffleTitle, numRows){
    val_names <- sprintf("%s (%s)", names, percent(round(values/sum(values), 2)))
    names(values) <- val_names
    waffle(values, title = waffleTitle, rows = numRows, colors = c('gold', 'hotpink3', 'mediumturquoise', 'purple', 'midnightblue', 'mediumvioletred', 'plum', 'red', 'steelblue4', 'tan4')) + theme(plot.background = element_rect(fill = "snow1"))
 }
  
  #drops words from the lexicon that could lead to misleading results
  #outputs various graphs
  if(lexicon == "nrc"){
    nrc_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis Using "nrc" lexicon
    sentiment_nrc = 
      data %>%
      inner_join(nrc_lexicon)

    #Data for Waffle Graph
    datanrcGrouped <- sentiment_nrc %>%
      group_by(sentiment) %>%
      summarise(n = n())
    
    #Waffle graph 
    waffleChart = 
      waffleFun(values = datanrcGrouped$n, names = datanrcGrouped$sentiment, waffleTitle = "nrc Sentiments", numRows = 50)
  }else if(lexicon == "bing"){
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)

    #Data for Waffle Graph
    dataBingGrouped <- sentiment_bing %>%
      group_by(sentiment) %>%
      summarise(n = n())
    
    #Waffle graph 
    waffleChart = 
      waffleFun(values = dataBingGrouped$n, names = dataBingGrouped$sentiment, waffleTitle = "Bing Sentiments", numRows = 50)
    
  }else{
    afinn_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "afinn" lexicon
    sentiment_afinn = 
      data %>%
      inner_join(afinn_lexicon)
    
    #Data for Waffle Graph
    dataAfinnGrouped <- sentiment_afinn %>%
      group_by(score) %>%
      summarise(n = n())
    
    #Waffle graph 
    waffleChart = 
      waffleFun(values = dataAfinnGrouped$n, names = dataAfinnGrouped$score, waffleTitle = "Afinn Sentiments", numRows = 50)
  }   
return(waffleChart)  
}
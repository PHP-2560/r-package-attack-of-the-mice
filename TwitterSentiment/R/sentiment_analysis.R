#-------------------------------------------------------------
#Sentiment Analysis with data 
#-------------------------------------------------------------
twitter_sentiment_analysis = function(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
{
  #Sets the local so that the dates format nicely
  lct <- Sys.getlocale("LC_TIME"); 
  Sys.setlocale("LC_TIME", "C")  
  
  #Directory to save plots
  mypath = paste(getwd(), "/Plots", sep = "")
  
  #drops words from the lexicon that could lead to misleading results
  #outputs various graphs
  if(lexicon == "nrc")
  {
    nrc_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis Using "nrc" lexicon
    sentiment_nrc = 
      data %>%
      inner_join(nrc_lexicon)
    
    #Count by sentiment
    
    graph1 = 
      sentiment_nrc %>%
      group_by(sentiment) %>%
      count() %>%
      ggplot(aes(x = sentiment,  y = n)) + geom_col(fill = "indianred3") + 
      labs(title = "Count of Individual Sentiments in Tweets", x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph1, file = file.path(mypath, "graph1.png"))
    
    #Percentage of each sentiment
    graph2 = 
      sentiment_nrc %>%
      group_by(sentiment) %>%
      ggplot(aes(x = sentiment)) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) + xlab("Sentiment") + ylab("Relative Frequencies") + ggtitle("Percentage of Sentiments in Tweets") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph2, file = file.path(mypath, "graph2.png"))
    
    #Count by sentiment and grouped by date
    graph3 = 
      sentiment_nrc %>%
      group_by(date, sentiment) %>%
      count() %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"),  y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Date", y = "Count") +
      scale_x_date(date_breaks = "3 days") + 
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph3, file = file.path(mypath, "graph3.png"))
    
    #Percentage by sentiment and grouped by date
    graph4 = 
      sentiment_nrc %>%
      group_by(date, sentiment) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Tweets By Date", x = "Date", y = "Percentage") +
      scale_x_date(date_breaks = "3 days") + 
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph4, file = file.path(mypath, "graph4.png"))
    
    #Displays the words that contribute the most to each sentiment
    graph5 = 
      sentiment_nrc %>%
      # Count by word and sentiment
      count(word, sentiment) %>%
      # Group by sentiment
      group_by(sentiment) %>%
      # Take the top 10 words for each sentiment
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      # Set up the plot with aes()
      ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~ sentiment, scales = "free") + coord_flip() +
      labs(title = "Words that Contributed the Most to Each Sentiment", x = "Count") +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph5, file = file.path(mypath, "graph5.png"))
    
    #Count by sentiment and grouped by individual tweet
    graph6 = 
      sentiment_nrc %>%
      group_by(sentiment, `tweet number`) %>%
      count() %>%
      ggplot(aes(x = as.numeric(`tweet number`), y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Tweet Number", y = "Count") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph6, file = file.path(mypath, "graph6.png"))
    
    #Percentage of sentiment grouped by individual tweet
    graph7 = 
      sentiment_nrc %>%
      group_by(sentiment, `tweet number`) %>%
      ggplot(aes(x = as.numeric(`tweet number`))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Each Tweet", x = "Tweet Number", y = "Percentage") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph7, file = file.path(mypath, "graph7.png"))
    
    # use nrc data to total sentiment by tweet
    datanrcTotal = 
      sentiment_nrc %>%
      group_by(`tweet number`) %>%
      summarise(totalSum = sum(n = n()))
    
    #Graph nrc Sentiment Totals by Tweet
    graph8 = 
      datanrcTotal %>%
      ggplot(aes(reorder(`tweet number`, as.numeric(`tweet number`)), totalSum, fill = totalSum)) + 
      geom_bar(stat = "identity", width = 0.25) +
      scale_fill_gradient(low = "red", high = "seagreen4") +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "snow1"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      scale_x_discrete("Tweet Number", breaks = c()) +
      scale_y_continuous("Sum of Score") +
      labs(fill = "Sum of Score", title = "Total Score Per Tweet") 
    
    ggsave(graph8, file = file.path(mypath, "graph8.png"))
    
    #Provides dataset of word count
    dataTest = pullWordCount(data = sentiment_nrc, word, lkupWord = "great")
    
    #Makes a visual graph of the word count
    graph9 = 
      dataTest[1:15,] %>%
      ggplot(aes(x = n, y=n,label = word, size = n, col = n)) + 
      geom_label(position = "jitter") +
      scale_colour_gradient(low = "lightcoral", high = "royalblue3") +
      scale_x_continuous("Word Usage Count") +
      scale_y_continuous(("Word Usage Count")) +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "aliceblue"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      labs(size = "Count by Size", col = "Count by Color")
    ggsave(graph9, file = file.path(mypath, "graph9.png"))
    
    #Data for Waffle Graph
    datanrcGrouped <- sentiment_nrc %>%
      group_by(sentiment) %>%
      summarise(n = n())
    
    #Waffle graph 
    graph10 = 
      waffleFun(values = datanrcGrouped$n, names = datanrcGrouped$sentiment, waffleTitle = "nrc Sentiments", numRows = 50)
    ggsave(graph10, file = file.path(mypath, "graph10.png"))
    
    
  }else if(lexicon == "bing")
  {
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)
    
    #Count by sentiment
    graph1 = 
      sentiment_bing %>%
      group_by(sentiment) %>%
      count() %>%
      ggplot(aes(x = sentiment,  y = n)) + geom_col(fill = "indianred3") + 
      labs(title = "Count of Individual Sentiments in Tweets", x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph1, file = file.path(mypath, "graph1.png"))
    
    #Percentage of each sentiment
    graph2 = 
      sentiment_bing %>%
      group_by(sentiment) %>%
      ggplot(aes(x = sentiment)) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) + xlab("Sentiment") + ylab("Relative Frequencies") + ggtitle("Percentage of Sentiments in Tweets") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph2, file = file.path(mypath, "graph2.png"))
    
    #Count by sentiment and grouped by date
    graph3 = 
      sentiment_bing %>%
      group_by(date, sentiment) %>%
      count() %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"),  y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Date", y = "Count") +
      scale_x_date(date_breaks = "3 days") + 
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph3, file = file.path(mypath, "graph3.png"))
    
    #Percentage by sentiment and grouped by date
    graph4 = 
      sentiment_bing %>%
      group_by(date, sentiment) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Tweets By Date", x = "Date", y = "Percentage") +
      scale_x_date(date_breaks = "3 days") + 
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph4, file = file.path(mypath, "graph4.png"))
    
    #Displays the words that contribute the most to each sentiment
    graph5 = 
      sentiment_bing %>%
      # Count by word and sentiment
      count(word, sentiment) %>%
      # Group by sentiment
      group_by(sentiment) %>%
      # Take the top 10 words for each sentiment
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      # Set up the plot with aes()
      ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~ sentiment, scales = "free") + coord_flip() +
      labs(title = "Words that Contributed the Most to Each Sentiment", x = "Count") +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph5, file = file.path(mypath, "graph5.png"))
    
    #Count by sentiment and grouped by individual tweet
    graph6 = 
      sentiment_bing %>%
      group_by(sentiment, `tweet number`) %>%
      count() %>%
      ggplot(aes(x = as.numeric(`tweet number`), y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Tweet Number", y = "Count") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph6, file = file.path(mypath, "graph6.png"))
    
    #Percentage of sentiment grouped by individual tweet
    graph7 = 
      sentiment_bing %>%
      group_by(sentiment, `tweet number`) %>%
      ggplot(aes(x = as.numeric(`tweet number`))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Each Tweet", x = "Tweet Number", y = "Percentage") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph7, file = file.path(mypath, "graph7.png"))
    
    # use Bing data to total sentiment by tweet
    dataBingTotal = 
      sentiment_bing %>%
      group_by(`tweet number`) %>%
      summarise(totalSum = sum(n = n()))
    
    #Graph Bing Sentiment Totals by Tweet
    graph8 = 
      dataBingTotal %>%
      ggplot(aes(reorder(`tweet number`, as.numeric(`tweet number`)), totalSum, fill = totalSum)) + 
      geom_bar(stat = "identity", width = 0.25) +
      scale_fill_gradient(low = "red", high = "seagreen4") +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "snow1"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      scale_x_discrete("Tweet Number", breaks = c()) +
      scale_y_continuous("Sum of Score") +
      labs(fill = "Sum of Score", title = "Total Score Per Tweet") 
    
    ggsave(graph8, file = file.path(mypath, "graph8.png"))
    
    #Provides dataset of word count
    dataTest = pullWordCount(data = sentiment_bing, word, lkupWord = "great")
    
    #Makes a visual graph of the word count
    graph9 = 
      dataTest[1:15,] %>%
      ggplot(aes(x = n, y=n,label = word, size = n, col = n)) + 
      geom_label(position = "jitter") +
      scale_colour_gradient(low = "lightcoral", high = "royalblue3") +
      scale_x_continuous("Word Usage Count") +
      scale_y_continuous(("Word Usage Count")) +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "aliceblue"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      labs(size = "Count by Size", col = "Count by Color")
    ggsave(graph9, file = file.path(mypath, "graph9.png"))
    
    #Data for Waffle Graph
    dataBingGrouped <- sentiment_bing %>%
      group_by(sentiment) %>%
      summarise(n = n())
    
    #Waffle graph 
    graph10 = 
      waffleFun(values = dataBingGrouped$n, names = dataBingGrouped$sentiment, waffleTitle = "Bing Sentiments", numRows = 18)
    ggsave(graph10, file = file.path(mypath, "graph10.png"))
    
  }else
  {
    afinn_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "afinn" lexicon
    sentiment_afinn = 
      data %>%
      inner_join(afinn_lexicon)
    
    #Average of each day
    graph1 = 
      sentiment_afinn %>%
      group_by(date) %>%
      summarise(mean = mean(score)) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"), y = mean, group = 1)) + geom_line(color = "indianred3", size = 2.5) + 
      labs(title = "Average Sentiment Over time", x = "Date", y = "Average Sentiment Score") +
      scale_x_date(date_breaks = "3 days") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph1, file = file.path(mypath, "graph1.png"))
    
    #Average of each tweet
    graph2 = 
      sentiment_afinn %>%
      group_by(`tweet number`) %>%
      summarise(mean = mean(score)) %>%
      ggplot(aes(x = as.numeric(`tweet number`), y = mean, group = 1)) + geom_line(color = "indianred3", size = 2.5) + 
      labs(title = "Average Sentiment Over time", x = "Tweet Number", y = "Average Sentiment Score") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph2, file = file.path(mypath, "graph2.png"))
    
    #Histogram of scores
    graph3 = 
      sentiment_afinn %>% 
      group_by(score) %>%
      ggplot(aes(x = score)) + geom_density(color = "black", fill = "indianred3",  alpha = 0.5 ) + 
      labs(title = "Histogram of Scores", x = "Score", y = "Density") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph3, file = file.path(mypath, "graph3.png"))
    
    #Number of tweets per day
    graph4 =
      sentiment_afinn %>%
      group_by(date) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(aes(y = ..count..)) + coord_flip() + labs(title = "Number of Tweets Per Day", y = "Count", x = "Date") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    ggsave(graph4, file = file.path(mypath, "graph4.png"))
    
    #Percent of tweets per day
    graph5 = 
      sentiment_afinn %>%
      group_by(date) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(aes(y = ..count../sum(..count..))) + coord_flip() + labs(title = "Number of Tweets Per Day", y = "Percent", x = "Date") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5)) +
      scale_y_continuous(labels=scales::percent) 
    ggsave(graph5, file = file.path(mypath, "graph5.png"))
    
    #Word Cloud positive words
    png(file.path(mypath, 'graph6.png'), width=600, height=600, unit = "px")
    
    sentiment_afinn %>%
      filter(score > 0) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
    
    dev.off()
    
    #Word Cloud negative words
    png(file.path(mypath, 'graph7.png'), width=600, height=600, unit = "px")
    sentiment_afinn %>%
      filter(score < 0) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
    dev.off()
    
    # use afinn data to total sentiment by tweet
    dataAfinnTotal = 
      sentiment_afinn %>%
      group_by(`tweet number`) %>%
      summarise(totalSum = sum(score))
    
    #Graph Afinn Sentiment Totals by Tweet
    graph8 = 
      dataAfinnTotal %>%
      ggplot(aes(reorder(`tweet number`, as.numeric(`tweet number`)), totalSum, fill = totalSum)) + 
      geom_bar(stat = "identity", width = 0.25) +
      scale_fill_gradient(low = "red", high = "seagreen4") +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "snow1"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      scale_x_discrete("Tweet Number", breaks = c()) +
      scale_y_continuous("Sum of Score") +
      labs(fill = "Sum of Score", title = "Total Score Per Tweet") 
    
    ggsave(graph8, file = file.path(mypath, "graph8.png"))
    
    #Provides dataset of word count
    dataTest = pullWordCount(data = sentiment_afinn, word, lkupWord = "great")
    
    #Makes a visual graph of the word count
    graph9 = 
      dataTest[1:15,] %>%
      ggplot(aes(x = n, y=n,label = word, size = n, col = n)) + 
      geom_label(position = "jitter") +
      scale_colour_gradient(low = "lightcoral", high = "royalblue3") +
      scale_x_continuous("Word Usage Count") +
      scale_y_continuous(("Word Usage Count")) +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "aliceblue"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      labs(size = "Count by Size", col = "Count by Color")
    ggsave(graph9, file = file.path(mypath, "graph9.png"))
    
    #Data for Waffle Graph
    dataAfinnGrouped <- sentiment_afinn %>%
      group_by(score) %>%
      summarise(n = n())
    
    #Waffle graph 
    graph10 = 
      waffleFun(values = dataAfinnGrouped$n, names = dataAfinnGrouped$score, waffleTitle = "Afinn Sentiments", numRows = 18)
    ggsave(graph10, file = file.path(mypath, "graph10.png"))
    
  }   
  
  #-------------------------------------------------------------
  #Other interesting stats
  #-------------------------------------------------------------
  #Most common word
  graph11 = 
    data %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    top_n(10)%>%
    ggplot(aes(word, n)) +
    geom_col(fill = "indianred3") + 
    labs(title = "Top 10 Most Commonly Used Words", x = "Word", y = "Number of Times Used") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5)) +
    coord_flip()
  
  ggsave(graph11, file = file.path(mypath, "graph11.png"))
  
  #returns to the local
  Sys.setlocale("LC_TIME", lct)
}

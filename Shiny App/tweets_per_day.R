tweets_per_day = function(data){
  data$created = as.Date(data$created)
  data = data %>%
    group_by(created) %>%
    count()

  ggplot(data, aes(created, n)) +
    geom_col(show.legend = FALSE, fill = "lightsteelblue") +
    labs(title = "Number of tweets per day", x = "Date", y = "Number")+
    scale_x_date(breaks = pretty_breaks(10))
}

tweets_per_day(trump2)


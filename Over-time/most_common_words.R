most_common_words = function(data, num_words) {
  data %>%
  filter(word != "pic.twitter.com" &
           word != "https" &
           word != "http" &
           word != "twitter.com" &
           word != "utm_source" &
           word != "www.instagram.com" &
           word != "igshid" &
           word != "ig_twitter_share" &
           word != "youtu.be" &
           word != "status" &
           word != "u.https")%>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  top_n(num_words)%>%
  ggplot(aes(word, n)) +
    geom_col(fill = "indianred3") +
    labs(title = paste("Top", num_words, "Most Commonly Used Words"), x = "Word", y = "Number of Times Used") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5)) +
    coord_flip()
}

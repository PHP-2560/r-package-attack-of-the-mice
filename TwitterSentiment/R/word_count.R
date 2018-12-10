#Counts the number of times each word occurs. 
#Shows the number of times the word lkupWord appears. 
pullWordCount <- function(data, word, lkupWord){
  dataTest <- data %>%
    group_by(word) %>%
    summarise(n = n()) %>%
    arrange(desc(n))
  
  for(i in 1:nrow(dataTest)){
    if(dataTest$word[i] == lkupWord){
      print(dataTest[i,])
    }
  }
  return(dataTest)
}

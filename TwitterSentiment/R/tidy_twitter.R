#-------------------------------------------------------------
# Puts the twitter data in tidy format 
#-------------------------------------------------------------
twitter_tidy_data = function(data)
{
  #creates a dataframe from the original list
  data = as.data.frame(data)
  
  #renames the columns "tweets" and "dates"
  col_names = colnames(data)
  names(data)[names(data) == col_names[1]] = 'tweet number'
  names(data)[names(data) == col_names[2]] = 'tweets'
  names(data)[names(data) == col_names[3]] = 'date'
  
  #Makes into characters each column
  data = apply(data, 2, as.character)
  
  #fixes date format so that Xh reads as today's date
  time_diff = str_subset(data[,3],"[0-9]*+(h|m|s)") #finds the "dates" of the tweets that had the date in hours, min, or secs
  time = str_match_all(time_diff, "^[0-9]+") #finds the time component 
  date_replace = c()
  unit = c()
  
  if(length(time) != 0)
  {
    #Calculates the actual date of the tweet 
    for(i in 1:length(time_diff))
    {
      if(str_detect(time_diff[i], 'h'))
      {
        date_replace[i] = format(Sys.time() - as.numeric(time[[i]])*60*60, "%h %d")
      }else if(str_detect(time_diff[i], "m"))
      {
        date_replace[i] = format(Sys.time() - as.numeric(time[[i]])*60, "%h %d")
      }else
      {
        date_replace[i] = format(Sys.time() - as.numeric(time[[i]]), "%h %d")
      }
    }
  }
  
  #Capitalizes the first letter so that the date format matches the others and inserts it back into the df
  date_replace = capitalize_first_letter(str_remove_all(date_replace, "\\."))
  data[str_which(data[,3], "[0-9]*+(h|m|s)"),3] = date_replace
  
  #saves a tibble
  data = as_tibble(data) #easier to read format
  
  #creates the tidy_data set
  tidy_data = 
    data %>%
    unnest_tokens(word, tweets)
  
  return(tidy_data)
}
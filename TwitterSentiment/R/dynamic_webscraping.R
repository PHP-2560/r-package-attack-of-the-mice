#-------------------------------------------------------------
#Starts RSelenium and dynamically scrapes the data
#-------------------------------------------------------------
twitter_dynamic_download = function(num_scrolls, url, machine = c("windows", "mac", "linux"))
{
  #chooses local host based on OS
  machine <- match.arg(machine)
  if(machine == "windows")
  {
    ip = "192.168.99.100"
  }else if(machine == "mac")
    {
      ip = "127.0.0.1"
    }else
  {
    ip = "localhost"
  }
  
  #Opens connection and browser
  remDr <- remoteDriver(remoteServerAddr = ip, port = 4445L, browserName = "firefox")
  remDr$open()
  
  #navigate to the twitter page given a twitter handle
  remDr$navigate(url)
  
  #Progress bar with ETA
  pb <- progress_bar$new(
    format = " downloading [:bar] :percent eta: :eta",
    total = num_scrolls, clear = FALSE, width= 60)
  
  #Scrolls for a given number of pages and loads it
  for(i in 1:num_scrolls)
  {
    pb$tick()
    remDr$executeScript(paste("scroll(0,",i*10000,");"),list(""))
    Sys.sleep(1)
  }
  
  #get the page html
  page_source<-remDr$getPageSource()
  remDr$close() #closes the browser
  
  #parses the data for tweets
  #Does not include responses!
  data = 
    read_html(page_source[[1]]) %>% html_nodes(css = '.js-tweet-text-container') %>%
                    html_text()
  
  #parses data for dates of tweets
  dates = read_html(page_source[[1]]) %>% html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "js-short-timestamp", " " ))]') %>% html_text()
  
  #return
  return(list(1:length(data), data, dates))
}

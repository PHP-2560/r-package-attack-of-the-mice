\name{twitter_dynamic_download}
\alias{twitter_dynamic_download}
\title{Webscrapes the Twitter data}

\description{Dynamically scrapes tweets from a given twitter url given the following input: the number of times the user wishes to scroll on a virtual browser, the url of the twitter page, and the machine on which the code is running. The output is in the form of a list.}

\usage{twitter_dynamic_download(num_scrolls, url, machine = c("windows", "mac", "linux"))}

\arguments{
  \item{num_scrolls}{The number of times you want the virtual browswer to scroll. How far you scroll determines how many tweets can be scraped.}
  \item{url}{The url of the twitter page that you want to scrape.}
  \item{machine}{This corresponds to the type of machine running the code. Required to set up a proper connection to the host.}
}

\details{Note that for this function you must have Docker and Docker toolbox installed. Instructions will be included in the general package description. Also note that your firewall might block the connection. Deactivate and retry if not working.}

\value{A list with 3 columns that corresponds to the tweet number, the text from the tweet, and the date of the tweet.}

\examples{twitter_dynamic_download(url = 'https://twitter.com/realDonaldTrump', num_scrolls = 20, machine = 'windows') %>%
  twitter_tidy_data()}




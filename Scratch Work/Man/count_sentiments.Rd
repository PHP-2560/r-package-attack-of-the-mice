\name{count_sentiments}
\alias{count_sentiments}
\title{Graph of Sentiment Counts by Word}

\description{
Performs sentiment analysis based on lexicon specified, and then returns a graph of the count of individual sentiments in tweets.
}

\usage{
count_sentiments(data, dropwords, lexicon = c("nrc", "bing"))
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph.}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis and graph.}
  
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and graph. Options are NRC or Bing.}
}

\value{A graph made in ggplot to visualize the data, showing the count of individual sentiments in tweets}

\seealso{\code{\link[ggplot2]{ggplot}}.}
\examples{
count_sentiments(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}
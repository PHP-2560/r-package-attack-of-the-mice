\name{percent_sentiments}
\alias{percent_sentiments}
\title{Graph of Sentiment Percents by Word}

\description{
Performs sentiment analysis based on lexicon specified, and then returns a graph of the percent of individual sentiments in tweets.
}

\usage{
percent_sentiments(data, dropwords, lexicon = c("nrc", "bing"))
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis and graph.}
  
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and graph. Options are NRC or Bing.}
}

\value{A graph made in ggplot to visualize the data, showing the percent of individual sentiments in tweets}

\seealso{\code{\link[ggplot2]{ggplot}}.}
\examples{
percent_sentiments(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}
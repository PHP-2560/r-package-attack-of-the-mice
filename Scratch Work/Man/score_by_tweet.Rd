\name{score_by_tweet}
\alias{score_by_tweet}
\title{Graph of Total Sentiment Score by Tweet}

\description{
Performs sentiment analysis based on lexicon specified, and then returns a graph of sentiment score by tweet.
}

\usage{
score_by_tweet(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis and graph.}
  
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and graph. Options are NRC, Afinn, or Bing.}
}

\value{A graph made in ggplot to visualize the data, showing the total sentiment score for each tweet.}

\seealso{\code{\link[ggplot2]{ggplot}}.}
\examples{
score_by_tweet(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}
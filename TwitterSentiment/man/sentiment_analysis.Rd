\name{twitter_sentiment_analysis}
\alias{twitter_sentiment_analysis}
\title{Twitter Sentiment Analysis}
\usage{
twitter_sentiment_analysis(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
}
\description{
Performs sentiment analysis based on lexicon specified.
}
\arguments{
  \item{data}{Data to run sentiment analysis on}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis.}
  
  \item{lexicon}{Choice of lexicon that would like to use for sentiment analysis. Options are NRC, Afinn, or Bing.}
}
\examples{
twitter_sentiment_analysis(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}


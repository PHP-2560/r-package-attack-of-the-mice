\name{twitter_sentiment_analysis}
\alias{twitter_sentiment_analysis}
\title{Twitter Sentiment Analysis}

\description{
Performs sentiment analysis based on lexicon specified.
}

\usage{
twitter_sentiment_analysis(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis.}
  
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis. Options are NRC, Afinn, or Bing.}
}

\value{A series of different graphs made in ggplot to visualize the data.}

\seealso{\code{\link[ggplot2]{ggplot}}. \code{\link[waffle]{waffle}}, \code{\link[wordcloud]{wordcloud}}.}
\examples{
twitter_sentiment_analysis(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}


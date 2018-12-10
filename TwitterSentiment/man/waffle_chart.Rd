\name{waffle_chart}
\alias{waffle_chart}
\title{Waffle Chart by Sentiment}

\description{
Performs sentiment analysis based on lexicon specified, and then returns a waffle chart of sentiment blocks by color.
}

\usage{
waffle_chart(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph.}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis and chart.}
  
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and chart. Options are NRC, Afinn, or Bing.}
}

\value{A graph made in waffle to visualize the data, showing sentiment frequency.}

\seealso{\code{\link[waffle]{waffle}}.}
\examples{
score_by_tweet(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}
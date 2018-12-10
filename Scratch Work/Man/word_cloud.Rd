\name{word_cloud}
\alias{word_cloud}
\title{Creates word cloud}
\description{
Creates a word cloud using positive or negative words
}
\usage{
word_cloud(df, sent = c("positive", "negative"))
}
\arguments{
  \item{df}{A dataframe}
  \item{sent}{Can be either "positive" or "negative". "positive" will choose positive words and "negative" will choose negative words from the bing lexicon}
}

\details{This function also computes the correct date if given the number of hours since a particular post. The tweets are broken down using tidytext functions.}

\value{Outputs a wordcloud.}

\examples{
word_cloud(data, sent = "positive")
word_cloud(data, sent = "negative")
}
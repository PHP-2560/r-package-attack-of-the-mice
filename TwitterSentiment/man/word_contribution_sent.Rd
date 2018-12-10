\name{word_contribution_sent}
\alias{word_contribution_sent}
\title{Graphs of Words that Contributed the Most to Each Sentiment}

\description{
Performs sentiment analysis based on lexicon specified, and then returns graphs for each sentiment showing words that contributed the most to each sentiment.
}

\usage{
word_contribution_sent(data, dropwords, lexicon = c("nrc", "bing"))
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph.}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis and graphs.}
  
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and graphs. Options are NRC or Bing.}
}

\value{A graph made in ggplot to visualize the data, showing largest word contribution by sentiment.}

\seealso{\code{\link[ggplot2]{ggplot}}.}
\examples{
word_contribution_sent(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}
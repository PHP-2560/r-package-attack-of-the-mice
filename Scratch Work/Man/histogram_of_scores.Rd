\name{histogram_of_scores}
\alias{histogram_of_scores}
\title{Histogram of Afinn Scores}

\description{
Performs sentiment analysis based on afinn lexicon, and then returns a histogram of the scores.
}

\usage{
histogram_of_scores(data, dropwords)
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph}

  \item{dropwords}{A vector of words to exclude from the sentiment analysis and histogram.}
}

\value{A histogram made in ggplot to visualize the data.}

\seealso{\code{\link[ggplot2]{ggplot}}.}
\examples{
histogram_of_scores(data = df, dropwords = c("trump", "like"))
}

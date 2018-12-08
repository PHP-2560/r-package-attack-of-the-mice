\name{most_common_words}
\alias{most_common_words}
\title{Most Common Words}
\usage{
most_common_words(data, num_words)
}
\description{
Outputs a bar graph of the most common words used. The number of words that are used is decided by the user.
}
\arguments{
  \item{data}{Data to run sentiment analysis on}

  \item{num_words}{The number of words you want graph}
}
\examples{
most_common_words(data = df, num_words = 10)
}


\name{most_common_words}
\alias{most_common_words}
\title{Most Common Words}

\description{
Outputs a bar graph of the most common words used. The number of words that are used is decided by the user.
}

\usage{
most_common_words(data, num_words)
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on}

  \item{num_words}{The number of words you want graph}
}

\details{This function excludes the following words common to Twitter: "pic.twitter.com", "https", "http" and "twitter.com"}

\value{It outputs a bar graph using ggplot.}

\seealso{\code{\link[ggplot2]{ggplot}}.}

\examples{
most_common_words(data = df, num_words = 10)
}


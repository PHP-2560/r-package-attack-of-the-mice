\name{top_words_sent}
\alias{top_words_sent}
\title{Top words per sentiment}
\usage{
top_words_sent(data, lexicon)
}
\description{
Returns a bar graph of the top words per sentiment
}
\arguments{
  \item{data}{A dataframe}

  \item{lexicon}{The lexicon that will be used for the graph. Can be "bing" or "nrc"}
}
\examples{
top_words_sent(data = df, lexicon = "nrc")
top_words_sent(data = df, lexicon = "bing")
}

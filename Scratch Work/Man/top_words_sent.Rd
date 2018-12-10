\name{top_words_sent}
\alias{top_words_sent}
\title{Top words per sentiment}
\description{
Returns a bar graph of the top words per sentiment
}
\usage{
top_words_sent(data, lexicon)
}
\arguments{
  \item{data}{A tidy dataframe}

  \item{lexicon}{The lexicon that will be used for the graph. Can be "bing" or "nrc"}
}
\details{This function will give you only the top 20 words.}
\value{It will output a bargraph.}
\examples{
top_words_sent(data = df, lexicon = "nrc")
top_words_sent(data = df, lexicon = "bing")
}

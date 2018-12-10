\name{num_tweets_per_day}
\alias{num_tweets_per_day}
\title{Bar graph of the number of tweets per day}

\description{The goal of this function is to output a bar graph of the number of tweets per day of the tweets dynamically scraped.}


\usage{num_tweets_per_day(data, lexicon, dropwords)

}
\arguments{
  \item{data}{The tidy dataframe you want to analyze}
  \item{lexicon}{The dictionary you want to use to perform the text analysis. The options are "afinn", "bing", or "nrc".}
  \item{dropwords}{A character vector that includes words you wish to exclude from the lexicon you selected. You would choose words that have another meaning besides the traditional dictionary meaning. For example, the word "Trump" can refer to the president or it can be a verb to best someone. In this case, we would want to exclude this word because the common usage in the tweets does not coincide with thet dictionary usage. }
}

\value{Outputs a bar graph using ggplot.}

\seealso{\code{\link[ggplot2]{ggplot}}.}

\examples{
num_tweets_per_day(data = df, lexicon = "afinn", dropwords = c("trump", "grand"))
}
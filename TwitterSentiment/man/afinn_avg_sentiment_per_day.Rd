\name{afinn_avg_sentiment_per_day}
\alias{afinn_avg_sentiment_per_day}
\title{Line graph of the average sentiment over time}

\description{The goal of this function is to output a line graph of the average sentiment over time of the tweets dynamically scraped.}


\usage{afinn_avg_sentiment_per_day(data, dropwords)

}
\arguments{
  \item{data}{The tidy dataframe you want to analyze}
  \item{dropwords}{A character vector that includes words you wish to exclude from the lexicon you selected. You would choose words that have another meaning besides the traditional dictionary meaning. For example, the word "Trump" can refer to the president or it can be a verb to best someone. In this case, we would want to exclude this word because the common usage in the tweets does not coincide with thet dictionary usage. }
}
\details{This function only works with afinn because the average can only be computed for numerical values. The average sentiment overtime is meaningless in the nrc or bing dictionaries.}
\value{Outputs a line graph using ggplot.}

\seealso{\code{\link[ggplot2]{ggplot}}.}

\examples{
afinn_avg_sentiment_per_day(data = df, dropwords = c("trump", "grand"))
}
\name{count_tweet_by_date}
\alias{count_tweet_by_date}
\title{Bar graph of the number of tweets per day for each sentiment}

\description{The goal of this function is to output a bar graph of the number of tweets per day for each sentiment for the tweets dynamically scraped.}


\usage{count_tweet_by_date(data, lexicon, dropwords)

}
\arguments{
  \item{data}{The tidy dataframe you want to analyze}
  \item{lexicon}{The dictionary you want to use to perform the text analysis. The options are "bing" or "nrc".}
  \item{dropwords}{A character vector that includes words you wish to exclude from the lexicon you selected. You would choose words that have another meaning besides the traditional dictionary meaning. For example, the word "Trump" can refer to the president or it can be a verb to best someone. In this case, we would want to exclude this word because the common usage in the tweets does not coincide with thet dictionary usage. }
}

\details{This is useful only for the lexicons "bing" or "nrc" since Afinn does not have non-numerical sentiments.}

\value{Outputs a bar graph using ggplot.}

\seealso{\code{\link[ggplot2]{ggplot}}.}

\examples{
count_tweet_by_date(data = df, lexicon = "nrc", dropwords = c("trump", "grand"))
}
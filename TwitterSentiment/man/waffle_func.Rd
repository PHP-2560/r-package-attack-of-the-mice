\name{waffleFun}
\alias{waffleFun}
\title{Waffle Chart by Sentiment}

\description{
Performs sentiment analysis based on lexicon specified, and then returns a waffle chart of sentiment blocks by color.
}

\usage{
waffleFun(data, lexicon, dropwords, num_Rows)
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph.}
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and chart. Options are NRC, Afinn, or Bing.}
  \item{dropwords}{A character vector of words to exclude from the sentiment analysis and chart.}
  \item{num_rows}{The number of rows you want for the graphic.}
}

\value{A graph made in waffle to visualize the data, showing sentiment frequency.}

\seealso{\code{\link[waffle]{waffle}}.}
\examples{
waffleFun(data = df, lexicon = "nrc", dropwords = c("trump", "like"), num_Rows = 20)
}
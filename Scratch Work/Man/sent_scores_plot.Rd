\name{sent_scores_plot}
\alias{sent_scores_plot}
\title{Graph of total sentiment scores}

\description{
Creates a graph of total sentiment scores based on lexicon
}

\usage{
sent_scores_plot(data, lexicon)
}

\arguments{
  \item{data}{A tidy data frame.} 
  \item{lexicon}{The dictionary to use in the analysis. c("afinn", "bing", "nrc")}
}

\value{It outputs a bar graph using ggplot.}

\seealso{\code{\link[ggplot2]{ggplot}}.}

\examples{
sent_scores_plot(data, lexicon)
}

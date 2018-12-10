\name{word_count_chart}
\alias{word_count_chart}
\title{Create a Word Usage Plot}

\description{The objective of this function is to output a visual of the number of times the most frequently repeated words are used, while highlighting a word of the user's choice.}

\usage{word_count_chart(data, lexicon, dropwords, lookup_word, top_n_words)}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on and graph.}
  \item{lexicon}{Choice of lexicon that you would like to use for sentiment analysis and chart. Options are NRC, Afinn, or Bing.}
  \item{dropwords}{A character vector of words to exclude from the sentiment analysis and chart.}
  \item{top_n_words}{This sets the number of words that the user wishes to see plotted.}
}

\details{This function will show the user's top n words, but only if they do not overlap. If they overlap, it will take the maximum number of words such that there is no overlap.}

\value{A text plot with the most commonly used words. The number of words outputted depends on the user's input. }

\examples{word_count_chart(data = df, lexicon = "bing", dropwords = c("trump", "grand"), lookup_word = "hard", top_n_words = 10)}




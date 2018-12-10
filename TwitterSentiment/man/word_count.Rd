\name{word_count}
\alias{word_count}
\title{Count by Word}

\description{
Performs a count of all words, and returns a tibble with all words counted, and the count specific to lkupWord - the word you're interested in counting.
}

\usage{
word_count(data, word, lkupWord)
}

\arguments{
  \item{data}{A tidy dataframe to run sentiment analysis on}

  \item{word}{Column name where word to lookup is located.}
  
  \item{lkupWord}{Word that is to be counted.}
}

\value{A count of number of times lkupWord shows up in the column, and a tibble with the count of all words.}

\seealso{\code{\link[dplyr]{dplyr}}.}
\examples{
word_count(data = df, word = "word", lkupWord = "the")
}


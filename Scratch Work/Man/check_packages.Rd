\name{Check Packages}
\alias{check_packages}
\title{Checks to make sure all packages are installed and libraried.}

\description{
Installs and libraries all of the dependencies required to use the package.  
}

\usage{
check_packages(names)

}

\arguments{
  \item{names}{A character vector of package names.}
}

\examples{
check_packages(c('tidytext', 'ggplot2', 'stringr', 'stats', 'waffle', 'scales', 'dplyr', 'wordcloud', 'shinythemes', 'vembedr', 'htmltools'))

}
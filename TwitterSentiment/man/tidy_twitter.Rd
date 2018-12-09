\name{twitter_tidy_data}
\alias{twitter_tidy_data}
\title{Tidy Twitter Data}
\description{
Takes a data frame and converts it to a tidy data frame
}
\usage{
twitter_tidy_data(list)
}
\arguments{
  \item{list}{The list you want to tidy}
}

\details{This function also computes the correct date if given the number of hours since a particular post. The tweets are broken down using tidytext functions.}

\value{Outputs a dataframe.}

\seealso{\code{\link[tidytext]{unnest_tokens}}.}
\examples{
twitter_tidy_data(data)
}

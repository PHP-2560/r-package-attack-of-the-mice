\docType{package}
\name{TwitterSentiment-package}
\alias{TwitterSentiment-package}
\title{TwitterSentiment: a method to scrape and analyze tweet data}

\description{
TwitterSentiment provides functions that focus on text analysis for twitter data that is dynamically scraped on a virtual browser. 
}

\details{It has 3 main goals: 
  \itemize{
    \item Dynamically download tweets given the following input: twitter url, number of scrolls, and machine (the type of computer you are using.)
    \item Tidy this data so that the text analysis is seamless
    \item Show relevant graphs on different features of the analysis. 
          }
        }
\section{Package configurations}{This package requires the installation of docker and docker toolbox to run the container. To properly install on a windows or linux, follow the instructions \href{https://rpubs.com/johndharrison/RSelenium-Docker}{here.} If using a Mac, follow \href{https://rpubs.com/grahamplace/rseleniumonmac}{these instructions.} Also, please note that you may have to disable your firewall for the connection to take place.}

\seealso{\code{\link[RSelenium]{RSelenium-package}}.}
\name{twitter_sentiment_analysis}
\alias{twitter_sentiment_analysis}
\title{Twitter Sentiment Analysis}
\usage{
twitter_sentiment_analysis(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
}
\description{
Performs sentiment analysis based on lexicon specified.
}
\examples{
twitter_sentiment_analysis(data = df, dropwords = c("trump", "like"), lexicon = "nrc")
}
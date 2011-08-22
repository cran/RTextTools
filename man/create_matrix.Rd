\name{create_matrix}
\alias{create_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creates a document-term matrix to be passed into create_corpus().
}
\description{
Creates an object of class \code{DocumentTermMatrix} from \pkg{tm} that can be used in the \code{\link{create_corpus}} function.
}
\usage{
create_matrix(textColumns, language = "en", minDocFreq = 1, 
minWordLength = 3, removeNumbers = FALSE, removePunctuation = TRUE, 
removeSparseTerms = 0, removeStopwords = TRUE, selectFreqTerms = 0, 
stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, 
weighting = weightTf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textColumns}{
Either character vector (e.g. data$Title) or a \code{cbind()} of columns to use for training the algorithms (e.g. \code{cbind(data$Title,data$Subject)}).
}
  \item{language}{
The language to be used for stemming the text data.
}
  \item{minDocFreq}{
The minimum number of times a word should appear in a document for it to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{minWordLength}{
The minimum number of letters a word should contain to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{removeNumbers}{
A \code{logical} parameter to specify whether to remove numbers.
}
  \item{removePunctuation}{
A \code{logical} parameter to specify whether to remove punctuation.
}
  \item{removeSparseTerms}{
See package \pkg{tm} for more details.
}
  \item{removeStopwords}{
A \code{logical} parameter to specify whether to remove stopwords using the language specified in language.
}
  \item{selectFreqTerms}{
Select the \code{N} most frequent terms in each document to use for training.
}
  \item{stemWords}{
A \code{logical} parameter to specify whether to stem words using the language specified in language.
}
  \item{stripWhitespace}{
A \code{logical} parameter to specify whether to strip whitespace.
}
  \item{toLower}{
A \code{logical} parameter to specify whether to make all text lowercase.
}
  \item{weighting}{
Either \code{weightTf} or \code{weightTfIdf}. See package \pkg{tm} for more details.
}
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>, Loren Collingwood <lorenc2@uw.edu>
}
\examples{
library(RTextTools)
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")
data <- data[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data$Title,data$Subject), language="english", 
removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{method}

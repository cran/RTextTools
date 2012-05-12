\name{create_matrix}
\alias{create_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creates a document-term matrix to be passed into create_container().
}
\description{
Creates an object of class \code{DocumentTermMatrix} from \pkg{tm} that can be used in the \code{\link{create_container}} function.
}
\usage{
create_matrix(textColumns, language="english", minDocFreq=1, maxDocFreq=Inf, 
minWordLength=3, maxWordLength=Inf, ngramLength=1, originalMatrix=NULL, 
removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, 
removeStopwords=TRUE,  stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, 
weighting=weightTf)
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
  \item{maxDocFreq}{
The maximum number of times a word should appear in a document for it to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{minWordLength}{
The minimum number of letters a word or n-gram should contain to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{maxWordLength}{
The maximum number of letters a word or n-gram should contain to be included in the matrix. See package \pkg{tm} for more details.
}
  \item{ngramLength}{
The number of words to include per n-gram for the document-term matrix.
}
  \item{originalMatrix}{
The original \code{DocumentTermMatrix} used to train the models. If supplied, will adjust the new matrix to work with saved models.
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
data(NYTimes)
data <- NYTimes[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english", 
removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{method}

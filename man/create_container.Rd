\name{create_container}
\alias{create_container}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
creates a container for training, classifying, and analyzing documents.
}
\description{
Given a \code{DocumentTermMatrix} from the \pkg{tm} package and corresponding document labels, creates a container of class \code{\link{matrix_container-class}} that can be used for training and classification (i.e. \code{\link{train_model}}, \code{\link{train_models}}, \code{\link{classify_model}}, \code{\link{classify_models}})
}
\usage{
create_container(matrix, labels, trainSize=NULL, testSize=NULL, virgin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
A document-term matrix of class \code{DocumentTermMatrix} or \code{TermDocumentMatrix} from the \pkg{tm} package, or generated by \code{\link{create_matrix}}.
}
  \item{labels}{
A \code{factor} or \code{vector} of labels corresponding to each document in the matrix.
}
  \item{trainSize}{
A range (e.g. \code{1:1000}) specifying the number of documents to use for training the models. Can be left blank for classifying corpora using saved models that don't need to be trained.
}
  \item{testSize}{
A range (e.g. \code{1:1000}) specifying the number of documents to use for classification. Can be left blank for training on all data in the matrix.
}
  \item{virgin}{
A logical (\code{TRUE} or \code{FALSE}) specifying whether to treat the classification data as virgin data or not.
}
}
\value{
A container of class \code{\link{matrix_container-class}} that can be passed into other functions such as \code{\link{train_model}}, \code{\link{train_models}}, \code{\link{classify_model}}, \code{\link{classify_models}}, and \code{\link{create_analytics}}.
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>, Loren Collingwood <loren.collingwood@gmail.com>
}
\examples{
library(RTextTools)
data(NYTimes)
data <- NYTimes[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english", 
removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix,data$Topic.Code,trainSize=1:75, testSize=76:100, 
virgin=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{method}

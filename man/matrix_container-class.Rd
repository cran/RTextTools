\name{matrix_container-class}
\docType{class}
\alias{matrix_container-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
an S4 class containing the training and classification matrices.
}
\description{
An S4 class containing all information necessary to train, classify, and generate analytics for a dataset.
}
\section{Objects from the Class}{
  Objects could in principle be created by calls of the 
  form \code{new("matrix_container", ...)}.
  The preferred form is to have them created via a call to 
  \code{\link{create_corpus}}.
}
\section{Slots}{
  \describe{
    \item{\code{training_matrix}}{Object of class \code{"matrix.csr"}:
      stores the training set of the \code{DocumentTermMatrix} created by \code{\link{create_matrix}}}
    \item{\code{training_codes}}{Object of class \code{"factor"}:
      stores the training labels for each document in the \code{training_matrix} slot of \code{\link{matrix_container-class}}}
    \item{\code{classification_matrix}}{Object of class \code{"matrix.csr"}:
      stores the classification set of the \code{DocumentTermMatrix} created by \code{\link{create_matrix}}}
    \item{\code{testing_codes}}{Object of class \code{"factor"}:
      if \code{virgin=FALSE}, stores the labels for each document in classification_matrix}
    \item{\code{column_names}}{Object of class \code{"vector"}:
      stores the column names of the \code{DocumentTermMatrix} created by \code{\link{create_matrix}}}
    \item{\code{virgin}}{Object of class \code{"logical"}:
      boolean specifying whether the classification set is virgin data (\code{TRUE}) or not (\code{FALSE}).}
  }
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\examples{
library(RTextTools)
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")
data <- data[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data$Title,data$Subject), language="english", 
removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)
corpus <- create_corpus(matrix,data$Topic.Code,trainSize=1:75, testSize=76:100, 
virgin=FALSE)

corpus@training_matrix
corpus@training_codes
corpus@classification_matrix
corpus@testing_codes
corpus@column_names
corpus@virgin
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
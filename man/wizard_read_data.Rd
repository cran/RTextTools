\name{wizard_read_data}
\alias{wizard_read_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
a simplified function for reading data from files.
}
\description{
A simple interface for reading in data from files and creating a corpus all in one step.
}
\usage{
wizard_read_data(filename, tablename = NULL, filetype = "csv", 
virgin=FALSE, textColumns, codeColumn, trainSize, testSize, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character string of the name of the file, include path if the file is not located in the working directory.
}
  \item{tablename}{
Microsoft Access database only. The table name in the database.
}
  \item{filetype}{
Character vector specifying the file type. Options include "csv","tab","accdb","mdb" to denote .csv files, text files, or Access databases.
}
  \item{virgin}{
A logical (\code{TRUE} or \code{FALSE}) specifying whether to treat the classification data as virgin data or not. Defaults to \code{FALSE}, specifying that classification data is not virgin data.
}
  \item{textColumns}{
The a \code{cbind()} of column(s) to use for training the algorithms (e.g. cbind(data$Title)).
}
  \item{codeColumn}{
A factor or vector of labels corresponding to each document in the matrix.
}
  \item{trainSize}{
A range (e.g. \code{1:1000}) specifying the number of documents to use for training the models.
}
  \item{testSize}{
A range (e.g. \code{1001:2000}) specifying the number of documents to use for classification.
}
  \item{\dots}{
Other parameters to be passed on to \code{\link{create_matrix}}.
}
}
\value{
A corpus of class \code{\link{matrix_container-class}} that can be passed into other functions such as \code{\link{train_model}}, \code{\link{train_models}}, \code{\link{classify_model}}, \code{\link{classify_models}}, \code{\link{wizard_train_classify}}, and \code{\link{create_analytics}}.
}
\author{
Wouter Van Atteveldt <wouter@vanatteveldt.com>, Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\examples{
library(RTextTools)
corpus <- wizard_read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"), 
textColumns=c("Title","Subject"), codeColumn="Topic.Code", trainSize=75, 
testSize=25, virgin=FALSE)
}
\keyword{method}

\name{read_data}
\alias{read_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
reads data from files into an R data frame.
}
\description{
Reads data from several types of data storage types into an R data frame.
}
\usage{
read_data(filename, tablename = NULL, type = c("csv", "tab", "accdb", "mdb"),
...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Character string of the name of the file, include path if the file is not located in the working directory.
}
  \item{tablename}{
Microsoft Access database only. The table name in the database.
}
  \item{type}{
Character vector specifying the file type. Options include \code{"csv"}, \code{"tab"}, \code{"accdb"}, \code{"mdb"} to denote .csv files, text files, or Access databases.
}
  \item{\dots}{
Other arguments passed to read_data.
}
}
\value{
An \code{data.frame} object is returned with the contents of the file.
}
\author{
Loren Collingwood <lorenc2@uw.edu>, Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\examples{
library(RTextTools)
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")
}
\keyword{method}

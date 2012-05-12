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
read_data(filepath, type=c("csv","delim","folder"), index=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filepath}{
Character string of the name of the file or folder, include path if the file is not located in the working directory.
}
  \item{type}{
Character vector specifying the file type. Options include \code{csv}, \code{delim}, and \code{folder} to denote .csv files, delimited files (tab, pipe, etc.) files, or folders of text files. If using the \code{delim} option, be sure to pass in a separate \code{sep} parameter to indicate how the file is delimited.
}
  \item{index}{
The path to a CSV file specifying the training label of each file in the folder of text files, one per line. An example of one line would be \code{1.txt,1}. Do not include the full file path for each file, that will be handled automatically using the folder location passed into \code{filepath}. This index file must be located outside the folder of files.
}
  \item{\dots}{
Other arguments passed to R's \code{read.csv} function.
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
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv",sep=";")
}
\keyword{method}

\name{analytics-class}
\docType{class}
\alias{analytics-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
an S4 class containing the analytics for a classified set of documents.
}
\description{
An \code{S4 class} containing the analytics for a classified set of documents. This includes a label summary, document summary, ensemble summary, and algorithm summary. This class is returned if \code{virgin=FALSE} in \code{\link{create_container}}.
}
\section{Objects from the Class}{
  Objects could in principle be created by calls of the 
  form \code{new("analytics", ...)}.
  The preferred form is to have them created via a call to 
  \code{\link{create_analytics}}.
}
\section{Slots}{
  \describe{
    \item{\code{label_summary}}{Object of class \code{"data.frame"}:
      stores the analytics for each label, including the percent coded accurately and how much overcoding occurred}
    \item{\code{document_summary}}{Object of class \code{"data.frame"}:
      stores the analytics for each document, including all available raw data associated with the learning process}
    \item{\code{algorithm_summary}}{Object of class \code{"data.frame"}:
      stores precision, recall, and F-score statistics for each algorithm, broken down by label}
    \item{\code{ensemble_summary}}{Object of class \code{"matrix"}:
      stores the accuracy and coverage for an n-algorithm ensemble scoring}
  }
}
\author{
Timothy P. Jurka <tpjurka@ucdavis.edu>
}
\examples{
library(RTextTools)
data(NYTimes)
data <- NYTimes[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english", 
removeNumbers=TRUE, stemWords=FALSE, weighting=weightTfIdf)
container <- create_container(matrix,data$Topic.Code,trainSize=1:75, testSize=76:100, 
virgin=FALSE)
models <- train_models(container, algorithms=c("MAXENT","SVM"))
results <- classify_models(container, models)
analytics <- create_analytics(container, results)

summary(analytics)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\name{analytics_virgin-class}
\docType{class}
\alias{analytics_virgin-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
an S4 class containing the analytics for a classified set of documents.
}
\description{
An \code{S4 class} containing the analytics for a classified set of documents. This includes a label summary and a document summary. This class is returned if \code{virgin=TRUE} in \code{\link{create_container}}.
}
\section{Objects from the Class}{
  Objects could in principle be created by calls of the 
  form \code{new("analytics_virgin", ...)}.
  The preferred form is to have them created via a call to 
  \code{\link{create_analytics}}.
}
\section{Slots}{
  \describe{
    \item{\code{label_summary}}{Object of class \code{"data.frame"}:
      stores the analytics for each label, including how many documents were classified with each label}
    \item{\code{document_summary}}{Object of class \code{"data.frame"}:
      stores the analytics for each document, including all available raw data associated with the learning process}
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
virgin=TRUE)
models <- train_models(container, algorithms=c("MAXENT","SVM"))
results <- classify_models(container, models)
analytics <- create_analytics(container, results)

summary(analytics)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
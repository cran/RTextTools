\name{recall_accuracy}
\alias{recall_accuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculates the recall accuracy of the classified data.
}
\description{
Given the true labels to compare to the labels predicted by the algorithms, calculates the recall accuracy of each algorithm.
}
\usage{
recall_accuracy(true_labels, predicted_labels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true_labels}{
A vector containing the true labels, or known values for each document in the classification set.
}
  \item{predicted_labels}{
A vector containing the predicted labels, or classified values for each document in the classification set.
}
}

\author{
Loren Collingwood <lorenc2@uw.edu>, Timothy P. Jurka <tpjurka@ucdavis.edu>
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
recall_accuracy(analytics@document_summary$MANUAL_CODE,
analytics@document_summary$GLMNET_LABEL)
recall_accuracy(analytics@document_summary$MANUAL_CODE,
analytics@document_summary$MAXENTROPY_LABEL)
recall_accuracy(analytics@document_summary$MANUAL_CODE,
analytics@document_summary$SVM_LABEL)
}
\keyword{method}

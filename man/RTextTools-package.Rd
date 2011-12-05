\name{RTextTools-package}
\alias{RTextTools-package}
\docType{package}
\title{RTextTools Machine Learning}
\description{
RTextTools is a machine learning package for automatic text classification that makes it simple for novice users to get started with machine learning, while allowing experienced users to easily experiment with different settings and algorithm combinations. The package includes nine algorithms for ensemble classification (svm, slda, boosting, bagging, random forests, glmnet, decision trees, neural networks, maximum entropy), comprehensive analytics, and thorough documentation.
}
\details{
\tabular{ll}{
Package: \tab RTextTools\cr
Type: \tab Package\cr
Version: \tab 1.3.2\cr
Date: \tab 2011-12-05\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
Using RTextTools can be broken down into five simple steps. First, read your data into R as a data frame using the included \code{\link{read_data}} function or any other method. Next, create the document term matrix from your textual documents using \code{\link{create_matrix}}, and create a container of these sparse matrices and labels with \code{\link{create_corpus}}. This object will then be input to both \code{\link{train_model}} and \code{\link{classify_model}}, which respectively train and classify the textual data. Alternatively, you may use \code{\link{train_models}} and \code{\link{classify_models}} to train and classify using multiple algorithms at once. You may use \code{\link{print_algorithms}} to see a list of available algorithms. Last, use \code{\link{create_analytics}} to analyze the results and determine accuracy rates as well as to prepare the ensemble agreement.
}
\author{
Timothy P. Jurka,  Loren Collingwood, Amber E. Boydstun, Emiliano Grossman, Wouter van Atteveldt

Maintainer: <tpjurka@ucdavis.edu>

}

\examples{
# LOAD THE RTextTools LIBARY
library(RTextTools)


# READ THE CSV DATA
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")

\donttest{
# [OPTIONAL] SUBSET YOUR DATA TO GET A RANDOM SAMPLE
data <- data[sample(1:3100,size=1000,replace=FALSE),]


# CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
# WE WILL TRAIN ON THE Title and Subject COLUMNS
matrix <- create_matrix(cbind(data$Title,data$Subject), language="english", 
removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)


# CREATE A CORPUS THAT IS SPLIT INTO A TRAINING SET AND A TESTING SET
# WE WILL BE USING Topic.Code AS THE CODE COLUMN. WE DEFINE A 750 
# ARTICLE TRAINING SET AND A 250 ARTICLE TESTING SET.
corpus <- create_corpus(matrix,data$Topic.Code,trainSize=1:750, testSize=751:1000, 
virgin=FALSE)


# THERE ARE TWO METHODS OF TRAINING AND CLASSIFYING DATA.
# ONE WAY IS TO DO THEM AS A BATCH (SEVERAL ALGORITHMS AT ONCE)
models <- train_models(corpus, algorithms=c("GLMNET","MAXENT","SVM"))
results <- classify_models(corpus, models)


# ANOTHER WAY IS TO DO THEM ONE BY ONE.
glmnet_model <- train_model(corpus,"GLMNET")
maxent_model <- train_model(corpus,"MAXENT")
svm_model <- train_model(corpus,"SVM")

glmnet_results <- classify_model(corpus,glmnet_model)
maxent_results <- classify_model(corpus,maxent_model)
svm_results <- classify_model(corpus,svm_model)

# USE print_algorithms() TO SEE ALL AVAILABLE ALGORITHMS.
print_algorithms()


# VIEW THE RESULTS BY CREATING ANALYTICS
# IF YOU USED OPTION 1, YOU CAN GENERATE ANALYTICS USING
analytics <- create_analytics(corpus, results)

# IF YOU USED OPTION 2, YOU CAN GENERATE ANALYTICS USING:
analytics <- create_analytics(corpus,cbind(svm_results,maxent_results))

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND 
#  ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE.
#  USES THE n VARIABLE PASSED INTO create_analytics()

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
head(analytics@ensemble_summary)

# WRITE OUT THE DATA TO A CSV
write.csv(analytics@algorithm_summary,"SampleData_AlgorithmSummary.csv")
write.csv(analytics@label_summary,"SampleData_LabelSummary.csv")
write.csv(analytics@document_summary,"SampleData_DocumentSummary.csv")
write.csv(analytics@ensemble_summary,"SampleData_EnsembleSummary.csv")
}
}

# AUTHOR: Timothy P. Jurka
# DESCRIPTION: This file demonstrates the simplest way to use RTextTools via the "wizard" functions.

# LOAD THE RTextTools LIBARY
library(RTextTools)


# USE THE WIZARD TO READ THE CSV DATA
# WE WILL TRAIN ON THE Title and Subject COLUMNS, USING
# Topic.Code AS THE CODE COLUMN. WE DEFINE A 2000 ARTICLE TRAINING
# SET AND A 1000 ARTICLE TESTING SET.
corpus = wizard_read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"), textColumns=c("Title","Subject"), codeColumn="Topic.Code", trainSize=1500, testSize=400)


# USE THE WIZARD TO TRAIN/TEST THE DATA
# RESULTS WILL BE REPORTED BACK IN THE results VARIABLE.
# results@label_summary: SUMMARY OF TOPIC ACCURACY
# results@algorithm_summary: SUMMARY OF ALGORITHM ACCURACY
# results@document_summary : SIMPLE SUMMARY OF DOCUMENT CODING
# results@ensemble_summary : RAW SUMMARY OF ALL DATA AND SCORING
results = wizard_train_classify(corpus, c("SVM", "MAXENT", "GLMNET"))

head(results@label_summary)
head(results@algorithm_summary)
head(results@document_summary)
head(results@ensemble_summary)


# WRITE OUT THE DATA TO A CSV
write.csv(results@algorithm_summary,"SampleData_AlgorithmSummary.csv")
write.csv(results@label_summary,"SampleData_LabelSummary.csv")
write.csv(results@document_summary,"SampleData_DocumentSummary.csv")
write.csv(results@ensemble_summary,"SampleData_EnsembleSummary.csv")
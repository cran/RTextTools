# AUTHOR: Tim Jurka
# DESCRIPTION: This file demonstrates saving a trained model and using it to classify new data.

# LOAD THE RTextTools LIBARY
library(RTextTools)

# READ THE CSV DATA
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")


# [OPTIONAL] SUBSET YOUR DATA TO GET A RANDOM SAMPLE
data <- data[sample(1:3000,size=3000,replace=FALSE),]


# CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
# WE WILL TRAIN ON THE Title and Subject COLUMNS
matrix <- create_matrix(cbind(data$Title,data$Subject), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf)

# CREATE A CORPUS THAT IS SPLIT INTO A TRAINING SET AND A TESTING SET
# WE WILL BE USING Topic.Code AS THE CODE COLUMN. WE DEFINE A 2000 
# ARTICLE TRAINING SET AND A 1000 ARTICLE TESTING SET.
corpus <- create_corpus(matrix,data$Topic.Code,trainSize=1:3000,virgin=FALSE)


# THERE ARE TWO METHODS OF TRAINING AND CLASSIFYING DATA.
# ONE WAY IS TO DO THEM AS A BATCH (SEVERAL ALGORITHMS AT ONCE)
models <- train_models(corpus, algorithms=c("SVM","MAXENT"))

# NOW SAVE THE ORIGINAL TERM-DOCUMENT MATRIX AND THE TRAINED MODELS
save(matrix,file="originalMatrix.Rd")
save(models,file="trainedModels.Rd")
rm(list=c("data","matrix","corpus","models")) # DELETE THE OLD DATA NOW THAT IT'S SAVED


# CLASSIFYING USING THE TRAINED MODELS
# READ THE CSV DATA
library(RTextTools)
data <- read_data(system.file("data/NYTimes.csv.gz",package="RTextTools"),type="csv")


# [OPTIONAL] SUBSET YOUR DATA TO GET A RANDOM SAMPLE
data <- data[sample(3000:3100,size=100,replace=FALSE),]
load("originalMatrix.Rd")
load("trainedModels.Rd")

# CREATE A TERM-DOCUMENT MATRIX THAT REPRESENTS WORD FREQUENCIES IN EACH DOCUMENT
# WE WILL TRAIN ON THE Title and Subject COLUMNS
new_matrix <- create_matrix(cbind(data$Title,data$Subject), language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTfIdf, originalMatrix=matrix)

# CREATE A CORPUS THAT IS SPLIT INTO A TRAINING SET AND A TESTING SET
# WE WILL BE USING Topic.Code AS THE CODE COLUMN. WE DEFINE A 2000 
# ARTICLE TRAINING SET AND A 1000 ARTICLE TESTING SET.
corpus <- create_corpus(new_matrix,data$Topic.Code,testSize=1:100,virgin=FALSE)

results <- classify_models(corpus, models)


# VIEW THE RESULTS BY CREATING ANALYTICS
# IF YOU USED OPTION 1, YOU CAN GENERATE ANALYTICS USING
analytics <- create_analytics(corpus, results)

# RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
# analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
# analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
# analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
# analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
head(analytics@ensemble_summary)

# WRITE OUT THE DATA TO A CSV
write.csv(analytics@algorithm_summary,"SampleData_AlgorithmSummary.csv")
write.csv(analytics@label_summary,"SampleData_LabelSummary.csv")
write.csv(analytics@document_summary,"SampleData_DocumentSummary.csv")
write.csv(analytics@ensemble_summary,"SampleData_EnsembleSummary.csv")
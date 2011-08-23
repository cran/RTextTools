train_model <- function(corpus,algorithm=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET","MAXENT"),
						method="C-classification", cross=0, cost=100, kernel="radial",  # SVM PARAMETERS
						maxitboost=100, # BOOSTING PARAMETERS
						maxitglm=10^5, # GLMNET PARAMETERS
						size=1,maxitnnet=1000,MaxNWts=10000,rang=0.1,decay=5e-4,trace=FALSE, # NNET PARAMETERS
						ntree=200, # RF PARAMETERS
						feature_cutoff=0,gaussian_prior=0,inequality_constraints=0, # MAXENT PARAMETERS
						...) {
        
        # CLEAN UP FROM PREVIOUS MODEL TRAINED
        gc()
        
        # CONDITIONAL TRAINING OF MODEL
        if (algorithm=="SVM") {
			model <- svm(x=corpus@training_matrix, y=corpus@training_codes, method=method, cross=cross, cost=cost, probability=TRUE, kernel=kernel)
		} else if (algorithm=="SLDA") {
           model <- slda(corpus.training_codes ~ ., data=data.frame(as.matrix(corpus@training_matrix),corpus@training_codes))
        } else if (algorithm=="BOOSTING") {
            model <- LogitBoost(xlearn=as.matrix(corpus@training_matrix), ylearn=corpus@training_codes, nIter=maxitboost)
        } else if (algorithm=="BAGGING") {
            model <- bagging(corpus.training_codes ~ ., data=data.frame(as.matrix(corpus@training_matrix),corpus@training_codes))
        } else if (algorithm=="RF") {
            model <- randomForest(x=as.matrix(corpus@training_matrix), y=corpus@training_codes, ntree=ntree)
        } else if (algorithm=="GLMNET") {
			training_matrix <- as(as.matrix.csc(corpus@training_matrix),"dgCMatrix")
			#colnames(training_matrix) <- corpus@column_names
            model <- glmnet(x=training_matrix, y=corpus@training_codes, family="multinomial", maxit=maxitglm)
        } else if (algorithm=="TREE") {
            model <- tree(corpus.training_codes ~ ., data=data.frame(as.matrix(corpus@training_matrix),corpus@training_codes))
        } else if (algorithm=="NNET") {
            model <- nnet(corpus.training_codes ~ ., data=data.frame(as.matrix(corpus@training_matrix),corpus@training_codes), size=size, maxit=maxitnnet, MaxNWts=MaxNWts, rang=rang, decay=decay, trace=trace)
        } else if (algorithm=="MAXENT") {
			model <- maxent(corpus@training_matrix,as.vector(corpus@training_codes),feature_cutoff=feature_cutoff,gaussian_prior=gaussian_prior,inequality_constraints=inequality_constraints)
		} else {
			stop("ERROR: Invalid algorithm specified. Type print_algorithms() for a list of available algorithms.")
		}
		
		# RETURN TRAINED MODEL
		gc() # CLEAN UP AFTER MODEL
		return(model)
}
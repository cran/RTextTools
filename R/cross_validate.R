cross_validate <- function(corpus,nfold,algorithm=c("SVM","SLDA","BOOSTING","BAGGING","RF","GLMNET","TREE","NNET","MAXENT"),seed=NA,
							method="C-classification", cross=0, cost=100, kernel="radial",  # SVM PARAMETERS
							maxitboost=100, # BOOSTING PARAMETERS
							maxitglm=500, # GLMNET PARAMETERS
							size=1,maxitnnet=1000,MaxNWts=10000,rang=0.1,decay=5e-4, # NNET PARAMETERS
							ntree=200, # RF PARAMETERS
							feature_cutoff=0,gaussian_prior=0,inequality_constraints=0 # MAXENT PARAMETERS
							) {

    options(warn=-1) #Supress warnings
    if (!is.na(seed)) #Set seed for replicability
        set.seed(seed)
    #Bring in info from the corpus function    
	alldata <- rbind(corpus@training_matrix,corpus@classification_matrix) #put all data together
	#alldata <- corpus@full_matrix
	allcodes <- as.factor(c(corpus@training_codes,corpus@testing_codes))
    #Sample
    rand <- sample(nfold,dim(alldata)[1], replace=T) #replace
    
    cv_accuracy <- NULL
    for (i in sort(unique(rand))) {
        if (algorithm=="SVM") {
            model <- svm(x=alldata[rand!=i,], y=allcodes[rand!=i],method=method,cross=cross,cost=cost,kernel=kernel) #put function here
            pred <- predict(model,alldata[rand==i,])
        } else
		if (algorithm=="SLDA") {
			alldata <- rbind(as.matrix(corpus@training_matrix),as.matrix(corpus@classification_matrix))
			colnames(alldata) <- corpus@column_names
			data_and_codes <- cbind(as.matrix(alldata),allcodes)
			model <- slda(as.factor(allcodes)~.,data=data.frame(data_and_codes[rand!=i,]))
            pred <- predict(model,data.frame(alldata[rand==i,]))
			pred <- as.numeric(pred$class)
		} else
        
        if (algorithm=="RF") {
			alldata <- rbind(as.matrix(corpus@training_matrix),as.matrix(corpus@classification_matrix))
			colnames(alldata) <- corpus@column_names
			data_and_codes <-cbind(alldata,allcodes)
            model <- randomForest(as.factor(allcodes)~.,data=data_and_codes[rand!=i,], ntree=ntree)
            pred <- predict(model,newdata=alldata[rand==i,])
        } else
        if (algorithm=="GLMNET") {
			sparsedata <- as(as.matrix.csc(alldata[rand!=i,]),"dgCMatrix")
            model <- glmnet(x=sparsedata, y=as.vector(allcodes[rand!=i]),family="multinomial", maxit=maxitglm)
			
			sparsedata <- as(as.matrix.csc(alldata[rand==i,]),"dgCMatrix")
            prob <- predict(model,sparsedata,s=0.01,type="response")            
            pred <- apply(prob[,,1],1,which.max)
 
        } else
        if (algorithm=="BOOSTING") {
			alldata <- rbind(as.matrix(corpus@training_matrix),as.matrix(corpus@classification_matrix))
			colnames(alldata) <- corpus@column_names
			data_and_codes <- cbind(alldata,allcodes)
            model <- LogitBoost(x=alldata[rand!=i,], y=allcodes[rand!=i],maxitboost)
            pred <- predict(model,data.frame(alldata[rand==i,]))
        } else
		if (algorithm=="BAGGING") {
			alldata <- rbind(as.matrix(corpus@training_matrix),as.matrix(corpus@classification_matrix))
			colnames(alldata) <- corpus@column_names
			data_and_codes <-cbind(alldata,allcodes)
            model <- bagging(as.factor(allcodes)~.,data=data.frame(data_and_codes[rand!=i,]))
            pred <- predict(model,newdata=alldata[rand==i,])
		} else
        if (algorithm=="TREE") {
			alldata <- rbind(as.matrix(corpus@training_matrix),as.matrix(corpus@classification_matrix))
			colnames(alldata) <- corpus@column_names
            data_and_codes <- cbind(alldata,allcodes)
            model <- tree(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]))
            prob <- predict(model,newdata=data.frame(alldata[rand==i,]), type="vector")
            pred <- apply(prob,1,which.max)

        } else
        if(algorithm=="NNET") {
			alldata <- rbind(as.matrix(corpus@training_matrix),as.matrix(corpus@classification_matrix))
			colnames(alldata) <- corpus@column_names
			data_and_codes <- cbind(alldata,allcodes)
            model <- nnet(as.factor(allcodes)~ ., data = data.frame(data_and_codes[rand!=i,]),size=size,maxit=maxitnnet,MaxNWts=MaxNWts,rang=rang,decay=decay,trace=FALSE)
            prob <- predict(model,newdata=data.frame(alldata[rand==i,]))
            pred <- apply(prob,1,which.max)
        } else
		if (algorithm=="MAXENT") {
			model <- maxent(corpus@training_matrix,as.vector(corpus@training_codes),feature_cutoff=feature_cutoff,gaussian_prior=gaussian_prior,inequality_constraints=inequality_constraints)
			pred <- predict(model,alldata[rand==i,])
			pred <- pred[,1]
		}

        cv_accuracy[i] <- recall_accuracy(allcodes[rand==i],pred)
		
        cat("Fold ",i," Out of Sample Accuracy"," = ",cv_accuracy[i],"\n",sep="")
    }
    #GLMNET sometimes has problems with 
    if (algorithm=="GLMNET") {
        return(list(cv_accuracy))
    } else {
        return(list(cv_accuracy,meanAccuracy=mean(cv_accuracy)))
    }
}

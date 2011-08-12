create_analytics <- function(corpus,classification_results,b=1,threshold=NULL) {
	score_classify <- function(corpus, results) {
		labels <- c()
		probs <- c()
		
		for (name in colnames(results)) {
			if(strsplit(name,"_")[[1]][2] == "LABEL") {
				labels <- as.data.frame(c(labels,results[name]));
			} else {
				probs <- as.data.frame(c(probs,results[name]));
			}
		}
		
		best_labels <- c()
		best_probs <- c()
		agree_score <- c()
		unique <- sort(unique(corpus@training_codes))
		for (i in 1:nrow(results)) {
			row_labels <- labels[i,]
			row_probs <- probs[i,]
			dist <- c()
			
			for (code in unique) dist <- append(dist,length(row_labels[row_labels==code]))
			
			if(length(colnames(probs)) > 1) {
				best_prob_name <- colnames(t(which.max(probs[i,])))[1]
				parse_prob_name <- strsplit(best_prob_name,"_")
				create_label_name <- paste(parse_prob_name[[1]][1],"_LABEL",sep="")
			} else {
				best_prob_name <- colnames(probs)
				parse_prob_name <- strsplit(best_prob_name,"_")
				create_label_name <- paste(parse_prob_name[[1]][1],"_LABEL",sep="")
			}
			
			best_probs <- append(best_probs,results[create_label_name][i,])
			best_labels <- append(best_labels,as.vector(unique[which.max(dist)]))
			agree_score <- append(agree_score,as.vector(max(dist)))
		}
		
		return(cbind(labels,BEST_LABEL=as.numeric(best_labels),BEST_PROB=best_probs, NUM_AGREE=agree_score))
	}
	
	create_documentSummary <- function(container, scores) {
		return(cbind(MANUAL_CODE=as.numeric(as.vector(container@testing_codes)),CONSENSUS_CODE=scores$BEST_LABEL,CONSENSUS_AGREE=scores$NUM_AGREE,CONSENSUS_INCORRECT=container@testing_codes!=scores$BEST_LABEL,PROBABILITY_CODE=scores$BEST_PROB,PROBABILITY_INCORRECT=container@testing_codes!=scores$BEST_PROB))
	}
	
	create_topicSummary <- function(container, scores) {
		topic_codes <- unique(container@training_codes)
		manually_coded <- c()
		automatically_coded_label <- c()
		automatically_coded_prob <- c()
		correctly_coded_label <- c()
		correctly_coded_prob <- c()
		over_coded_label <- c()
		over_coded_prob <- c()
		
		for (code in topic_codes) {
			num_manual <- length(container@testing_codes[container@testing_codes==code])
			num_automatic_label <- length(scores$BEST_LABEL[scores$BEST_LABEL==code])
			pct_analysis_label <- container@testing_codes[container@testing_codes==scores$BEST_LABEL]==code
			pct_correct_label <- length(pct_analysis_label[pct_analysis_label == TRUE])/num_manual
			
			num_automatic_prob <- length(scores$BEST_PROB[scores$BEST_PROB==code])
			pct_analysis_prob <- container@testing_codes[container@testing_codes==scores$BEST_PROB]==code
			pct_correct_prob <- length(pct_analysis_prob[pct_analysis_prob == TRUE])/num_manual
			
			manually_coded <- append(manually_coded,num_manual)
			
			automatically_coded_label <- append(automatically_coded_label,num_automatic_label)
			correctly_coded_label <- append(correctly_coded_label,pct_correct_label*100)
			over_coded_label <- append(over_coded_label,num_automatic_label/num_manual*100)
			
			automatically_coded_prob <- append(automatically_coded_prob,num_automatic_prob)
			correctly_coded_prob <- append(correctly_coded_prob,pct_correct_prob*100)
			over_coded_prob <- append(over_coded_prob,num_automatic_prob/num_manual*100)
		}
		
		return(cbind(TOPIC_CODE=as.numeric(as.vector(topic_codes)),NUM_MANUALLY_CODED=manually_coded,NUM_CONSENSUS_CODED=automatically_coded_label,NUM_PROBABILITY_CODED=automatically_coded_prob,PCT_CONSENSUS_CODED=over_coded_label,PCT_PROBABILITY_CODED=over_coded_prob,PCT_CORRECTLY_CODED_CONSENSUS=correctly_coded_label,PCT_CORRECTLY_CODED_PROBABILITY=correctly_coded_prob))
	}
	
	create_algorithmSummary <- function(container, scores) {
		topic_codes <- unique(container@testing_codes)
		
		bagging_accuracy <- c()
		slda_accuracy <- c()
		logitboost_accuracy <- c()
		svm_accuracy <- c()
		forests_accuracy <- c()
		glmnet_accuracy <- c()
		tree_accuracy <- c()
		nnetwork_accuracy <- c()
		maxentropy_accuracy <- c()
		
		algorithm_summary <- cbind(TOPIC_CODE=as.numeric(as.vector(topic_codes)))
		columns <- colnames(scores)
		
		for (code in topic_codes) {
			if (pmatch("SVM_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$SVM_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				svm_accuracy <- append(svm_accuracy,pct_correct)
			}
			
			if (pmatch("SLDA_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$SLDA_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				slda_accuracy <- append(slda_accuracy,pct_correct)
			}
			
			if (pmatch("LOGITBOOST_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$LOGITBOOST_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				logitboost_accuracy <- append(logitboost_accuracy,pct_correct)
			}
			
			if (pmatch("BAGGING_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$BAGGING_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				bagging_accuracy <- append(bagging_accuracy,pct_correct)
			}
			
			if (pmatch("FORESTS_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$FORESTS_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				forests_accuracy <- append(forests_accuracy,pct_correct)
			}
			
			if (pmatch("GLMNET_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$GLMNET_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				glmnet_accuracy <- append(glmnet_accuracy,pct_correct)
			}
			
			if (pmatch("TREE_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$TREE_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				tre_accuracy <- append(tree_accuracy,pct_correct)
			}
			
			if (pmatch("NNETWORK_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$NNETWORK_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				nnetwork_accuracy <- append(nnetwork_accuracy,pct_correct)
			}
			
			if (pmatch("MAXENTROPY_LABEL",columns,nomatch=0) > 0) {
				num_manual <- length(container@testing_codes[container@testing_codes==code])
				pct_analysis <- container@testing_codes[container@testing_codes==scores$MAXENTROPY_LABEL]==code
				pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
				maxentropy_accuracy <- append(maxentropy_accuracy,pct_correct)
			}
			
		}
		
		if (length(bagging_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,BAGGING_ACCURACY=bagging_accuracy*100)
		if (length(slda_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,SLDA_ACCURACY=slda_accuracy*100)
		if (length(logitboost_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,LOGITBOOST_ACCURACY=logitboost_accuracy*100)
		if (length(svm_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,SVM_ACCURACY=svm_accuracy*100)
		if (length(forests_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,FORESTS_ACCURACY=forests_accuracy*100)
		if (length(glmnet_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,GLMNET_ACCURACY=glmnet_accuracy*100)
		if (length(tree_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,TREE_ACCURACY=tree_accuracy*100)
		if (length(nnetwork_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,NNETWORK_ACCURACY=nnetwork_accuracy*100)
		if (length(maxentropy_accuracy) > 0) algorithm_summary <- cbind(algorithm_summary,MAXENTROPY_ACCURACY=maxentropy_accuracy*100)
		
		return(algorithm_summary)
	}
	
	if (is.null(threshold)) threshold <- (ncol(classification_results)/2)
	if (corpus@virgin == FALSE) {
		score_summary <- score_classify(corpus, classification_results)
		document_summary <- create_documentSummary(corpus, score_summary)
		topic_summary <- as.data.frame(create_topicSummary(corpus, score_summary))
		algorithm_summary <- as.data.frame(create_algorithmSummary(corpus, score_summary))
		statistics_summary <- as.data.frame(create_precisionRecallSummary(corpus, classification_results, b))
		
		topic_summary <- topic_summary[with(topic_summary, order(TOPIC_CODE)),]
		row.names(topic_summary) <- topic_summary[,1]
		algorithm_summary <- algorithm_summary[with(algorithm_summary, order(TOPIC_CODE)),]
		
		raw_summary <- cbind(classification_results,document_summary)
		algorithm_summary <- cbind(statistics_summary, algorithm_summary)
		algorithm_summary <- algorithm_summary[,(-ncol(statistics_summary)-1)]
		
		ensemble_summary <- create_ensembleSummary(as.data.frame(raw_summary),threshold=threshold)
		
		container <- new("analytics_container", label_summary=as.data.frame(topic_summary)[,-1], document_summary=as.data.frame(raw_summary), algorithm_summary=as.data.frame(algorithm_summary), ensemble_summary=ensemble_summary)
    } else {
		score_summary <- score_classify(corpus, classification_results)
		
		document_summary <- create_documentSummary(corpus, score_summary)
		document_summary <- document_summary[,c(2,3,5)]
		#cbind(document_summary$CONSENSUS_CODE,document_summary$CONSENSUS_AGREE,document_summary$PROBABILITY_CODE)

		raw_summary <- cbind(classification_results, document_summary)
		
		topic_summary <- create_topicSummary(corpus, score_summary)
		topic_summary <- as.data.frame(topic_summary[,c(1,3,4)])
		topic_summary <- topic_summary[with(topic_summary, order(TOPIC_CODE)),]
		row.names(topic_summary) <- topic_summary[,1]
		#cbind(topic_summary$TOPIC_CODE,topic_summary$NUM_CONSENSUS_CODED,topic_summary$NUM_PROBABILITY_CODED)
		
		container <- new("analytics_container_virgin", label_summary=as.data.frame(topic_summary)[,-1], document_summary=as.data.frame(raw_summary))
	}
	
    return(container)   
}
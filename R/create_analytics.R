create_analytics <- function(corpus,classification_results,b=1) {	
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
		
        accuracies <- list()
		
		algorithm_summary <- cbind(TOPIC_CODE=as.numeric(as.vector(topic_codes)))
		columns <- colnames(scores)
        labels <- c()
		
		for (code in topic_codes) {
            for (i in seq(1,length(columns)-3)) {
                num_manual <- length(container@testing_codes[container@testing_codes==code])
                pct_analysis <- container@testing_codes[container@testing_codes==scores[,i]]==code
                pct_correct <- length(pct_analysis[pct_analysis == TRUE])/num_manual
                
                accuracies[[columns[i]]] <- append(accuracies[[columns[i]]],pct_correct)
			}
		}
		
        for (i in seq(1,length(columns)-3)) {
            algorithm_summary <- cbind(algorithm_summary,accuracies[[i]]*100)
            
            label <- paste(strsplit(columns[i],"_")[[1]][1],"_ACCURACY",sep="")
            labels <- append(labels,label)
        }

        colnames(algorithm_summary) <- c("TOPIC_CODE", labels)
		
		return(algorithm_summary)
	}
	
	if (corpus@virgin == FALSE) {
		#print(system.time(score_summary <- create_scoreSummary(corpus, classification_results)))
		#print(system.time(document_summary <- create_documentSummary(corpus, score_summary)))
		#print(system.time(topic_summary <- as.data.frame(create_topicSummary(corpus, score_summary))))
		#print(system.time(algorithm_summary <- as.data.frame(create_algorithmSummary(corpus, score_summary))))
		#print(system.time(statistics_summary <- as.data.frame(create_precisionRecallSummary(corpus, classification_results, b))))
		
		score_summary <- create_scoreSummary(corpus, classification_results)
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
		
		ensemble_summary <- create_ensembleSummary(as.data.frame(raw_summary))
		
		container <- new("analytics_container", label_summary=as.data.frame(topic_summary)[,-1], document_summary=as.data.frame(raw_summary), algorithm_summary=as.data.frame(algorithm_summary), ensemble_summary=ensemble_summary)
    } else {
		score_summary <- create_scoreSummary(corpus, classification_results)
		
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
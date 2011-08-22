create_scoreSummary <- function(corpus, classification_results) {
	labels <- c()
	probs <- c()
	
	for (name in colnames(classification_results)) {
		if(strsplit(name,"_")[[1]][2] == "LABEL") {
			labels <- as.data.frame(c(labels,classification_results[name]));
		} else {
			probs <- as.data.frame(c(probs,classification_results[name]));
		}
	}
	
	best_labels <- c()
	best_probs <- c()
	agree_score <- c()
	unique <- sort(unique(corpus@training_codes))
	for (i in 1:nrow(classification_results)) {
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
		
		best_probs <- append(best_probs,classification_results[create_label_name][i,])
		best_labels <- append(best_labels,as.vector(unique[which.max(dist)]))
		agree_score <- append(agree_score,as.vector(max(dist)))
	}
	
	return(cbind(labels,BEST_LABEL=as.numeric(best_labels),BEST_PROB=best_probs, NUM_AGREE=agree_score))
}

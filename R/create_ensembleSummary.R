create_ensembleSummary <- function(score_summary, threshold) {
	algorithms <- score_summary[score_summary$CONSENSUS_AGREE>=threshold,]
	coverage <- paste("Minimum",threshold,"ensemble agreement coverage is",round(dim(algorithms)[1]/dim(score_summary)[1],2))
	recall <- paste("Minimum",threshold,"ensemble recall accuracy is", round(recall_accuracy(algorithms$MANUAL_CODE,algorithms$CONSENSUS_CODE),2))
	return(rbind(coverage,recall))
}
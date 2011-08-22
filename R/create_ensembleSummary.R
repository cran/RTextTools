create_ensembleSummary <- function(document_summary, threshold) {
	algorithms <- document_summary[document_summary$CONSENSUS_AGREE>=threshold,]
	coverage <- paste("Minimum",threshold,"ensemble agreement coverage is",round(dim(algorithms)[1]/dim(document_summary)[1],2))
	recall <- paste("Minimum",threshold,"ensemble recall accuracy is", round(recall_accuracy(algorithms$MANUAL_CODE,algorithms$CONSENSUS_CODE),2))
	return(rbind(coverage,recall))
}
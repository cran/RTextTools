wizard_read_data <- function(filename, tablename=NULL, filetype="csv", virgin=FALSE, textColumns, codeColumn, trainSize, testSize, ...) {
# convenience method to read a corpus object from source
# combines read_data, create_matrix, and create_corpus
# ellipsis options go to create_matrix
# trainSize and testSize should be integers, the wizard will sample from the file
	
# read data from source
	data <- read_data(filename, tablename, type=filetype)
# randomize and sample |trainSize| + |testSize|  documents
	data <- data[sample(1:dim(data)[1], size=trainSize+testSize,replace=F),]
	
# create td matrix from text columns
	columns <- data[,textColumns[1]]
	if (length(textColumns) > 1) {
		for (i in 2:length(textColumns)) {
			 columns <- cbind(columns,data[,textColumns[i]])
		}		
	}
	data_matrix <- create_matrix(columns, ...)
	
# add code colunm and subset to create corpus
	return(create_corpus(data_matrix,data[,codeColumn],trainSize=1:trainSize, testSize=(trainSize+1):(trainSize+testSize), virgin=virgin))
}
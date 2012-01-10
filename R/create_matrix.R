create_matrix <- function(textColumns, language="english", minDocFreq=1, minWordLength=3, ngramLength=0, originalMatrix=NULL, removeNumbers=FALSE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=TRUE,  stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
	
    stem_words <- function(x) {
        split <- strsplit(x," ")
        return(wordStem(split[[1]],language=language))
    }
    
    tokenize_ngrams <- function(x, n=ngramLength) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
	
	control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
    
    if (ngramLength > 0) control <- append(control,list(tokenize=tokenize_ngrams),after=6)
    if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
    
    trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
    trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")

	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=control);
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
	
    if (!is.null(originalMatrix)) {
        diff_new <- pmatch(colnames(originalMatrix),colnames(matrix),nomatch=0)
        terms_new <- colnames(matrix)[diff_new]

        diff_old <- which(colnames(originalMatrix)!=colnames(matrix))
        terms_old <- colnames(originalMatrix)[diff_old]

        diff <- pmatch(terms_new,terms_old,nomatch=0)
        terms <- terms_old[-diff]

        weight <- 0
        if (attr(weighting,"Acronym")=="tf-idf") weight <- 0.000000001
        amat <- matrix(weight,nrow=nrow(matrix),ncol=length(terms))
        colnames(amat) <- terms
        rownames(amat) <- rownames(matrix)

        fixed <- as.DocumentTermMatrix(cbind(matrix[,diff_new],amat),weighting=weighting)
        matrix <- fixed
    }

	gc()
	return(matrix)
}
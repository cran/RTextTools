wizard_train_classify <- function(corpus, algorithms, ...) {
# convenience method to train model(s), classify, and run analytics
# combines train_models, classify_models, and create_analytics
	models = train_models(corpus,  algorithms, ...)
	result = classify_models(corpus, models) 
	return(create_analytics(corpus, result))
}
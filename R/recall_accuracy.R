recall_accuracy <- function(true_labels, predicted_labels) 
{
    a <- cbind(true_labels, predicted_labels)
#Function for Apply, compares each row one by one.
    identical_row <- function(vector) {
        vec1 <- vector[1]
        vec2 <- vector[2]
        if (vec1 %in% vec2 == FALSE) {
            answer <- "FALSE"
        }
        else {
            answer <- "TRUE"
        }
        return(answer)
    }
    out <- apply(a, 1, identical_row)
#If all cases agree
    if (length(table(out))==1 && names(table(out)) =="TRUE"){
    	out2 <- 100
    }
#If all cases disagree
    else if (length(table(out))==1 && names(table(out)) =="FALSE") {
    	out2 <- 0
    }
#When there's variation, this will happen in 99.9% of cases, exactly
else {
out2 <- table(out)[2]/sum(table(out))
}
names(out2) <- "Recall Accuracy"
return(out2)
}
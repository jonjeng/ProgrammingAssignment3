rankhospital <- function(state, outcome, num="best") {
    ## Read outcome data
    data <- read.csv('outcome-of-care-measures.csv')
    
    ## Check that state and outcome are valid
    if (!(state %in% state.abb)) {
        stop("invalid state")
    }
    if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
        stop("invalid outcome")
    }
    
    if (outcome == 'heart attack') {
        sdata <- data[data["State"] == state,c(2,11)]
    } else if (outcome == 'heart failure') {
        sdata <- data[data["State"] == state,c(2,17)]
    } else if (outcome == 'pneumonia') {
        sdata <- data[data["State"] == state,c(2,23)]
    }
    
    sdata[,2] <- suppressWarnings(as.numeric(as.character(sdata[,2])))
    sdata2 <- na.omit(sdata[order(sdata[2], sdata[1]),])
    if (!(num %in% 1:nrow(sdata))) {
        if (num == 'best') num <- 1
        else if (num == 'worst') num <- nrow(sdata2)
        else return(NA)
    }
    sdata2[num, 1]
    #ret: char vec w name of hospital that has ranking "num"
}
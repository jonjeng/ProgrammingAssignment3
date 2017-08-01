rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv('outcome-of-care-measures.csv')
    
    ## Check that state and outcome are valid
    if (!(state %in% state.abb)) {
        stop("invalid state")
    }
    if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    if (outcome == 'heart attack') {
        sdata <- data[,c(2,7,11)]
    } else if (outcome == 'heart failure') {
        sdata <- data[,c(2,7,17)]
    } else if (outcome == 'pneumonia') {
        sdata <- data[,c(2,7,23)]
    }
    colnames(sdata) <- c("Hospital.Name", "State", "Rate")
    sdata[,3] <- suppressWarnings(as.numeric(as.character(sdata[,3])))
    sdata <- na.omit(sdata[order(sdata[2], sdata[3], sdata[1]),])
    sdata <- split(sdata, sdata$State)
    res <- lapply(sdata, function(x, num) {
        if (!is.numeric(num)) {
            if (num == 'best') x$Hospital.Name[1]
            else if (num == 'worst') x$Hospital.Name[nrow(x)]
            else stop("invalid ranking")
        } else x$Hospital.Name[num]
    }, num)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    data.frame(hospital=unlist(res), state=names(res))
}
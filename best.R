best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!(state %in% state.abb)) {
        stop("invalid state")
    }
    if (!(outcome %in% c('heart attack', 'heart failure', 'pneumonia'))) {
        stop("invalid outcome")
    }
    
    ## Ret hospital.name in that state w least 30d death rate
    rates <- data[data['State'] == state,]
    
    if (outcome == 'heart attack') {
        rates[which.min(as.numeric(rates[,11])), "Hospital.Name"]
    } else if (outcome == 'heart failure') {
        rates[which.min(as.numeric(rates[,17])), "Hospital.Name"]
    } else if (outcome == 'pneumonia') {
        rates[which.min(as.numeric(rates[,23])), "Hospital.Name"]
    }
}
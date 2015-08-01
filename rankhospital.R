rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    ## purge data of all columns not needed and rename column to match values
    ## this will make it easier to sort by column later
    data <- data[,c(2,7,11,17,23)]
    names(data)[3] <- "heart attack"
    names(data)[4] <- "heart failure"
    names(data)[5] <- "pneumonia"
    
    ## Check that state and outcome are valid
    ## Get all unique state abbreviations from dataset
    allStates <- unique(data[,2])
    
    ## Tests if value provided to function is in the list of states
    ## If not return error message
    if(is.element(state,allStates)){
        validOutcomes <- c("heart attack","heart failure","pneumonia")
        
        ## Now we test for valid outcome. If valid proceed to look up data
        ## If not return error message
        if(is.element(outcome,validOutcomes)){
            
            ##Trim down the data set to only what we need
            switch(outcome,
                   "heart attack" = data <- data[data$State == state,1:3],
                   "heart failure" = data <- data[data$State == state,c(1:2,4)],
                   "pneumonia" = data <- data[data$State == state,c(1:2,5)])
            
            ## Make column 3 numeric
            data[,3] <- suppressWarnings(as.numeric(data[,3]))
            
            ## Remove NAs
            data <- data[!is.na(data[,3]),]
            
            ## Order data
            data <- data[order(data[,3],data[,1]),]
            
            ## Add column to data.frame that ranks in numerical order
            ## getting length of rows
            
            data[,4] <- as.numeric(1:length(data[,3]))
            
            
            if(num == "best" || num == "worst"){
                switch(num,
                       "best" = Hospital.Name <- data[1,1],
                       "worst" = Hospital.Name <- data[length(data[,3]),1]
                       )
            }
            else if(num > length(data[,3])){
                Hospital.Name <- NA
            }
            else{
                Hospital.Name <- data[num,1]
            }
                
        }
        else{
            stop("invalid outcome")
        }
    }
    else{
        stop("invalid state")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    
    return(Hospital.Name)
    ## rate
}
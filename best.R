outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

outcome_df <- read.csv("outcome-of-care-measures.csv")
states_set <- unique(outcome_df$State, na.rm = TRUE)


best <- function(state, outcome) {
  ## Read outcome data
  
  
  
  outcome_subset_state <- subset(outcome_df, outcome_df$State == state)
  if (!(state %in% states_set)) {
    stop("invalid state")
  } 
  else if (outcome == "heart attack") {
    outcome_col <- 11
  }
  else if (outcome == "heart failure") {
    outcome_col <- 17
  }
  else if (outcome == "pneumonia") {
    outcome_col <- 23
  }
  else {
    stop("invalid outcome")
  }
    
  
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}


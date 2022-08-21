outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

setup_best <- function(){
  ## Read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv")
  
  # build the set of States
  states_set <- unique(outcome_df$State, na.rm = TRUE)
  
  # import helper library
  library(dplyr)  
}




best <- function(state, outcome) {
  
  
  ## Check that state and outcome are valid
  
  
  if (!(state %in% states_set)) {
    stop("invalid state")
  } else if (outcome == "heart attack") {
    outcome_col <- 11
  } else if (outcome == "heart failure") {
    outcome_col <- 17
  } else if (outcome == "pneumonia") {
    outcome_col <- 23
  } else {
    stop("invalid outcome")
  }
  
  
  # take only data of the specified State
  outcome_subset_state <- subset(outcome_df, outcome_df$State == state)
  outcome_subset_state[, outcome_col] <- as.numeric(
    outcome_subset_state[, outcome_col])
  
  
  # find the minimal mortality score
  min_mortality <- min(outcome_subset_state[, outcome_col], na.rm = TRUE)
  
  # subset the dataframe considering only minimal scores
  subset_min_mortality <- subset(outcome_subset_state, 
                                 outcome_subset_state[, 
                                                      outcome_col] == 
                                   min_mortality)
  
  # sort th e resulting dataframe by hospital names in ascending order
  # down the alphabet
  winner_hospitals_df <-subset_min_mortality[
    order(subset_min_mortality$Hospital.Name),]
  
  # take the first occurence of winner hospitals vector
  winner_hospital <- winner_hospitals_df$Hospital.Name[1]
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  winner_hospital
  
  
  
}


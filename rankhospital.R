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

rankhospital <- function(state, outcome, num = "best") {
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
  
  outcome_df[, outcome_col] <- as.numeric(
    outcome_df[, outcome_col])
  
  # order the dataframe in ascending order and add the rank column
  outcome_df_ranked <- outcome_df %>%
    filter(State == state) %>%
    filter(!is.na(.[[outcome_col]])) 
  
  outcome_df_ranked <- outcome_df_ranked %>%
    arrange(.[[outcome_col]], Hospital.Name) %>%
    mutate(rank = 1:nrow(outcome_df_ranked))%>%
    select(Hospital.Name, outcome_col, rank, State)
  
  
  if ((num == "best")) {
    ranked_hospital <- head(outcome_df_ranked, n=1)$Hospital.Name
  } else if ((num == "worst")) {
    ranked_hospital <- tail(outcome_df_ranked, n=1)$Hospital.Name
  } else if (num %% 1 == 0 && num > 0 && num <= nrow(outcome_df_ranked)) {
      ranked_hospital_df <- outcome_df_ranked %>% filter(rank == num)
      ranked_hospital <- ranked_hospital_df$Hospital.Name
  } else {
    ranked_hospital <- NA
  }
      
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  ranked_hospital
}
setup_best <- function(){
  ## Read outcome data
  outcome_df <- read.csv("outcome-of-care-measures.csv")
  
  
  # build the set of States
  states_set <- unique(outcome_df$State, na.rm = TRUE)
  
  # import helper library
  library(dplyr)  
}

rankall <- function(outcome, num = "best") {
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
    #filter(!is.na(all_of(outcome_col))) %>%
    rename(outcome = outcome_col)
  
  outcome_df_ranked <- outcome_df_ranked %>%
    arrange(State, Hospital.Name, .[[outcome_col]]) %>%
    group_by(State) %>%
    mutate(Rank = order(order(outcome, Hospital.Name, 
                              decreasing=FALSE)))%>%
    select(Hospital.Name, State, Rank)  
  
  outcome_df_ranked <- outcome_df_ranked %>%
    rename(hospital = Hospital.Name)
  
  if ((num == "best")) {
    ranked_hospital_df <- outcome_df_ranked %>%
      filter(Rank == 1)
  } else if ((num == "worst")) {
    last_ranked <- tail(outcome_df_ranked %>% arrange(Rank), n=1)$Rank
    ranked_hospital_df <- outcome_df_ranked %>%
      filter(Rank == last_ranked)
  } else if (num %% 1 == 0 && num > 0 && num <= nrow(outcome_df_ranked)) {
    ranked_hospital_df <- outcome_df_ranked %>%
      filter(Rank == num)
  } else {
    ranked_hospital_df <- NA
  } 
  
  ranked_hospital_df
  
  
}
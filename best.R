best <- function(state = "character", outcome) {
  
  result <- NULL
  
  ## Read outcome data
  outcome_dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  state_dat <- split(outcome_dat, outcome_dat$State)
  state_detail <- state_dat[[state]]
  
  ## load unique state and outcome
  states <- unique(outcome_dat$State)
  outcomes <- c("heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if(is.null(state) | !(state %in% states)){
    stop("invalid state")
  }
  
  if(is.null(outcome) | !(outcome %in% outcomes)){
    stop("invalid outcome")
  }

  ## Return hospital name in that state with lowest 30-day death rate
  if(outcome == "heart attack"){
    lower_heart_attack_dat <- state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    lower_heart_attack_min <- state_detail$Hospital.Name[suppressWarnings(as.double(state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) ) == suppressWarnings(min(as.double(state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na.rm = TRUE))]
    result <- lower_heart_attack_min
  }

  if(outcome == "heart failure"){
    lower_heart_failure_dat <- state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    lower_heart_failure_min <- state_detail$Hospital.Name[suppressWarnings(as.double(state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) ) == suppressWarnings(min(as.double(state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na.rm = TRUE))]
    result <- lower_heart_failure_min
  }
  
  if(outcome == "pneumonia"){
    lower_pneumonia_dat <- state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    lower_pneumonia_min <- state_detail$Hospital.Name[suppressWarnings(as.double(state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) ) == suppressWarnings(min(as.double(state_detail$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na.rm = TRUE)) ]
    result <- lower_pneumonia_min
  }
  
  sort(result)[1]

}
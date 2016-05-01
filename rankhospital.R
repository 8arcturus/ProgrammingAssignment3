rankhospital <- function(state, outcome, num = "best") {
  
  result <- NULL
  resultdat <- NULL
  
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
  
  
  ## Return hospital name in that state with the given rank
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(outcome == "heart attack"){
    mortality_heart_attack_dat <- state_detail$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    mortality_rate_heart_attack <- state_detail[with(state_detail, order(suppressWarnings(as.double(mortality_heart_attack_dat)), state_detail$Hospital.Name, na.last = NA)),]
  
    resultdat <- mortality_rate_heart_attack$Hospital.Name
  }else if(outcome == "heart failure"){
    mortality_heart_failure_dat <- state_detail$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    mortality_rate_heart_failure <- state_detail[with(state_detail, order(suppressWarnings(as.double(mortality_heart_failure_dat)), state_detail$Hospital.Name, na.last = NA)),]
   
    resultdat <- mortality_rate_heart_failure$Hospital.Name
  }else if(outcome == "pneumonia"){
    pneumonia_heart_failure_dat <- state_detail$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    mortality_rate_pneumonia <- state_detail[with(state_detail, order(suppressWarnings(as.double(pneumonia_heart_failure_dat)), state_detail$Hospital.Name, na.last = NA)),]
    
    resultdat <- mortality_rate_pneumonia$Hospital.Name

  }
  
  ## 30-day death rate
  if(num == "best"){
    result <- unique(resultdat[1])
  }else if(num == "worst"){
    result <- unique(tail(resultdat, 1))
  }else{
    result <- resultdat[num]
  }
  
  if(is.null(result) || length(result) == 0) {
    result <- c(NA)
  }
    result
  
}
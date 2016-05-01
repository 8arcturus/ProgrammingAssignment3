rankall <- function(outcome, num = "best") {
  result <- NULL
  
  
  N <- 1e4  # some magic number, possibly an overestimate
  resultdat <- list()

  
  DF <- data.frame(hospital=rep(NA, N), state=rep("", N),  # as many cols as you need
                   stringsAsFactors=FALSE)
  
  ## Read outcome data
  outcome_dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #state_dat <- split(outcome_dat, outcome_dat$State)
  # state_detail <- state_dat[[state]]
  state_detail <- split(outcome_dat, outcome_dat$State)
  # state_detail <- split(outcome_dat, outcome_dat$State, outcome_dat$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)

  ## load unique state and outcome
  states <- unique(outcome_dat$State)
  outcomes <- c("heart attack","heart failure","pneumonia")

  if(is.null(outcome) | !(outcome %in% outcomes)){
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with the given rank
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(outcome == "heart attack"){

    for(state in state_detail){
      mortality_heart_attack_dat <- state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
      mortality_rate_heart_attack <- state[with(state, order(suppressWarnings(as.double(mortality_heart_attack_dat)), state$Hospital.Name, state$State, na.last = NA)),]
      resultdat <- append(resultdat, list(mortality_rate_heart_attack$Hospital.Name, mortality_rate_heart_attack$State))
    }
    
  }else if(outcome == "heart failure"){
    for(state in state_detail){
      mortality_heart_failure_dat <- state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
      mortality_rate_heart_failure <- state[with(state, order(suppressWarnings(as.double(mortality_heart_failure_dat)), state$Hospital.Name, state$State, na.last = NA)),]
      resultdat <- append(resultdat, list(mortality_rate_heart_failure$Hospital.Name, mortality_rate_heart_failure$State))
    }
  }else if(outcome == "pneumonia"){
    for(state in state_detail){
      mortality_pneumonia_dat <- state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      mortality_rate_pneumonia <- state[with(state, order(suppressWarnings(as.double(mortality_pneumonia_dat)), state$Hospital.Name, state$State, na.last = NA)),]
      resultdat <- append(resultdat, list(mortality_rate_pneumonia$Hospital.Name, mortality_rate_pneumonia$State))
    }
  }
  
  ## 30-day death rate
  if(num == "best"){
    #result <- unique(resultdat[1])
    print(sapply(resultdat, function(x) 
      x[1]
      , simplify = TRUE))
  }else if(num == "worst"){
    #result <- unique(tail(resultdat, 1))
    print(sapply(resultdat, function(x) 
        x[length(x)]
      , simplify = TRUE))
  }else{
    # result <- resultdat$Hospital.Name[num]
    # result <- resultdat$State[num]
    # result <- data.frame(hospital = resultdat$Hospital.Name[num], state = resultdat$State[num], row.names = resultdat$State[num])
    # result <- data.frame(hospital = resultdat$Hospital.Name, state = resultdat$State, row.names = resultdat$State)
    # result <- resultdat[num]
    print(sapply(resultdat, function(x)
      x[num]
      , simplify = TRUE))
  }
  
  if(is.null(result) || length(result) == 0) {
    result <- c(NA)
  }
  result
}
rankhospital <- function(state, outcome, num = "best")
{
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  
  #state <-"MD"
  #outcome <- "heart attack"
  #num <- 4
  #print(state)
  #print(outcome)
  #print(num)
  
  ## Check that state and outcome are valid
  if (!state %in% fd[, "state"]) 
    {
      stop('invalid state')
    } 
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    {
      stop('invalid outcome')
    } 
  
  else if (is.numeric(num)) 
    {
      si <- which(fd[, "state"] == state) #select all the rows that match the state
      #View(fd)
      #View(si)
      ts <- fd[si, ]  # extracting dataframe for the called state
      #View(ts)
      ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
      #View(ts[, eval(outcome)])
      ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
      #View(order(ts[, eval(outcome)]))
      #View(ts[order(ts[, eval(outcome)], ts[, "hospital"]), ])# This sorts by the column outcome and hospital name
      #View(ts[order(ts[, eval(outcome)]), ]) # This sorts by the column outcome only
      
      output <- ts[, "hospital"][num]
              #ts[num, "hospital"]
    } 
  else if (!is.numeric(num))
    {
      if (num == "best") 
        {
          output <- best(state, outcome)
        } 
    else if (num == "worst") 
      {
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]    
        ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
        ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
        output <- ts[, "hospital"][1]
       
        #View(ts[, "hospital"])
        #View(ts[, "hospital"][1])
        #View(ts[1, "hospital"])
      } 
    else 
      {
        stop('invalid rank')
      }
  }
  return(output)
    
}
#create a function
best<-function(state, outcome)
{
  #state <- "MD"
  #outcome <- "heart attack"
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  fd   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

  if(!state %in% fd[, "state"])
    {
      stop('invalid state')
    }   
  else if(!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    {
      stop('invalid outcome')
    } 
  else 
    {
        si <- which(fd[, "state"] == state) # Select the vector(row #) in the Dataset that matches the state
        ts <- fd[si, ]    # extracting data for the called state
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)
        result  <- ts[, "hospital"][which(oi == min_val)]
        output  <- result[order(result)]
    }
  
  return(output)
}

#View(oi) # allows you to see the datafram in an excel format screen
#View(fd[,"heart attack"])

#View(fd[,c("heart attack", "heart failure")]) # selecting multiple rows with more than one column in dataframe 
#View(fd[,"heart attack"]) # shows all the row for the column heart attack
#View(min(oi,na.rm = TRUE))
#View(ts[, "hospital"])
#View(ts[, eval(outcome)])
#View(ts[, outcome])
#View(oi)
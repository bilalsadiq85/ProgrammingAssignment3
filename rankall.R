rankall <- function(outcome, num = "best")
{
  ## Read outcome data
# data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #View(data$State)
  fd   <- as.data.frame(cbind(data[, 2],  # Hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
  
  #View(as.numeric(fd[, eval(outcome)]))
  #View(data)
  #View(fd)
  
  ## Check that state and outcome are valid
  
  #num = "best"
  #outcome = "heart attack"
  
  ## Check that state and outcome are valid
  if(! ( state %in% levels(factor(data$State)) ) ) 
    {
      stop("invalid state")
    }
 
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    {
      stop('invalid outcome')
    }
  
  if(class(num) == "character")
    {
      if (! (num == "best" || num == "worst"))
        {
          stop("invalid number")
        }
  }
  
  ################################
  
  #View(colnum)
  #View(fd[,1]) is the same as fd[,"hospital"]
  #View(fd)
  #View(fd[!is.na(fd[,colnum]),])
  #View(fd[,colnum])
  
  #data1 = fd[fd$State==state,]
  data1 = fd[,c(1,2,3,4,5)]
  
  #View(data1)
  
  if (outcome == "heart attack") 
    {
        data1<- data1[,c(1,2,3)]
        #colnum <- fd[,c(1,2,3)]
        #colnum = fd[,c(3)]
    }
  else if (outcome == "heart failure") 
    {
      data1<- data1[,c(1,2,4)]
    }
  else 
    {
      data1<- data1[,c(1,2,5)] # pneumonia
    }
  

  names(data1)[3] = "Deaths"
  
  #View(data1[!is.na(data1$Deaths),])
  #fd <- fd[!is.na(fd[,colnum]),]
  
  # Remove rows with NA from the Deaths Columns
  data1 <- data1[!is.na(data1$Deaths),]
  
  splited <- split(data1,data1$state)
  
  #View(split(fd,fd$state))
  #View(splited)
  #splited <- split(fd,fd$state)
  
  ans <- lapply(splited, function(x, num)
    {
      # Order by Deaths and then HospitalName
      x = x[order(x$Deaths, x$hospital),]
    
      # Return
      if(class(num) == "character") 
        {
          if(num == "best") 
            {
              return (x$hospital[1])
            }
          else if(num == "worst") 
            {
              return (x$hospital[nrow(x)])
            }
        }
    else 
      {
        return (x$hospital[num])
      }
  }, num)
  
  #Return data.frame with format
  return (data.frame(hospital=unlist(ans), state=names(ans)))
  
}

##      Mike Lennon   Assignment 3


#####   Question 1 #####
#Write a function that takes a vector as input and returns the number of 
#missing values in the vector
numberMissingValues<- function(inputVector){
  count<- 0
  for(value in inputVector){
    if(is.na(value))
      count<- count + 1
  }
  count
  
}

q1<- c(1,NA,2,3,NA)
numberMissingValues(q1)


#####   Question 2 #####
# Write a function that takes a data frame as input and returns a named 
#vector with the number 
#of missing values in each column of the data frame. (The names of the entries 
#should be the corresponding column names of the data frame.) 
#You may use the function from the previous question as part of your solution.

columnNAcounts <- function(frameInput){
  
   names <-colnames(frameInput)
    countNA <- numeric()
  for(i in 1:ncol(frameInput)){
    q2 <- frameInput[, c(i)]
    countNA[length(countNA)+1] <-numberMissingValues(q2)
  }
  answer2 <- paste(names,countNA, sep= "=")
  answer2
}


##### Question 3 #####
# Write a function that takes a numeric vector as input and uses it
#to determine the minimum, the maximum, 
#the mean, the median, the first quartile, the third quartile, the standard 
#deviation of the vector, and the number of missing values. Do not use 
#any built-in functions to do this. Return a named list with the eight
#desired values in an order you deem best. (You may, if you like, 
#use the function you wrote for question 1.)

allStats <- function(inputVector){
  values<- numeric()  # print(length(values))
  
  for( i in 1:length(inputVector)){  # This loop gets rid of the na values
    # print(inputVector[i])  # for debugging
    if(!is.na(inputVector[i]))
      values[length(values) +1] <- inputVector[i]
  }
  # return if the list is zero do nothing
  values<- sort(values, decreasing= FALSE)
  min<- values[1]
  max<- values[length(values)]
  sum <- 0
  for( i in 1:length(values)){
    sum <- sum + values[i]
  }
  mean <- sum/ length(values)
  
  sumSqrDiff <- 0
  for(i in 1:length(values)){
    #print((values[i] - mean)^2)
    sumSqrDiff <- sumSqrDiff + (values[i] - mean)^2
  }
  #print(sumSqrDiff)
  stdDev <- sqrt((sumSqrDiff)/((length(values)-1)))
  missing <- length(inputVector) - length(values)
  
   median <- values[1]
  if(length(values)%%2 != 0){
    median <- values[length(values)%/%2 +1] # determines cutoffs here quartiles
  }
  if(length(values)%%2 == 0){
    median <- (values[length(values)%/%2] + values[length(values)%/%2 +1])/2
  }
  mid <- length(values)%/%2
  qOnePosition <- mid%/%2
  qThreePosition <- (mid + length(values))%/%2
  
  qOne <- values[qOnePosition]
  qThree <- values[qThreePosition]
 
  returnList <- list(minimum= min, maximum = max, med = median, 
      quartileOne = qOne, quartileThree= qThree, Mean = mean, deviation= stdDev,
      numNA = missing)
  
}

q3 <- c(1,2,3,4,5,NA,6)
(allStats(q3))



##### Question 4 #####
#Write a function that takes a character or factor vector and determines the 
#number of distinct elements in the vector, the most commonly occurring element, 
#the number of times the most commonly occurring element occurs, and the number 
#of missing values. (Be sure to handle ties gracefully.) Have the function 
#return a named list with the desired information in a logical order.


distinctValues <- function(input){
  
  missing <- numberMissingValues(input)   #print(missing)
  
  values<- character()  # print(length(values))
  
  for( i in 1:length(input)){  # This loop gets rid of the na values
    # print(inputVector[i])  # for debugging
    if(!is.na(input[i]))
      values[length(values) +1] <- input[i]
  }
  x<- unique(values)
  numUnique <- length(x)#####
  #print(x)
  numOccur <- numeric()
  for(i in 1:length(x)){
    count <- 0
    for(j in 1: length(values))
      if(x[i] == values[j])
        count <- count + 1
  numOccur[length(numOccur)+1]   <- count
  }
  #print(x)   print(numOccur)
 findMode <- sort(numOccur, decreasing= FALSE) 
   most<- findMode[length(findMode)]
   mode <- 0
 for(i in 1:length(numOccur))
   if(numOccur[i] == most)
     mode <-x[i]
   
 #print("Mode") 
 #print(mode)
 #print("Mode occurs") 
# print(most)
 #print("missing ") 
 #print( missing)
 #print("Distinct ") 
 #print(numUnique)
returnList <- list(mostCommon = mode, nummostOccur= most, numDistinct = numUnique, numNA = missing)
}

q4 <- c("2","2","3","3","3","3","5","6","7",NA)
(distinctValues(q4))




##### Question 5 #####
#Write a function that takes a logical vector and determines the number of true 
#values, the number of false values, the proportion of true values, and the 
#number of missing values. Have the function return a named list with
#the desired information in a logical order

logicalCount <- function(inputLogicVector){
  missing <- numberMissingValues(inputLogicVector)
  print(missing)
  #values<- numeric()  # print(length(values))
  
  trueNum <-0
  falseNum <- 0
  
  for( i in 1:length(inputLogicVector)){  # This loop gets rid of the na values
    
    if(!is.na(inputLogicVector[i])){
      #values[length(values) +1] <- inputLogicVector[i]
      if( inputLogicVector[i] == TRUE)
        trueNum <- trueNum + 1
      if( inputLogicVector[i] == FALSE)
        falseNum <- falseNum + 1
    }

   
  }
  #print(trueNum)
  #print(falseNum)
  returnList <- list(numTrue = trueNum, numFalse = falseNum, numNA = missing)
  returnList
}

q5 <- c(TRUE, FALSE, TRUE, TRUE, NA, FALSE)
(logicalCount(q5))









##### Question 6 #####



columnRunFncs <- function(frameInput){
  
  
  names <-colnames(frameInput)
  listAnswer <- list()
  for(i in 1:ncol(frameInput)){
    q6 <- frameInput[, c(i)]
     if(is.logical(q6))
       listAnswer[i] <-logicalCount(q6)
     if(is.numeric(q6))
       listAnswer[i] <- allStats(q6)
     if(is.character(q6)) 
       listAnswer[i] <- distinctValues(q6)
  }
  listAnswer
}

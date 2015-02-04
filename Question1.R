
## Question 1
numberseats <- 25
seats <- rep(0,numberseats)
fiftythou <- rep(0,50000)
tenthou <- rep(0,10000)
thou <- rep(0,1000)


##generate uniform random number between  and length(seats)
set.seed(123)

## test whether seat is empty

##test whether seats on both sides are empty

## if all conditions are true, sit on seat

## Check whether there are any places left to sit
## (3  empty in a row, or 2 empty on an end)

set.seed(123)
## create vectors of empty seats below
## zero denotes an empty seat.. set to 1 when somebody sits in it
numberseats <- 25
seats <- rep(0,numberseats)
fiftythou <- rep(0,50000)


## function below checks whether any seats are available to sit on
## assumes that no rider will occupy a seat next to an occupied seat
## this requires either 3 empty seats in a row, 
##  or 2 empty seats at the end of the row
seatsAvail <- function(seats) {
  ## exit and return true as soon as an available seat is identified  
   if (sum(seats[1:2])==0) {return(TRUE)}
   if (sum(seats[c(length(seats)-1,length(seats))])==0) {return(TRUE)}
   inner <- c(2:(length(seats)-1))
   for (i in inner) {
     if ((seats[i-1]==0) & (seats[i]==0) & (seats[i+1]==0)) {return(TRUE)}
   }
   return(FALSE)
}


seatsAvail(full)

## function willsit calculates whether a person will sit in the selected seat
## assuming the person refuses to sit next to an already occupied seat
willsit <- function(seats,selectSeat) {
  if (selectSeat==1) {
       if ((!seats[1])&(!seats[2])) {return(TRUE)}
       else {return(FALSE)}
  }
  if (selectSeat==length(seats)) {
       if ((!seats[length(seats)])&(!seats[length(seats)-1])) {return(TRUE)}
       else {return(FALSE)}
  } 
  if (!seats[selectSeat]&!seats[selectSeat-1]&!seats[selectSeat+1]) {return(TRUE)}
  else {return(FALSE)}
}

full <- c(1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0)

## function q1 performs a simulation of the subway seat problem 
## as described in question 1
## it takes a vector of 0's corresponding to empty subway seats 
## and the number of replications of the exercise
## defaulting to 100 reps if no value is provided
q1 <- function(seats,reps=100)  {
    totals <- 0; innersum <- 0; outersum <- 0
    for (j in 1:reps) {
        seats <- rep(0,length(seats))
        ## check whether any seats are available, 
        ##  if so, select a seat and see whether you will sit
        while (seatsAvail(seats)) {
            selectSeat <- ceiling(runif(1,min=0,max=length(seats)))
             if (willsit(seats,selectSeat)) {seats[selectSeat] <- 1}
        }
    totals[j] <- sum(seats)/length(seats)
    innersum[j] <- sum(seats[2:(length(seats)-1)])
    outersum[j] <- sum(seats[c(1,length(seats))])
    
#    print(innersum[j])
#    print(outersum[j])
    }
##    print summary of the results of the simulations
    print(paste("# seats =",length(seats)))
    print (paste("N=",length(totals)," mean=",mean(totals)," sd=",sd(totals)))
    print(paste("inner sum mean = ",mean(innersum)/(length(seats)-2)))
    print(paste("outer sum mean = ",mean(outersum)/2))
}

seats
willsit(seats,25)
willsit(seats,24)
willsit(seats,23)
willsit(seats,22)

letters(22)

q1(seats)
((.5*2) + (23*((1/3 + .5)/2)))/25


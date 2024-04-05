### demo code for audpc
# tyr wiesner-hanks
# 2023-05-05

library(tidyverse)
library(agricolae)
set.seed(37)

# simulate some data
dpis = matrix(NA, nrow=5, ncol=5)
r = runif(5, 0, 1)
for (i in 1:5){
  p_0 = r[i]
  pops = sapply(1:5,
                function(x) 100 / (1 + (exp(-x*2*p_0))*(100-p_0)/(p_0)))
  dpis[i,] = round(pops)
}

print(dpis)

# check agricolae's function by hand
audpc(dpis[1,], c(1:5))
(2+5)/2 + (5+13)/2 + (13+31)/2 + (31+57)/2

# vectorized so it works across values that fail
audpc(dpis, c(1:5))

# relative AUDPC is just divided by days elapsed * 100
# 4 days elapsed
# the multiplying * 100 is unusual, usually have seen it just divided by # days?
# ask Chris Smart what she prefers
audpc(dpis[1,], c(1:5), type = 'relative')
78.5 / 400

# what if we introduce NAs into it?
# fails...
dpis_na = dpis
dpis_na[1,3] = NA
audpc(dpis_na[1,], c(1:5))

# most reasonable guess at answer - 83.5
# skip over the missing data point and use the 2 flanking it
(2+5)/2 + 2*(5+31)/2 + (31+57)/2

# if we drop values where either date or value is NA, works fine
audpc(dpis_na[1,c(1,2,4,5)], c(1,2,4,5))

# my attempt at a new function
# modifying the guts of the fxn from agricolae
# not vectorized because you'd need a 
audpc_new = function (evaluation, dates, type = "absolute") {
  if (!(is.null(dim(evaluation)))){
    cat('Error:\nThis function can only be applied to a vector, not matrix or df')
  }
  na_ind = which(!is.na(evaluation) & !is.na(dates))
  dates = dates[na_ind]
  evaluation = evaluation[na_ind]
  
  n <- length(dates)
  k <- length(evaluation)
  if (n != k) {
    cat("Error:\nThe number of dates of evaluation \nmust agree with the number of evaluations\n")
    return()
  }
  if (n == 0 | k == 0){
    return(NA)
  }
  audpc <- 0
  area.total <- 100 * (dates[n] - dates[1])
  for (i in 1:(n - 1)) {
    audpc <- audpc + (evaluation[i] + evaluation[i+1]) * (dates[i + 1] - dates[i])/2
  }
  if (type == "relative") 
    audpc <- audpc/area.total
  if (type == "absolute" | type == "relative") {
    return(audpc)
  }
  else cat("Error: type is 'absolute' or 'relative'\n\n")
}

# seems to work alright
audpc_new(dpis_na[1,], 1:5)

# note that if the last value is NA it probably skews the data
# missing what is (presumably) the highest value
dpis_na[3,5] = NA
audpc_new(dpis_na[3,], 1:5)
(2+8)/2 + (8+24)/2 + (24+54)/2



ds0<-1
ds1<-2
ds2<-7
ds3<-7.5

disease.severity<-c(ds0,ds1,ds2,ds3)


t0<-0
t1<-2
t2<-5
t3<-6

time.period<-c(t0,t1,t2,t3)

help(plot)

plot(time.period,
  disease.severity,
  ylim=c(0,(ds3+1)),
  xlim=c(0,(t3+0.5)),
  xlab="Time",
  ylab="Disease Severity (%)",
  type="o",
  pch=19,
  col="mediumblue")

  title(main="Illustration of AUDPC Calculation",
      sub="Figure 1")

mtext("=t0",1,at=0.3,1)
mtext("=t1",1,at=2.3,1)
mtext("=t2",1,at=5.3,1)
mtext("=t3",1,at=6.3,1)

rect(t0,0,t1,((ds0+ds1)/2),border="orange")
text(1,1,"A1")
rect(t1,0,t2,((ds1+ds2)/2),border="orange")
text(((t1+t2)/2),(((ds1+ds2)/2)/2),"S2")

segments(.4,((ds1+ds2)/2),t2,((ds1+ds2)/2),
         col="black",lty="18")
text(0,((ds1+ds2)/2),((ds1+ds2)/2))
rect(t2,0,t3,((ds2+ds3)/2),border="orange")
text(((t2+t3)/2),(((ds2+ds3)/2)/2),"S3")
segments(0.4,((ds2+ds3)/2),t2,((ds2+ds3)/2),
         col="black",lty="18")
text(0,((ds2+ds3)/2),((ds2+ds3)/2))

audpc <- function(disease.severity,time.period){

  #n is the length of time.period, or
  #  the total number of sample dates
  n <- length(time.period)

  #meanvec is the vector (matrix with one dimension)
  #that will contain the mean percent infection
  #it is initialized containing -1 for all entries
  #this sort of initialization is sometimes useful
  #  for debugging
  meanvec <- matrix(-1,(n-1))

  #intvec is the vector that will contain the length of
  #   time between sampling dates
  intvec <- matrix(-1,(n-1))

  #the loop goes from the first to the penultimate entry
  #the left curly bracket indicates the beginning of
  #   commands in the loop
  for(i in 1:(n-1)){

    #the ith entry in meanvec is replaced with the
    #   mean percent infection
    #between sample time i and sample time i+1
    meanvec[i] <- mean(c(disease.severity[i],
                         disease.severity[i+1]))

    #the ith entry in intvec is replaced with the length
    # of the time interval between time i and time i+1
    intvec[i] <- time.period[i+1] - time.period[i]

    #the right curly bracket ends the loop
  }

  infprod <- meanvec * intvec

  #the sum of the entries in the resulting vector
  #   gives the AUDPC
  sum(infprod)

  #the right curly bracket ends the function
}

#Now apply the function to the example data and put
# the result in a new object called 'AUDPCexample'
audpc(disease.severity,time.period) -> AUDPCexample
#Display AUDPC Value
#Draw rectangle around value
rect(0.1,(ds3+.3),2,(ds3+1),border="black")
#AUDPC Text
text(1.05,(ds3+0.8),"AUDPC")
text(1.05,(ds3+0.5),AUDPCexample)


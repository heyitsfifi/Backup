#
# This R function merges two tables and returns the resulting table in a new data frame.
# inputs
# 1. tbl1 loaded from a IDA .
# 2. tbl2 is output from footfall_2014 containing foreigners and locals
# tbl1 <- previousHour
# tbl2 <- workcluster
#when calling this function type: mergetbl(previousHour,workcluster)

mergetbl <- function(tbl1, tbl2)
{
  
  newtbl = data.frame(hour=numeric(),forgHour=numeric())
  
  ntbl1rows<-nrow(tbl1) # get the number of rows
 
  for(n in 1:ntbl1rows)
  {
    #for n in 1: number of rows{
    # check the previous hour from IDA dataset !!!!
    # calculate sumDate - previousHour = newHourSum and store it as newHourSum
    # calculate hour/(newHourSum-previousHour) * Foreigners and store it as footfallHour
    # add to the empty dataframe }
    newHourSum <- 3588 - tbl1
    footfallHour <- (tbl1$hour/(newHourSum-previousHour)) * tbl2$Foreigners
    newtbl <- rbind(newtbl, footfallHour)  
  }
}
#read ida footfall dataset & footfall overall dataset
footfall_ori <- read.csv("idafootfall.csv", header=TRUE, stringsAsFactor=FALSE)
footfall_2014 <- read.csv("footfall.csv", header=TRUE, stringsAsFactor=FALSE)
previousHour <- read.csv("previousHour.csv", header=TRUE, stringsAsFactor=FALSE)
#take sum of footfall & sum of hours by category || sum of hours by date
sumFootfall <- aggregate(footfall_ori$footfall, by=list(Category=footfall_ori$category), FUN=sum)
sumHours <- aggregate(footfall_ori$hour, by=list(category=footfall_ori$category), FUN=sum)
sumDate <- aggregate(footfall_ori$hour, by=list(category=footfall_ori$day), FUN=sum)

#write to csv file
write.csv("sumFootfall.csv", x= sumFootfall)
write.csv("sumHours.csv", x= sumHours)

#merge new ida footfall with overall footfall dataset
sumFootfall <- read.csv("sumFootfall.csv", header=TRUE, stringsAsFactor=FALSE)
footfall_merged <- merge(x=sumFootfall, y=footfall_2014)
footfall_hours <- merge(x=footfall_merged, y=sumHours)

#rename column to hour
names(footfall_hours)[5] <- "hour"

#write to csv for the ida footfall merged dataset
write.csv("footfall_ida.csv", x=footfall_hours)

#matrix correlation of all pair-wise
pairs(footfall_hours[-1]) #without cat column

#run correlation of all pair-wise
footfall_cor <- cor(footfall_hours[-1], method="pearson")
write.csv(file="footfall_cor.csv", x=footfall_cor)

#run regression
footfall_result<-lm(footfall~Foreigners +Locals, data= footfall_hours)
summary(footfall_result)

########################################################new strategy ###########################

# based on variable values // only work cluster 
workcluster <- footfall_2014[ which(footfall_2014$category=='Work cluster'), ]

# based on variable values // only Shopping cluster
shopping <- footfall_2014[ which(footfall_2014$category=='Shopping cluster'), ]


# based on variable values // only work cluster 
airport <- footfall_2014[ which(footfall_2014$category=='Airport'), ]

# based on variable values // only Shopping cluster
residential <- footfall_2014[ which(footfall_2014$category=='Residential area'), ]

#footfall count according to hour
newHourSum <- hourSum - previousHour
footfallHour <- (tbl1$hour/(newHourSum-previousHour)) * tbl2$Foreigners[which(tbl2$category=='Work cluster')
newtbl <- rbind(newtbl, footfallHour)

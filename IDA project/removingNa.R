library(ggplot2)

strategic <- read.csv("strategic-information-sample.csv", header=TRUE, stringsAsFactor=FALSE)

#change to numeric for thousand digits
strategic$TTASSET <- as.numeric(gsub(",", "", strategic$TTASSET))
strategic$TTCRASSET <- as.numeric(gsub(",", "", strategic$TTCRASSET))
strategic$TTLIAB <- as.numeric(gsub(",", "", strategic$TTLIAB))
strategic$TTCRLIAB <- as.numeric(gsub(",", "", strategic$TTCRLIAB))
strategic$TTNCRLIAB <- as.numeric(gsub(",", "", strategic$TTNCRLIAB))
strategic$TTEQUITY <- as.numeric(gsub(",", "", strategic$TTEQUITY))
strategic$SHARECAP <- as.numeric(gsub(",", "", strategic$SHARECAP))
strategic$OTHERRESERVESTT <- as.numeric(gsub(",", "", strategic$OTHERRESERVESTT))
strategic$RETAINEARN <- as.numeric(gsub(",", "", strategic$RETAINEARN))
strategic$TTNCRASSET <- as.numeric(gsub(",", "", strategic$TTNCRASSET))
strategic$PUCAP <- as.numeric(gsub(",", "", strategic$PUCAP))
strategic$AR_CURRWCAP <- as.numeric(gsub(",", "", strategic$AR_CURRWCAP))
strategic$AR_PROPRATIO <- as.numeric(gsub(",", "", strategic$AR_PROPRATIO))
strategic$AR_TOTALDEBT <- as.numeric(gsub(",", "", strategic$AR_TOTALDEBT))

#removing NA's column
strategic<- subset(strategic, select = -c( 6 : 10 ))
strategic<- subset(strategic, select = -c( 17 : 20 ))
str(strategic) #structure
write.csv(file="strategic.csv", x=strategic,  na="0")

#matrix scatterplot of 19 by 19
pairs(strategic[-c(1:4)]) # without char attributes

#correlation of all pair-wise
strategic_cor <- cor(strategic[-c(1:4)], method="pearson")
write.csv(file="strategic_cor.csv", x=strategic_cor)

#Generate descriptive statistics 
library(psych)
desc_stat <- describe(strategic[-c(1:4)])
write.csv(file="desc_stat.csv", x=desc_stat)


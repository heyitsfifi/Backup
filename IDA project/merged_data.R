library(ggplot2)

attach(strategic_merged)
# merge data into one csv file

strategic <- read.csv("strategic-information-sample.csv", header=TRUE, stringsAsFactor=FALSE)
totalAsset <- read.csv("AnotherStrategic_merged.csv", header=TRUE, stringsAsFactor=FALSE)
qualification <- read.csv("High.csv", header=TRUE, stringsAsFactor=FALSE)
GDP <- read.csv("GDP_SG.csv", header=TRUE, stringsAsFactor=FALSE)
skilled <- read.csv("skilled_data.csv", header=TRUE, stringsAsFactor=FALSE)
strategic_merged_qualification <- merge(x=strategic, y=qualification)
strategic_merged_gdp <- merge(x=strategic_merged_qualification, y=GDP)
strategic_merged <- merge(x=strategic_merged_gdp, y=skilled)

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
write.csv(file="strategic.csv", x=strategic,  na="0")

strategic_merged <- read.csv(file="strategic_merged.csv", header=TRUE, stringsAsFactor=FALSE)
# analyse data

#what is the trend of university students over the years?
totalUni <- ggplot(strategic_merged, aes(FYEAR,SG_TOTAL_UNI_SURVEYED)) + geom_line() + ylab("Frequency") +ggtitle("No. Of University Students over 2009-2011")
totalUni

#what is the trend of diploma students over the years?
totalDip <- ggplot(strategic_merged, aes(FYEAR,SG_TOTAL_DIPLOMA_PROFESSIONAL_SURVEYED)) + geom_line() + ylab("Frequency") +ggtitle("No. Of Diploma Students over 2009-2011")
totalDip

#regression in total assets and total equity

results = lm( TTASSET ~ TTEQUITY  , data=strategic_merged)

summary(results)

#matrix scatterplot of employment rate, gdp, total liab, total assets
pairs(~EMPLOYMENT_RATE+TTLIAB+TTASSET+SG_GDP+RETAINEARN,data=strategic_merged, 
      main="Simple Scatterplot Matrix")

#removing NA's column
strategic<- subset(strategic, select = -c( 6 : 10 ))
strategic<- subset(strategic, select = -c( 17 : 20 ))
str(strategic) #structure
write.csv(file="strategic.csv", x=strategic,  na="0")

totalAsset <- subset(totalAsset, select=-2)
#matrix scatterplot of 19 by 19
pairs(strategic[-c(1:4)]) # without char attributes
pairs(totalAsset[-c(2:5)]) #merged with main industrial total asset data

#correlation of all pair-wise
strategic_cor <- cor(strategic[-c(1:4)], method="pearson")
write.csv(file="strategic_cor.csv", x=strategic_cor)

#Generate descriptive statistics 
library(psych)
desc_stat <- describe(strategic[-c(1:4)])
write.csv(file="desc_stat.csv", x=desc_stat)

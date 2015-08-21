#attach the data
attach(S_G_E)

#check names
names(S_G_E)

#################################################################
#Scatterplot of SG_GDP against TTASSET
plot(SG_GDP, TTASSET, main="GDP vs Total Asset")

#calculate pearson correlation between the variables
cor(SG_GDP, TTASSET, method="pearson")

#fit a linear regression using lm(linear model) command
#fit linear regression to the data and save it in an object
m1<-lm(TTASSET ~ SG_GDP)

#to see the summary
summary(m1)

#to add regression line
abline(m1, col=2, lwd=2)

#to produce diagnostics plot
plot(m1)

#displaying 4plots on a screen
par(mfrow=c(2,2))
plot(m1)
#################################################################

#################################################################
#Scatterplot of EMPLOYMENT_RATE against TTASSET
plot(EMPLOYMENT_RATE, TTASSET, main="Employment rate vs Total Asset")

#calculate pearson correlation between the variables
cor(EMPLOYMENT_RATE, TTASSET, method="pearson")

#fit a linear regression using lm(linear model) command
#fit linear regression to the data and save it in an object
m2<-lm(TTASSET ~ EMPLOYMENT_RATE)

#to see the summary
summary(m2)

#to add regression line
abline(m2, col=2, lwd=2)

#to produce diagnostics plot
plot(m2)

#displaying 4plots on a screen
par(mfrow=c(2,2))
plot(m2)
#################################################################

#################################################################
#Scatterplot of EMPLOYMENT_RATE against TTLIAB
plot(EMPLOYMENT_RATE, TTLIAB, main="Employment rate vs Total liabilities")

#calculate pearson correlation between the variables
cor(EMPLOYMENT_RATE, TTLIAB, method="pearson")

#fit a linear regression using lm(linear model) command
#fit linear regression to the data and save it in an object
m3<-lm(TTLIAB ~ EMPLOYMENT_RATE)

#to see the summary
summary(m3)

#to add regression line
abline(m3, col=2, lwd=2)

#to produce diagnostics plot
plot(m3)

#displaying 4plots on a screen
par(mfrow=c(2,2))
plot(m3)
#################################################################


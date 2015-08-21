attach(strategic_merged)
repeat{
  choice <- readline(prompt="Please select from following options: 
0: Merged data
1: Multiple Linear Regression
2: Reg btwn Emplymt vs Liab
3: Reg btwn Liab vs TTAssets
4: Corr:Employment rate & Liabilities
5: Corr:Employment rate & TTAssets
6: Corr:Employment rate & GDP")
  
 
  print(choice)
  
  if(choice==0){
   # print(summary(results))
    strategic <- read.csv("strategic-information-sample.csv", header=TRUE, stringsAsFactor=FALSE)
    employment <- read.csv("Employment_rate.csv", header=TRUE, stringsAsFactor=FALSE)
    GDP <- read.csv("GDP_SG.csv", header=TRUE, stringsAsFactor=FALSE)
    strategic_merged_employment <- merge(x=strategic, y=employment)
    strategic_merged <- merge(x=strategic_merged_employment, y=GDP)
    strategic_merged$TTASSET <- as.numeric(gsub(",", "", strategic_merged$TTASSET))
    strategic_merged$TTCRASSET <- as.numeric(gsub(",", "", strategic_merged$TTCRASSET))
    strategic_merged$TTLIAB <- as.numeric(gsub(",", "", strategic_merged$TTLIAB))
    strategic_merged$TTCRLIAB <- as.numeric(gsub(",", "", strategic_merged$TTCRLIAB))
    strategic_merged$TTNCRLIAB <- as.numeric(gsub(",", "", strategic_merged$TTNCRLIAB))
    strategic_merged$TTEQUITY <- as.numeric(gsub(",", "", strategic_merged$TTEQUITY))
    strategic_merged$SHARECAP <- as.numeric(gsub(",", "", strategic_merged$SHARECAP))
    strategic_merged$OTHERRESERVESTT <- as.numeric(gsub(",", "", strategic_merged$OTHERRESERVESTT))
    strategic_merged$RETAINEARN <- as.numeric(gsub(",", "", strategic_merged$RETAINEARN))
    strategic_merged$TTNCRASSET <- as.numeric(gsub(",", "", strategic_merged$TTNCRASSET))
    write.csv(file="strategic_merged.csv", x=strategic_merged,  na="0")
  }
  if(choice==1){
    
    result<-lm(EMPLOYMENT_RATE~TTLIAB+ TTASSET + SG_GDP, data= strategic_merged)
    print(summary(result))
  }
  if(choice==2){
    m1 <- lm(EMPLOYMENT_RATE~TTLIAB, data= strategic_merged)
    print(summary(m1))
  }
  if(choice==3){
    m2 <- lm(TTASSET~TTLIAB, data= strategic_merged)
    print(summary(m2))
  }
  if(choice==4){
    print(cor(EMPLOYMENT_RATE,TTLIAB, method="pearson"))
  }
  if(choice==5){
    print(cor(EMPLOYMENT_RATE,TTASSET, method="pearson"))
    
  }
  if(choice==6){
    print(cor(EMPLOYMENT_RATE, SG_GDP, method="pearson"))
  }
}

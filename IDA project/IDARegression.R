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
   # strategic-information-sample.csv is available in github
    strategic <- read.csv("strategic-information-sample.csv", header=TRUE, stringsAsFactor=FALSE)
    totalAsset <- read.csv("AnotherStrategic_merged.csv", header=TRUE, stringsAsFactor=FALSE)
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
    write.csv(file="merged_data.csv", x=totalAsset,  na="0")
  }
  if(choice==1){
    
    result<-lm(TTLIAB~TTEQUITY+ TTASSET + TTASSET_MAIN_SECTOR , data= totalAsset)
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

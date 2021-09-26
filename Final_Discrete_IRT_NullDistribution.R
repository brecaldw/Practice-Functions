install.packages("stringr")     # Install stringr R package
library("stringr")              # Load stringr R package
library(lavaan)
library(semTools)


# Clear the console in RStudio, clear the environment variables and set 
# the max.print option, and set the working directory, suppress warnings
cat("\014")  
rm()
options(max.print = 1000000)
setwd("C:/Users/brent/Desktop/TTU/PhD/Spring 2021/SEM2/Project1v3/ThousandFiles-0-0/")
options(warn=-1)

for (i in (1900:1901)){
  
  cat("******************************************************************",i)
  name <- toString(i)
  name <- paste(name,".csv")
  name = str_replace(name, " ", "")
  print(name)
  
  Data1 <- read.table(name, header=TRUE, sep=",", row.names=NULL)
  
  colnames(Data1) <- c("id", "group", "item1", "item2", "item3", "item4", "item5", "item6", "item7", "item8", "item9", "item10", 
                       "item11", "item12", "item13", "item14", "item15", "item16", "item17", "item18", "item19", "item20",
                       "item21", "item22", "item23", "item24", "item25", "item26", "item27", "item28", "item29", "item30",
                       "item31", "item32", "item33", "item34", "item35", "item36", "item37", "item38", "item39", "item40",
                       "blank")
 
  
  model1 <- '
  lv1 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10 + item11 + item12 + item13 + item14 + item15 + item16 + item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24 + item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32 + item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40
  lv2 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10 + item11 + item12 + item13 + item14 + item15 + item16 + item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24 + item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32 + item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40
  lv3 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10 + item11 + item12 + item13 + item14 + item15 + item16 + item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24 + item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32 + item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40
  lv4 =~ item1 + item2 + item3 + item4 + item5 + item6 + item7 + item8 + item9 + item10 + item11 + item12 + item13 + item14 + item15 + item16 + item17 + item18 + item19 + item20 + item21 + item22 + item23 + item24 + item25 + item26 + item27 + item28 + item29 + item30 + item31 + item32 + item33 + item34 + item35 + item36 + item37 + item38 + item39 + item40
  
  lv1 ~~ lv2 + lv3 + lv4 
  lv2 ~~ lv3 + lv4 
  lv3 ~~ lv4 
  
  
  '
  
  
  
  
  fit1 <- cfa(model = model1, data = Data1, group = "group", missing='FIML')
  
  #fit2 <- cfa(model = model1, data = Data1, group = "group", group.equal = "loadings", missing='FIML')
  #fit3 <- cfa(model = model1, data = Data1, group = "group", group.equal = c( "loadings", "intercepts"), missing='FIML') 
  #fit4 <- cfa(model = model1, data = Data1, group = "group", group.equal = c( "loadings", "intercepts", "means"),missing='FIML')
  
  
  fObj <- fitmeasures(fit1, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue"), output = "matrix")
  print(fObj)
  fit1_CFI <- fObj[4]
  fit1_RMSEA <- fObj[5]
  fit1_RMSEA_LOWER <- fObj[6]
  fit1_RMSEA_UPPER <- fObj[7]
  fit1_RMSEA_PVAL <- fObj[8]
  if(fit1_CFI > 0.90){
    CFI_acceptable <- "True"
  }
  else{
    CFI_acceptable <- "False"
  }
  
  stringToWriteToFile <- cat(fit1_CFI, fit1_RMSEA, fit1_RMSEA_LOWER, fit1_RMSEA_UPPER, fit1_RMSEA_PVAL, CFI_acceptable,)
  
  
  fObj <- fitmeasures(fit2, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue"))
  print(fObj)
  fit2_CFI <- fObj[4]
  fit2_RMSEA <- fObj[5]
  fit2_RMSEA_LOWER <- fObj[6]
  fit2_RMSEA_UPPER <- fObj[7]
  fit2_RMSEA_PVAL <- fObj[8]
  
  
  fObj <- fitmeasures(fit3, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue"))
  print(fObj)
  fit3_CFI <- fObj[4]
  fit3_RMSEA <- fObj[5]
  fit3_RMSEA_LOWER <- fObj[6]
  fit3_RMSEA_UPPER <- fObj[7]
  fit3_RMSEA_PVAL <- fObj[8]
  
  fObj <- fitmeasures(fit4, fit.measures = c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue"))
  print(fObj)
  fit4_CFI <- fObj[4]
  fit4_RMSEA <- fObj[5]
  fit4_RMSEA_LOWER <- fObj[6]
  fit4_RMSEA_UPPER <- fObj[7]
  fit4_RMSEA_PVAL <- fObj[8]
  
}





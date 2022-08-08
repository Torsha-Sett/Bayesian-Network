library(bnlearn)
library(bnviewer)
library(reshape2)
library(broom)
#library(XLConnect)
library(readxl)
library(tidyverse)
library(svDialogs)


#setting Directory Folder
setwd(choose.dir(default = "", caption = "Select Directory folder"))  #USER INPUT DIRECTORY

#calling required functions
df_prep=Data_preprocess()
blwllist=BlWl_Preprocess()
bl=data.frame(blwllist[1])
wl=data.frame(blwllist[2])

sat_df=df_prep


#Checking Blacklist/Whitelit for any mismatch of names
tryCatch({
  bl=unname(as.matrix(bl))
  notinbl=bl[bl %in% colnames(sat_df)==FALSE]
  wl=unname(as.matrix(wl))
  notinwl=wl[wl %in% colnames(sat_df)==FALSE]
  sat_df_bn<- hc(as.data.frame(sat_df))
}, error=function(e){
  message(paste("Mismatch in blacklist column names : \n",unique(notinbl),"\n"))
  message(paste("Mismatch in Whitelist column names : \n",unique(notinwl),"\n"))
  message("Here's the original error message:")
  message(e)
}
)

#plot will not be visible in viewer since you are storing in object
bn=viewer(sat_df_bn,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Bayesian Network for Overall Satisfaction",
       bayesianNetwork.subtitle = "Overall",
       # bayesianNetwork.footer = "Nodes and Arcs Practise",
       # edges.dashes = TRUE, edges.smooth = FALSE,
       
       node.colors = list(background = "#f4bafd",
                          border = "#2b7ce9",
                          highlight = list(background = "red",
                                           border = "black"))
       
)

file=data.frame(read_excel("Instructions.xlsx",sheet="Files"))
netpath=paste0(file[5,2],"/",file[5,3])
htmlwidgets::saveWidget(bn, netpath)


src_strength=arc.strength(sat_df_bn,sat_df)
#arc_strength=src_strength[src_strength$strength <= -10,]
arc_strength=src_strength
write.xlsx(as.data.frame(src_strength), file='C:/Users/ttorsha/OneDrive - Kantar/Documents/Bayesian Input/Output/strength.xlsx')

#Arcs_DF <- as.data.frame(arcs(sat_df_bn))
Arcs_DF=subset(arc_strength, select=-c(strength))
Arcs_DF <- Arcs_DF[order(Arcs_DF$to),]
models_DF <- melt(Arcs_DF, id.vars = "to")
models_DF$time <- ave(models_DF$value, models_DF$to, FUN = seq_along)
models_DF_final=dcast(models_DF, to ~ time, value.var = "value")


chk1=models_DF_final[-1]
data1 <- sapply(chk1, function(x) ifelse(x != "", paste0("'",x, "'"), ""))
data2=data.frame(models_DF_final[1],data1)
data3=data2 %>%
  mutate_if(is.factor, as.character)%>% 
  na_if('NA') %>% unite("z", 2:ncol(data2),sep=",",  remove = TRUE,na.rm = TRUE)



olsfunc <- function(yname,xname,df)
{
  df1=df[c(yname,xname)]
  colnames(df1)[1] <- "y"
  model= lm(y ~ ., data=df1)
  results=tidy(model)
  results$estimate=round(results$estimate,2)
  results$std.error=round(results$std.error,4)
  results$statistic=round(results$statistic,3)
  results$pval = ifelse(results$p.value<0.05,"<0.05",ifelse(results$p.value<0.1,"<0.1",round(results$p.value,3)))
  results=subset(results,select=-c(p.value))
  results[nrow(results)+1,] <- NA
  results[nrow(results)+1,"term"]<-"R-squared"
  results[nrow(results),"estimate"]<-summary(model)$r.squared
  return(results)
  
}


outpath=paste0(file[4,2],"/",file[4,3])

for (i in 1:nrow(data3))
{
  res=as.character(chk1[i,])
  res=res[complete.cases(res)]
  result=olsfunc(data3[i,"to"],res,sat_df)

  sheetname=strtrim(data3[i,"to"],31)
  sheetname <- gsub("/", "_", sheetname)
  write.xlsx(as.data.frame(result), file=outpath, sheetName=sheetname,row.names=FALSE,showNA=FALSE, append=TRUE)
  
}


#################################################################################
library(glmnet)

x = as.matrix(train_dummies)
y_train = train$unemploy

x_test = as.matrix(test_dummies)
y_test = test$unemploy

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)
library(caret)

ridgefunc <- function(yname,xname,df)
{
  x = as.matrix(df[c(xname)])
  y = as.matrix(df[yname])
  #lambdas <- 10^seq(2, -3, by = -.1)
  # Using cross validation glmnet
  #ridge_cv <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
  # Best lambda value
  #best_lambda <- ridge_cv$lambda.min
  # Rebuilding the model with optimal lambda value
  best_ridge <- glmnet(x, y, alpha = 0, lambda = 3.1)
  
  result = coef(best_ridge)
  return(result)
  
}

outpath="C:/Users/ttorsha/OneDrive - Kantar/Documents/Bayesian Input/Output/Ridge_Models_std_high_lambda3.xlsx"

for (i in 1:nrow(data3))
{
  res=as.character(chk1[i,])
  res=res[complete.cases(res)]
  if(length(res)>1){
    result=ridgefunc(data3[i,"to"],res,sat_df)
    res1=as.data.frame(as.matrix(result))
    sheetname=strtrim(data3[i,"to"],31)
    sheetname <- gsub("/", "_", sheetname)
    write.xlsx(res1, file=outpath, sheetName=sheetname, append=TRUE)
    
  }
    
}


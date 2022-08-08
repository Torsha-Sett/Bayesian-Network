library(readxl)
library(svDialogs)
library("stringr")
library(bnlearn)
library(bnviewer)
library(reshape2)
library(broom)
library(xlsx)
library(tidyverse)

Data_preprocess=function ()
{
  #read all file names and location from instruction
  files=data.frame(read_excel("Instructions.xlsx",sheet="Files"))
  
  #reading the input file for Bayesian analysis
  df=read_excel(paste0(files[1,2],"/",files[1,3]),sheet=files[1,4])
  
  
  columnstochoose <-data.frame(read_excel("Instructions.xlsx",sheet="filtering columns"))
  if(nrow(columnstochoose)!=0)
  {
    df=df[columnstochoose[,1]]
  }

  
  #Remove missing columns
  
  allmisscols <- apply(df,2, function(x)all(is.na(x)));  
  colswithallmiss <-names(allmisscols[allmisscols>0]);    
  cat(paste0("the columns with all values missing (and hence removed from analysis):","\n"))    
  cat(paste0("\t",colswithallmiss,"\n"))
  df1 <- df[,colSums(is.na(df))<nrow(df)]
  
  #dp="Power (Mean)"
  #df1=df1[!is.na(df1[dp]),]
  
  missperc <- dlgInput("\nWhat % of missing data do you want to allow for the variables?", Sys.info()["user"])$res
  k = as.numeric(gsub("[\\%,]", "", missperc))
  colpermiss <- apply(df1,2,function(x) mean(!is.na(x)) > (1-k/100))
  colpermissnames <-names(colpermiss[colpermiss==FALSE]);
  cat(paste0("\n","Columns with >",k,"% missing data (Henced removed from analysis):","\n"));    
  cat(paste0("\t",colpermissnames,"\n"))
  df1=df1[, which(colMeans(!is.na(df1)) > (1-k/100))]
  df1=as.data.frame(df1)
  
  
  keys=as.data.frame(read_excel("Instructions.xlsx",sheet="Transform Column values"))
  if(nrow(keys)!=0)
  {
    for (i in 1:nrow(keys)){
      columnsToTransForm <- keys[i,"Columns to transform"]
      currentValue <- keys[i, 'Current Value']
      replaceValue <- keys[i, 'Recoded Value']
      columnsVector <- unlist(strsplit(columnsToTransForm, ","))
      currentValuesVector <- unlist(strsplit(currentValue, ","))
      
      for (col in columnsVector) {
        col <- str_trim(col)
        df1[,col][df1[,col] %in% currentValuesVector] <- paste(replaceValue, "", sep = " ")
      }
    }
    df2 <- data.frame(t(apply(df1, 1,str_trim)))
    colnames(df2) <- colnames(df1)
  } else{
    df2=df1
  }
  
  
  columnstoupdate=data.frame(read_excel("Instructions.xlsx",sheet="Column names modify"))
  if(nrow(columnstoupdate)!=0)
  {
    columnstoupdate=columnstoupdate %>% 
      mutate_all(~ replace_na(.x, ""))
    
    for(i in 1:nrow(columnstoupdate))
    {
      colpermissnames <- gsub(columnstoupdate$Current[i], columnstoupdate$Replacement[i], colpermissnames )
    }
    
    colpermissnames <- gsub("  ", " ", colpermissnames )
    colpermissnames <- gsub(" / ", "/", colpermissnames )
    colpermissnames <- gsub(" - ", "-", colpermissnames )
    colpermissnames <- gsub("[()]", "", colpermissnames )
    colpermissnames=trimws(colpermissnames)
    
    for(i in 1:nrow(columnstoupdate))
    {
      colnames(df2) <- gsub(columnstoupdate$Current[i], columnstoupdate$Replacement[i], colnames(df2) )
    }
  }  
    colnames(df2) <- gsub("  ", " ", colnames(df2) )
    colnames(df2) <- gsub(" / ", "/", colnames(df2) )
    colnames(df2) <- gsub(" - ", "-", colnames(df2) )
    colnames(df2) <- gsub("[()]", "", colnames(df2) )
    colnames(df2) <- gsub("JR", "", colnames(df2) )
    colnames(df2)=trimws(colnames(df2))
    
  
  
  
  assign("miss",colpermissnames,envir = .GlobalEnv)
  
  
  cat(paste0("\n","Total observations in original dataset: ",nrow(df2)),"\n");
  df2=df2[complete.cases(df2), ]
  cat(paste0("\n","Total observations used in analysis (only complete cases will be used): ",nrow(df2)),"\n");
  
  
  
  coltype <- dlgInput("Are all your columns numeric/factor/both?", Sys.info()["user"])$res
  if (tolower(coltype)=="numeric") {
    df2[] <- lapply(df2, function(x) as.numeric(as.character(x)))
  } else if (tolower(coltype)=="factor"){
    df2[] <- lapply(df2, function(x) as.factor(as.character(x)))
  } else if (tolower(coltype)=="both") {
    print("will update this part shortly")
  } else {
    cat(paste0("\n","No proper input mentioned. Converting to numeric by default"))
    df2[] <- lapply(df2, function(x) as.numeric(as.character(x)))
  }
  
  colsrem=colnames(df2)
  assign("df_cols",colsrem,envir = .GlobalEnv)

  return(df2)
  #return(colpermissnames)
  #outputloc=choose.dir("", caption = "Choose output folder for processed file")
  #write.xlsx(df3,file=paste0(outputloc,"\\",ModelTitle,"_processed.xlsx"),row.names = FALSE)
  
  #print("Pre-processing Done. Processed file will be available at the selected output folder");
  #print(outputloc)
}




BlWl_Preprocess=function()
{
  #Blacklist and Whitelist preprocessing:
  BLWL=data.frame(read_excel("Instructions.xlsx",sheet="Files"))
  
  #Reading Blacklist
  blist=as.data.frame(read_excel(paste0(BLWL[2,"File_location"],"/",BLWL[2,"Filename"]),sheet=BLWL[2,"Sheetname"] ))
  wlist=as.data.frame(read_excel(paste0(BLWL[3,"File_location"],"/",BLWL[3,"Filename"]),sheet=BLWL[3,"Sheetname"] ))
  
  
  
  colnamemodify=function(df)
  {
    columnstoupdate=data.frame(read_excel("Instructions.xlsx",sheet="Column names modify"))
    columnstoupdate=columnstoupdate %>% 
      mutate_all(~ replace_na(.x, ""))
    
    if(nrow(columnstoupdate)!=0)
    {
      for(i in 1:nrow(columnstoupdate))
      {
        df<- data.frame(lapply(df, function(x) {gsub(columnstoupdate$Current[i], columnstoupdate$Replacement[i], x)}))                       
      }
    }
    
    
    df=data.frame(lapply(df, function(x) {gsub("  ", " ", x)}))
    df=data.frame(lapply(df, function(x) {gsub(" / ", "/", x)}))
    df=data.frame(lapply(df, function(x) {gsub(" - ", "-", x)}))
    df=data.frame(lapply(df, function(x) {gsub("[()]", "", x)}))
    df=data.frame(lapply(df, function(x) trimws(x)))
    
    if (length(miss)>0)
    {
      df=df %>% filter_all(all_vars(!grepl(paste(miss, collapse = "|"), .)))
    }
    to=as.vector(df$To)
    from=as.vector(df$From)
    vec=c(to,from)
    un=unique(vec)
    extra_cols=setdiff(un,as.vector(df_cols))
    if(length(extra_cols)>0)
    {
      
      df1=df %>% filter_all(all_vars(!grepl(paste(extra_cols, collapse = "|"), .)))
      
    }
 
    else{
      df1=df
    }
    return(df1)

    
    
   
  }
  bl=colnamemodify(blist)                     
  wl= colnamemodify(wlist)
  blwllist=list(bl,wl)
  return(blwllist)
  
}


                     
          
                      
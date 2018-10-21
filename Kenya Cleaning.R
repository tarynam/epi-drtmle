library(ggplot2)
library(dplyr)

setwd("/Applications/Old Computer/EPI/Data_clean")
total<-read.csv("total.csv")

df<-merge(TBRU_info, NK_info, by="StudyID", all=TRUE)
df1<-merge(total, df, by="StudyID", all=TRUE)


df1$HIV<-NA
for (i in 1:length(df1$HIV)){
    if(!is.na(df1$HIV.status[i])){
        df1$HIV[i] <- df1$HIV.status[i]}
    
    else if(!is.na(df1$HIVResult[i])){
        df1$HIV[i] <- df1$HIVResult[i]} 
    
    else{
        df1$HIV[i] <- df1$HIV.STATUS[i]} 
}

df1$cd4<-NA
for (i in 1:length(df1$cd4)){
    if(!is.na(df1$CD4[i])){
        df1$cd4[i] <- df1$CD4[i]}
    
    else{
        df1$cd4[i] <- df1$CD4.COUNT[i]} 
}


df1$viral.load<-NA
for (i in 1:length(df1$viral.load)){
    if(!is.na(df1$VL[i])){
        df1$viral.load[i] <- df1$VL[i]}
    
    else{
        df1$viral.load[i] <- df1$Viral.Load[i]} 
}

df1$viral.load<-NA
for (i in 1:length(df1$viral.load)){
    if(!is.na(df1$VL[i])){
        df1$viral.load[i] <- df1$VL[i]}
    
    else{
        df1$viral.load[i] <- df1$Viral.Load[i]} 
}



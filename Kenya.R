install.packages("ggplot2") 
install.packages("plyr")
install.packages("data.table")
install.packages("fmsb")

#set workspace
setwd("/Users/oldmac/Desktop/EPI")

#Clean Helminth Data Set
Hel <- read.csv("TBRU & NK Helminth Results 2016_DEC_19.csv")
Split = strsplit(as.character(Hel$StudyID),"-");
Hel$StudyID = as.factor(sapply(Split,"[",1));
Hel$Visit = as.factor(sapply(Split,"[",2));
Hel$Visit<-as.numeric(as.character(Hel$Visit))
Hel$Iteration = as.factor(sapply(Split,"[",4));
Hel$Iteration<-as.numeric(as.character(Hel$Iteration))
Hel<-data.table(Hel)
HEL<-Hel[!(Visit>0)]

#Import and Clean QFT Data Set
TB <- read.csv("TBRU & NK QFT Results_2016_DEC_19.csv")
Split = strsplit(as.character(TB$StudyID),"-");
TB$StudyID = as.factor(sapply(Split,"[",1));
TB$Visit = as.factor(sapply(Split,"[",2))
TB$Visit <- as.numeric(as.character(TB$Visit))
levels(TB$TBAntigen)[levels(TB$TBAntigen)==">10"] <- "10"
levels(TB$TBAntigen)[levels(TB$TBAntigen)==">77"] <- "NA"
levels(TB$Mitogen)[levels(TB$Mitogen)==">10"] <- "10"
TB$TBAntigen<-as.numeric(as.character(TB$TBAntigen))
TB$Mitogen<-as.numeric(as.character(TB$Mitogen))
TB$QFT=(TB$TBAntigen-TB$NIL)
TB$Control=(TB$Mitogen-TB$NIL)
TB<-data.table(TB)
TB<-TB[!(Visit>0)]
TB<-TB[!(QualResult==3)]
TB$QualResult[TB$QFT>0.35]<- 1
TB$QualResult [TB$QFT<0.35]<- 2
TB$QualResult[TB$QualResult==1] <- as.character("LTBI")
TB$QualResult[TB$QualResult==2]<-as.character("HEALTHY CONTROL")

#getting rid of duplicates
TB<-TB[!(TB$LabNo=="NKS0122")]
TB<-TB[!(TB$LabNo=="NKS0745")]
TB<-TB[!(TB$LabNo=="NKS0416")]
TB<-TB[!(TB$LabNo=="NKS1213")]
TB<-TB[!(TB$StudyID=="NK2464")]
TB<-TB[!(TB$StudyID=="NK2712")]

##Import HIV status for TBRU study
HIV <- read.csv("TBRU HIV TEST RESULTS_2017_JAN_11.csv")
HIV<-as.data.table(HIV)
HIV<-HIV[!(CurrVisit=="2")]
HIV$HIVResult[HIV$HIVResult==1]<-as.character("POS")
HIV$HIVResult[HIV$HIVResult==2]<-as.character("NEG")

#Import and Clean NKS Disease Status
Disease<-read.csv("NK Cell Study Enrollment Summary 2016_DEC_14.csv")
Disease$StudyID<-Disease$STUDY.ID

#Determining Helminth Results
S = aggregate(SchistosomaPositive ~ StudyID, data = HEL, function(x) {
  
  if (1 %in% x) {
    return(1);
  } else {
    return(0);
  }
});

H = aggregate(HelmithPositive ~ StudyID, data = HEL, function(x) {
  
  if (1 %in% x) {
    return(1);
  } else {
    return(0);
  }
});

A = aggregate(AscarisPositive ~ StudyID, data = HEL, function(x) {
  
  if (1 %in% x) {
    return(1);
  } else {
    return(0);
  }
});

T = aggregate(TricurisPositive ~ StudyID, data = HEL, function(x) {
  
  if (1 %in% x) {
    return(1);
  } else {
    return(0);
  }
});

h = aggregate(HookwormPositive ~ StudyID, data = HEL, function(x) {
  
  if (1 %in% x) {
    return(1);
  } else {
    return(0);
  }
});

pos<- merge(H,A,by="StudyID");
pos<- merge(pos,T, by="StudyID");
pos<- merge(pos,S, by="StudyID");
Hel.Results<- merge(pos,h, by="StudyID")
Hel.Results$Number<-(Hel.Results$AscarisPositive+Hel.Results$TricurisPositive+Hel.Results$SchistosomaPositive+Hel.Results$HookwormPositive)

#clean environment
remove(A,h,H,pos,S,T)
Hel<-merge(Hel.Results,HEL,by="StudyID")
Hel<-Hel[,c(1:7,22,23)]
Hel<-data.table(Hel)
Hel<-Hel[(SchistosomaPositive.y==1)]
Hel<-Hel[,c(1,9)]

#Everything in One Table
I<-merge(Disease,TB,by="StudyID",all=TRUE);
II<-merge(I,Hel.Results,by="StudyID",all=TRUE);
III<-merge(II,HIV,by="StudyID",all=TRUE);
Final<-data.table(III)

#Check for duplicated IDs
dups<-Final$StudyID[duplicated(Final$StudyID)]


## create new column with NA as value
Final[,"TB.Status"]<-NA
Final[,"HIV.Status"]<-NA

###update new column with value in one of two other columns
for (i in 1:length(Final$HIV.Status)){
  Final$HIV.Status[i] <- ifelse(is.na(Final$HIVResult[i]), as.character(Final$HIV.STATUS[i]),
                                as.character(Final$HIVResult[i]))
}

## update new column with value based on other column
for (i in 1:length(Final$TB.Status)){
  Final$TB.Status[i] <- ifelse(is.na(Final$QualResult[i]),as.character(Final$SCREENING.ARM[i]),
                               as.character(Final$QualResult[i]))
}

Final<-Final[!StudyID==""]
#CHECKKK
check<-Final[,c(1,3,45,56,10,73,82,83)]
Final$TB.Status[Final$StudyID=="NK2476"]<-"HEALTHY CONTROL"

#subset
LTBI<-subset(Final,Final$TB.Status=="LTBI")
ACTIVE<-subset(Final,Final$TB.Status=="ACTIVE")
HEALTHY<-subset(Final, Final$TB.Status=="HEALTHY CONTROL")

#clean
Final<-Final[HIV.Status=="NEG"]
Final<-Final[!is.na(TB.Status)]
Final<-Final[!is.na(HelmithPositive)]
Final<-Final[!Number>1]

#subset
LTBI<-subset(Final,Final$TB.Status=="LTBI")
ACTIVE<-subset(Final,Final$TB.Status=="ACTIVE")
HEALTHY<-subset(Final, Final$TB.Status=="HEALTHY CONTROL")

#Proportions
a=as.numeric(length(which(ACTIVE$SchistosomaPositive==1)))
b=as.numeric(length(which(ACTIVE$SchistosomaPositive==0)))
c=as.numeric(length(which(LTBI$SchistosomaPositive==1)))
d=as.numeric(length(which(LTBI$SchistosomaPositive==0)))
e=as.numeric(length(which(HEALTHY$SchistosomaPositive==1)))
f=as.numeric(length(which(HEALTHY$SchistosomaPositive==0)))
HC.SM=e/(e+f)  #126
LTBI.SM=c/(c+d)  #216
A.SM=a/(a+b)  #66

dat=c(a,c,e,b,d,f)
Matrix=matrix(dat,nrow=2,ncol=3,byrow=TRUE)
dat<-data.table(Matrix)

#plots
Dat<-merge(Hel,Final,by="StudyID")
Data<-summarySE(Dat, measurevar = "SchistosomaIntensity",groupvars = "TB.Status")
dodge <- position_dodge(width=0.5)  

ggplot(Dat, aes(x=log(SchistosomaIntensity), y=QFT, color=TB.Status)) +
  geom_point(shape=16, size=3, position=dodge)+
  geom_smooth(method=lm) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw()+
  theme(text = element_text(size = 30))+
  theme(legend.position="bottom")

ggplot(Dat, aes(x=Mitogen, y=QFT, color=TB.Status))+
  geom_point(shape=16, size=3, position=dodge)+
  geom_smooth(method=lm) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw()+
  theme(text = element_text(size = 30))+
  theme(legend.position="bottom")

ggplot(Dat, aes(x=log(SchistosomaIntensity), y=Mitogen, col=TB.Status)) +
  geom_point(shape=16, size=3, position=dodge)+
  geom_smooth(method=lm) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw()+
  theme(text = element_text(size = 30))+
  theme(legend.position="bottom")

ggplot(Dat, aes(x=TB.Status,y=SchistosomaIntensity))+
  geom_bar(shape=16, size=3, position=dodge)+
  geom_smooth(method=lm) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  theme_bw()+
  theme(text = element_text(size = 30))+
  theme(legend.position="bottom")

ggplot(Data, aes(x=TB.Status, y=SchistosomaIntensity)) + 
  geom_errorbar(aes(ymin=SchistosomaIntensity-se, ymax=SchistosomaIntensity+se), colour="black", width=.1, position=dodge) +
  geom_line(position=dodge) +
  geom_point(position=dodge, size=3) +
  xlab("Stimulation") +
  ylab("Frequency of Oregon Green Low CD4+ T cells") +
  ggtitle("Proliferation Restimulation Assay") +
  theme_bw()
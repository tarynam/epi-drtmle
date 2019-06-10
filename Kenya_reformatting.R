setwd("/Applications/Old Computer/Epi Project/Data_clean/")
kenya<-read.csv("complete-kenya-filledin.csv")
#Selecting and renaming variables
reporting<-dplyr::select(kenya, 
        StudyID, SCREENING.SITE, RACE, age, sex, MAL.TEST.RESULTS , PT.RESULT, HB,
        TB,
        HHC, chest.xray, gene.expert, hain, mgit, microscopy, 
        IPT, Drug.Resistant, INH.Resistant, Rif.Resistant,
        QFT, Control, 
        HelminthPositive, Number, AscarisPositive, HookwormPositive, TricurisPositive, 
        SchistosomaPositive, SchistosomaIntensity, IgG, OD, 
        HIV, cd4, viral.load)%>%
    dplyr::rename(site = SCREENING.SITE, malaria = MAL.TEST.RESULTS, pregnant = PT.RESULT, ControlQFT = Control,
                  hookworm = HookwormPositive, ascaris = AscarisPositive, helminth = HelminthPositive,
                  tricuris = TricurisPositive, SM = SchistosomaPositive, egg = SchistosomaIntensity)


reporting$QFT<-as.numeric(as.character(reporting$QFT))
reporting$cd4<-as.numeric(as.character(reporting$cd4))
reporting$pregnant[reporting$pregnant=="0"]<-as.character("NEG")
#all missing HIV values are TBRU or joint enrolled and therefore negative (double checked for joint enrolled)
reporting$HIV[is.na(reporting$HIV)]<-as.character("NEG")
reporting$viral.load[is.na(reporting$HIV)]<-0
reporting$viral.load<-as.numeric(as.character(reporting$viral.load))

analysis<-filter(reporting, !(is.na(TB)))%>%
    dplyr::select(StudyID, site, age, sex, pregnant, HB,
           HC, LTBI, ATB, QFT, ControlQFT, 
           malaria,  helminth, Number, ascaris, hookworm, tricuris, 
           SM, egg, IgG, 
           HIV, cd4, viral.load)

#Dummy Variables for factor variables (accounts for NAs inherently)
tb<-dummies::dummy(analysis$TB)
site<-dummies::dummy(analysis$site)
pregnant<-dummies::dummy(analysis$pregnant)
malaria<-dummies::dummy(analysis$malaria)
#dealing with NA for helminth columns ????

#NA for continuous variables
analysis$QFT.ind<-NA
for(i in 1:length(analysis$QFT)){
    if (is.na(analysis$QFT[i])){
        analysis$QFT.ind[i]<-0
    }
    else analysis$QFT.ind[i]<-1
}
analysis$QFT[is.na(analysis$QFT)]<-10

analysis$HB.ind<-NA
for(i in 1:length(analysis$HB)){
    if (is.na(analysis$HB[i])){
        analysis$HB.ind[i]<-0
    }
    else analysis$HB.ind[i]<-1
}
analysis$HB[is.na(analysis$HB)]<-median(analysis$HB, na.rm=TRUE)

analysis$ControlQFT.ind<-NA
for(i in 1:length(analysis$ControlQFT)){
    if (is.na(analysis$ControlQFT[i])){
        analysis$ControlQFT.ind[i]<-0
    }
    else analysis$ControlQFT.ind[i]<-1
}
analysis$ControlQFT[is.na(analysis$ControlQFT)]<-10



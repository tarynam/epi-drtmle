setwd("/Applications/Old Computer/Epi Project/Data_clean/")
kenya<-read.csv("complete-kenya-filledin.csv")
library(dplyr)
#Selecting and renaming variables
reporting<-dplyr::select(kenya, 
        StudyID, SCREENING.SITE, RACE, age, sex, MAL.TEST.RESULTS , PT.RESULT, HB,
        TB,?
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
reporting$HB<-as.numeric(as.character(reporting$HB))
reporting$pregnant[reporting$pregnant=="0"]<-as.character("NEG")
#all missing HIV values are TBRU or joint enrolled and therefore negative (double checked for joint enrolled)
reporting$HIV[is.na(reporting$HIV)]<-as.character("NEG")
reporting$viral.load[reporting$HIV=="NEG"]<-0
reporting$viral.load<-as.numeric(as.character(reporting$viral.load))
reporting$site[reporting$site=="No file"]<-NA


reporting<-filter(reporting, !(is.na(TB)) &TB!="INDETERMINATE" & !(is.na(SM)))
remove<-which(is.na(reporting$chest.xray) & is.na(reporting$gene.expert) 
      & is.na(reporting$QFT) & reporting$TB=="LTBI")
reporting<-reporting[-remove,]

analysis<-dplyr::select(reporting, 
                 SM, age, helminth, ascaris, hookworm, tricuris, Number, egg, #fine the way they are
                 QFT, ControlQFT, IgG, HB, cd4, viral.load) #continuous variables

#Need to be turned into 0/1 and reinserted 
sex<-gsub("M", "0", reporting$sex)
analysis$sex<-gsub("F", "1", sex) 
hiv<-gsub("NEG", "0", reporting$HIV)
analysis$hiv<-gsub("POS", "1", hiv)

#NA for continuous variables
analysis$QFT.ind<-NA
for(i in 1:length(analysis$QFT)){
    if (is.na(analysis$QFT[i])){
        analysis$QFT.ind[i]<-0
    }
    else analysis$QFT.ind[i]<-1
}
analysis$QFT[is.na(analysis$QFT)]<-median(subset(analysis$QFT, reporting$TB=="LTBI"), na.rm = TRUE)

analysis$ControlQFT.ind<-NA
for(i in 1:length(analysis$ControlQFT)){
    if (is.na(analysis$ControlQFT[i])){
        analysis$ControlQFT.ind[i]<-0
    }
    else analysis$ControlQFT.ind[i]<-1
}
analysis$ControlQFT[is.na(analysis$ControlQFT)]<-median(analysis$ControlQFT, na.rm = TRUE)

analysis$HB.ind<-NA
for(i in 1:length(analysis$HB)){
    if (is.na(analysis$HB[i])){
        analysis$HB.ind[i]<-0
    }
    else analysis$HB.ind[i]<-1
}
analysis$HB[is.na(analysis$HB)]<-median(analysis$HB, na.rm=TRUE)

analysis$cd4.ind<-NA
for(i in 1:length(analysis$cd4)){
    if (is.na(analysis$cd4[i])){
        analysis$cd4.ind[i]<-0
    }
    else analysis$cd4.ind[i]<-1
}
analysis$cd4[is.na(analysis$cd4) & analysis$hiv==0]<-800
analysis$cd4[is.na(analysis$cd4) & analysis$hiv==1]<-median(subset(analysis$cd4, reporting$HIV=="POS"), na.rm=TRUE)



analysis$vl.ind<-NA
for(i in 1:length(analysis$viral.load)){
    if (is.na(analysis$viral.load[i])){
        analysis$vl.ind[i]<-0
    }
    else analysis$vl.ind[i]<-1
}
analysis$viral.load[is.na(analysis$viral.load)]<-median(subset(analysis$viral.load, reporting$HIV=="POS"), na.rm=TRUE)

analysis$IgG.ind<-NA
for(i in 1:length(analysis$IgG)){
    if (is.na(analysis$IgG[i])){
        analysis$IgG.ind[i]<-0
    }
    else analysis$IgG.ind[i]<-1
}
analysis$IgG[is.na(analysis$IgG)]<-median(analysis$IgG, na.rm=TRUE)


#Dummy Variables for factor variables (accounts for NAs inherently)
tb<-dummies::dummy(reporting$TB)
site<-dummies::dummy(reporting$site)
pregnant<-dummies::dummy(reporting$pregnant)
malaria<-dummies::dummy(reporting$malaria)

test<-cbind(analysis, malaria, pregnant, site, tb)
test<-dplyr::rename(test, tb = TBACTIVE, hc = "TBHEALTHY CONTROL", ltbi = TBLTBI)%>%
    dplyr::select(tb, ltbi, hc, SM, everything())
test<-dplyr::select(test, -IgG, -IgG.ind, -cd4, -cd4.ind, -QFT, -QFT.ind, -helminth)
test<-dplyr::select(test, hc, ltbi, tb, SM, age, sex, 
    ascaris, hookworm, tricuris, Number, egg,
    hiv, viral.load, vl.ind,
    ControlQFT, ControlQFT.ind, 
    HB, HB.ind, malariaNEG, malariaPOS, pregnantNEG, pregnantPOS, siteJOOTRH, siteKOMBEWA)


write.csv(test, "Kenya_analysis.csv")
write.csv(reporting, "Kenya_reporting.csv")

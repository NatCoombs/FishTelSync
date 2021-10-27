# Creating a "core array" and creating a series of sets to 
# assess movement behaviors


library(RColorBrewer)
FWS07_200Dense<-SHK200FWSDense[which(SHK200FWSDense$FishID %in% na.omit(unique(USFWS200Rkm07$FishID))),]
FWS07_200Locs<-unique(FWS07_200Dense$Loc)
FWS07_200Locs<-as.data.frame(FWS07_200Locs)
FWS07_200Locs$Rkm<- NA
RkmTable[190,1]<-"BtlCkCNFHWeir"
RkmTable[190,2]<-527.392	
for(i in 1:length(FWS07_200Locs[,1])){
  FWS07_200Locs$Rkm[i] <- RkmTable$RiverKm[which(RkmTable$DetectionLocation %in% FWS07_200Locs[i,1])]
}

FWS07_200Locs<-FWS07_200Locs[order(FWS07_200Locs$Rkm, decreasing = FALSE),]

FWS07_200CoreDets<-matrix(data = 0,nrow = 109,ncol = 18)
colnames(FWS07_200CoreDets)<-USFWS200Rkm07$FishID
rownames(FWS07_200CoreDets)<-FWS07_200Locs[,1]
for(i in 1:length(FWS07_200Locs[,1])){
  for(j in 1:length(USFWS200Rkm07$FishID)){
    FishDense<-FWS07_200Dense[which(FWS07_200Dense$FishID %in% USFWS200Rkm07$FishID[j]),]
    FishMin<-min(FishDense$Rkm, na.rm = TRUE)
    if(FWS07_200Locs[i,1] %in% FishDense$Loc) {
      FWS07_200CoreDets[i,j]<-2
    } else {
    if(RkmTable[which(RkmTable$DetectionLocation %in% FWS07_200Locs[i,1]),"RiverKm"] >= FishMin) {FWS07_200CoreDets[i,j]<- -2}
    } 
    }
}

heatmap(FWS07_200CoreDets, Colv = NA, Rowv = NA, scale="column", col = brewer.pal(5,"RdBu"))

FWS07_200CoreDetsRkm<-matrix(data = 0,nrow = 109,ncol = 18)
colnames(FWS07_200CoreDetsRkm)<-USFWS200Rkm07$FishID
rownames(FWS07_200CoreDetsRkm)<-FWS07_200Locs[,2]
for(i in 1:length(FWS07_200Locs[,1])){
  for(j in 1:length(USFWS200Rkm07$FishID)){
    FishDense<-FWS07_200Dense[which(FWS07_200Dense$FishID %in% USFWS200Rkm07$FishID[j]),]
    FishMin<-min(FishDense$Rkm, na.rm = TRUE)
    if(FWS07_200Locs[i,1] %in% FishDense$Loc) {
      FWS07_200CoreDetsRkm[i,j]<-2
    } else {
      if(RkmTable[which(RkmTable$DetectionLocation %in% FWS07_200Locs[i,1]),"RiverKm"] >= FishMin) {FWS07_200CoreDetsRkm[i,j]<- -2}
    } 
  }
}

heatmap(FWS07_200CoreDetsRkm, Colv = NA, Rowv = NA, scale="column", col = brewer.pal(5,"RdBu"))


GenRkm07200<-unique(rownames(FWS07_200CoreDetsRkm))

FWS07_200CoreDetsAgg<-matrix(data = 0, nrow = 57, ncol = 18)
colnames(FWS07_200CoreDetsAgg)<-USFWS200Rkm07$FishID
rownames(FWS07_200CoreDetsAgg)<-GenRkm07200

for(i in 1:length(FWS07_200CoreDetsAgg[,1])){
  for(j in 1:length(FWS07_200CoreDetsAgg[1,])){

    
    if(any(FWS07_200CoreDetsRkm[which(rownames(FWS07_200CoreDetsRkm) %in% GenRkm07200[i]),j] > 0)){
      FWS07_200CoreDetsAgg[i,j] <- 1
    } else{ if(any(FWS07_200CoreDetsRkm[which(rownames(FWS07_200CoreDetsRkm) %in% GenRkm07200[i]),j] < 0)){
      FWS07_200CoreDetsAgg[i,j] <- -1}}
  }
}

heatmap(FWS07_200CoreDetsAgg, Colv = NA, Rowv = NA, scale="column", col = brewer.pal(5,"RdBu"))

PerfectSites<-vector(mode = "numeric")
k<-1
for(i in 1:length(FWS07_200CoreDetsAgg[,1])){
  if(!any(FWS07_200CoreDetsAgg[i,] %in% -1)){
  PerfectSites[k]<-as.numeric(GenRkm07200[i])
  k<-k+1}
}
  
DenseDetRkmAgg<-function(DenseTable,fcol,t1Col,t2Col,RkmCol){
  RkmAggTable<-data.frame()
  RkmAggOut<-data.frame("FishID" = NA, "Rkm" = NA, "FirstDet" = NA, "LastDet" = NA)
  jMax<-0
  FishID<-unique(DenseTable[,fcol])
  for(i in 1:length(FishID)){
    fDense<-na.omit(DenseTable[which(DenseTable[,fcol] %in% unique(DenseTable[,fcol])[i]),])
    OrdfDense<-fDense[order(fDense[,t1Col]),]
    
    FirstDetVec<-vector(mode = "list")
    for(j in 2:length(OrdfDense[,t1Col])){
      FirstDetVec[j] <- OrdfDense[j,RkmCol] ==  OrdfDense[j-1,RkmCol]
    }
    FirstDetInd<-vector(mode = "list")
    FirstDetInd<-c(1,which(FirstDetVec %in% FALSE))
    
    LastDetVec<-vector(mode = "list")
    for(j in 1:(length(OrdfDense[,t1Col])-1)){
      LastDetVec[j]<-OrdfDense[j,RkmCol] == OrdfDense[j+1,RkmCol]
    }
    LastDetInd<-vector(mode = "list")
    LastDetInd<-c(which(LastDetVec %in% FALSE),length(OrdfDense[,RkmCol]))
    
    FishFrame<-data.frame()
    FishFrame[1:length(FirstDetInd),"FishID"]<-FishID[i]
    FishFrame[1:length(FirstDetInd),"Rkm"]<-OrdfDense[FirstDetInd,RkmCol]
    FishFrame[1:length(FirstDetInd),"FirstDet"]<-OrdfDense[FirstDetInd,t1Col]
    FishFrame[1:length(FirstDetInd),"LastDet"]<-OrdfDense[LastDetInd,t2Col]
    
    RkmAggOut<-rbind(FishFrame,RkmAggOut)
  }
  return(RkmAggOut)
}
FWS07_200Steps<-DenseDetRkmAgg(DenseTable = FWS07_200Dense,fcol = "FishID", t1Col = "FirstDet",
               t2Col = "LastDet", RkmCol = "Rkm")

HoldsByRkm<-data.frame()

for(i in 1:length(unique(FWS07_200Steps$Rkm))){
  Rkmdex<-unique(FWS07_200Steps$Rkm)[i]
  RkmSet<-FWS07_200Steps[which(FWS07_200Steps$Rkm %in% Rkmdex),]
  for(j in 1:length(RkmSet[,1])){
    HoldsByRkm[j,i]<-24*60*(as.chron(RkmSet[j,"LastDet"])-as.chron(RkmSet[j,"FirstDet"]))
  }
  colnames(HoldsByRkm)[i]<-as.character(Rkmdex)
}

colnames(HoldsByRkm)<-GenRkm07200      #paste("Holds at", unique(FWS07_200Steps$Rkm), "Rkm", sep = " ")


PerfectHoldDex<-which(colnames(HoldsByRkm) %in% PerfectSites)

boxplot(x = HoldsByRkm[,c(1,2,3,)],xlab = "Time in minutes",
               ylab = "Rkm", horizontal = TRUE)
title(main = "Boxplot of Rkm vs holding time at so-called core detectors")

pdf(file = "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Results/Holding distributions.pdf")

boxplot(x = HoldsByRkm[,c(2,3,5,6,10,11,12,13,14,15,16,19,20,25,26,27,29,30)],xlab = "Time in minutes",
        ylab = "Rkm", horizontal = TRUE)
title(main = "Boxplot of Rkm vs holding time at so-called core detectors")

par(mfrow = c(3,1))
for(i in 1:length(PerfectHoldDex)){
  hist(x = HoldsByRkm[,PerfectHoldDex[i]],main = paste("Holds at Rkm", PerfectSites[19-i],sep =" "),
       xlab = "Hold time in minutes")
}
for(i in 1:length(PerfectHoldDex)){
  hist(x = HoldsByRkm[,PerfectHoldDex[i]],main = paste("Holds at Rkm", PerfectSites[19-i],sep =" "),
       xlab = "Hold time in minutes",xlim = c(0,100), breaks = seq(from = 0, to = 600000, by = 5))
}
par(mfrow = c(1,1))

dev.off()



RkmCoverage<-data.frame()
for(i in 1:length(GenRkm07200)){
  nFish<-length(which(FWS07_200CoreDetsAgg[i,] != 0))
  nMissed<-length(which(FWS07_200CoreDetsAgg[i,] < 0))
  RkmCoverage[i,1]<-GenRkm07200[i]
  RkmCoverage[i,2]<-(nFish-nMissed)/nFish
}
colnames(RkmCoverage)<-c("Rkm","Pecent Coverage")

plot.new()
abline(h=RkmCoverage$Rkm, lwd = RkmCoverage$`Pecent Coverage`*5)


barplot(height = RkmCoverage$`Pecent Coverage`, names.arg = RkmCoverage$Rkm,
        horiz = TRUE, xlab = "Percent Coverage",ylab = "Rkm",main = "Coverage by Rkm")



CoreSteps07200<-matrix(data = NA, ncol = 18)
colnames(CoreSteps07200)<-PerfectSites[c(1:18)]
#CoreSteps07200<-as.data.frame(CoreSteps07200)
# FWS07_200Steps[,c("FirstDet","LastDet")]<-as.chron(FWS07_200Steps[,c("FirstDet","LastDet")])
PerfectSites<-PerfectSites[order(PerfectSites,decreasing = TRUE)]
# for(i in 1:(length(PerfectSites)-1)){
#   RkmSet1<-FWS07_200Steps[which(FWS07_200Steps$Rkm %in% PerfectSites[i]),]
#   RkmSet2<-FWS07_200Steps[which(FWS07_200Steps$Rkm %in% PerfectSites[i+1]),]
#   for(j in 1:length(RkmSet2)){
#     FishRkm2<-RkmSet2[j,]
#     FishDex<-FishRkm2["FishID"]
#     FishRkms1<-RkmSet1[which(RkmSet1["FishID"] %in% FishDex),]
#     ArrOfInterest<-FishRkm2[1,"FirstDet"]
#     DepSet<-FishRkms1["LastDet"]
#     DepOffset<-vector(mode = "numeric")
#     DepDex<-vector(mode = "logical")
#     if(!is.na(DepSet)){
#     for(k in 1:length(DepSet)){
#       DepOffset[k]<-as.chron(ArrOfInterest)-as.chron(DepSet[k,1])
#       DepDex[k]<-DepOffSet[k] > 0
#     }
#     if(! is.na(DepOffset[1])){
#       if(any(DepDex)){
#         StepOfInterest<-min(DepOffSet[which(DepDex)],na.rm = TRUE)
#         CoreSteps07200[j,i]<-StepOfInterest
#       }
#     }}
#   }
# }
StepFish<-unique(FWS07_200Steps$FishID)
for(i in 1:length(StepFish)){
  FishSet<-FWS07_200Steps[which(FWS07_200Steps$FishID %in% StepFish[i]),]
  CoreStepsPerFish<-matrix(data = NA, ncol = 18)
  colnames(CoreStepsPerFish)<-PerfectSites[c(1:18)]
  for(j in 1:(length(which(PerfectSites %in% FishSet[,"Rkm"]))-1)){
    Rkm1Dex<-which(FishSet[,"Rkm"] %in% PerfectSites[j])
    Rkm2Dex<-which(FishSet[,"Rkm"] %in% PerfectSites[j+1])
    for(k in 1:length(Rkm2Dex)){
      StepTest<-vector(mode = "list")
      StepTest<- -Rkm1Dex+Rkm2Dex[k]
      StepToUse<- which(StepTest %in% max(StepTest[which(StepTest < 0)]))
      CoreStepsPerFish[k,j]<-as.chron(FishSet[Rkm2Dex[k],"FirstDet"]) - as.chron(FishSet[Rkm1Dex[StepToUse],"LastDet"]) 
    }
  }
  CoreSteps07200<-rbind(CoreSteps07200,CoreStepsPerFish)
}


plot(x = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), y = PerfectSites, pch = 13, col = "DarkGreen",
     main = "Site coverage along Rkm", ylab = "Rkm")

abline(h = as.numeric(RkmCoverage$Rkm), lwd = 2*RkmCoverage$`Pecent Coverage`, col = "Gray")

points(x = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), y = PerfectSites, pch = 13, col = "DarkGreen")



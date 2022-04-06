# Linearized SHK demo
library(chron)

FWSSHK<-SHK[which(substr(SHK$StudyID,1,5) %in% "USFWS"),]
FWSSHK<-FWSSHK[which(!is.na(FWSSHK$FishID)),]
FWS07Raw<-FWSSHK[which(substr(FWSSHK$Date_Released,1,4) %in% "2007"),]

GGBvec<-c("Golden Gate West Line","Golden Gate East Line")

GGBdex<-which(FWS07Raw$General_Location %in% GGBvec)

FWS07Raw[GGBdex,"General_Location"]<-"Golden Gate and Ocean"


FWS07BADLocDense<-DetCondense(DetFrame = FWS07Raw, FishID = unique(FWS07Raw$FishID), LocCol = "DetectionLocation",tCol = "DetectDate",
                              IDCol = "FishID")
FWS07LocDense<-DetCondense(DetFrame = FWS07Raw, FishID = unique(FWS07Raw$FishID), LocCol = "General_Location",tCol = "DetectDate",
                           IDCol = "FishID")



FWS07BADLocDense$Rkm <- vector(mode = "numeric", length = nrow(FWS07BADLocDense))


for(i in 1:nrow(FWS07BADLocDense)){
  OpRow <- FWS07BADLocDense[i,]
  FWS07BADLocDense$Rkm[i] <- RkmTable[which(RkmTable[,1] %in% OpRow$Loc),2]
}


FWS07Fall<-WaterSplit(FWS07BADLocDense, "FishID", "Rkm", "FirstDet", "LastDet")

FWDt<-t(as.data.frame(strsplit(FWS07Fall$Times, split = " ")))

FWS07Fall$ChronTimes<-chron(dates. = FWDt[,1], times. = FWDt[,2], format = c("y-m-d", "h:m:s"))


FishSet<-na.omit(unique(FWS07Fall$Fish))


wfp0<-function(WaterFrame,FishCol,tCol,RkmCol,FishIDs){
  Watersub<-WaterFrame[which(WaterFrame[,FishCol] %in% FishIDs),]
  plot(chron::as.chron(Watersub[,tCol]), y = Watersub[,RkmCol], xlab = "Date", ylab = "Rkm", main = FishIDs,
       type = "l")
}

pdf("Intermediate stuff/FWS07SteelFalls.pdf")

for(i in FishSet){
  wfp0(FWS07Fall, "Fish", "ChronTimes", "Rkm", i)
}

dev.off()

ReboundList<-c("STH0457", "STH0442", "STH0467", "STH0466", "STH0464","STH0460", "STH0453")
# Culling 7, leaves 16

FWS07LocDenseSub<-FWS07LocDense[which(!(FWS07LocDense$FishID %in% ReboundList)),]

SteelDemSub<-na.omit(unique(FWS07LocDenseSub$FishID))

SteelDemPairs<-data.frame(data = NA)
PairDex<-1

for(i in 1:(length(SteelDemSub)-1)){
  for(j in (i+1):length(SteelDemSub)){
    SteelDemPairs[PairDex,1] <- SteelDemSub[i]
    SteelDemPairs[PairDex,2] <- SteelDemSub[j]
    PairDex <- PairDex + 1
  }  
}

#which(is.na(FWS07Pairs[,1]))

SteelDemPairs<-SteelDemPairs[which(!is.na(SteelDemPairs[,2])),]

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

ReducedSteelDem<-list()

FWS07LocDenseSub$FirstDet<-as.numeric(as.POSIXct(FWS07LocDenseSub$FirstDet))
FWS07LocDenseSub$LastDet<-as.numeric(as.POSIXct(FWS07LocDenseSub$LastDet))



Printdex <- 1

Apples<-LCSCalc(FWS07LocDenseSub,SteelDemPairs)

for(i in 1:length(SteelDemPairs[,1])){
  cat("The fish pair: ", SteelDemPairs[i,1]," ", SteelDemPairs[i,2], "\n")
  
  cat("The LCS info: \n")
  ReducedSteelDem[[i]]<-LCSExtract(FWS07LocDenseSub[which(FWS07LocDenseSub$FishID %in% SteelDemPairs[i,1]),"Loc"],
                                FWS07LocDenseSub[which(FWS07LocDenseSub$FishID %in% SteelDemPairs[i,2]),"Loc"],
                                as.numeric(FWS07LocDenseSub[which(FWS07LocDenseSub$FishID %in% SteelDemPairs[i,1]),"FirstDet"]),
                                as.numeric(FWS07LocDenseSub[which(FWS07LocDenseSub$FishID %in% SteelDemPairs[i,1]),"LastDet"]),
                                as.numeric(FWS07LocDenseSub[which(FWS07LocDenseSub$FishID %in% SteelDemPairs[i,2]),"FirstDet"]),
                                as.numeric(FWS07LocDenseSub[which(FWS07LocDenseSub$FishID %in% SteelDemPairs[i,2]),"LastDet"]),
                                c("All"))
  cat("The completed run #: ", Printdex, "\n")
  Printdex <- Printdex+1
}

library(wsyn)
PopTest<-MFPop(ReducedSteelDem, SteelDemPairs, SteelDemSurrs[[1]], 100000)

PopTest[[1]]
PopTest[[2]]
par(mfrow = c(3,3))
pdf("Intermediate stuff/SteelDemoSurrHists.pdf")

par(mfrow = c(3,3))
hist(PopTest[[3]][[1]][,1], breaks = 20, main = "Surrogate Euclid Mean")
abline(v = PopTest[[1]][1,1], col = "Red", lwd = 2)
hist(PopTest[[3]][[1]][,2], breaks = 20, main = "Surrogate Phase Mean")
abline(v = PopTest[[1]][1,2], col = "Red", lwd = 2)
hist(PopTest[[3]][[1]][,3], breaks = 20, main = "Surrogate Stepwise Mean")
abline(v = PopTest[[1]][1,3], col = "Red", lwd = 2)
hist(PopTest[[3]][[3]][,1], breaks = 20, main = "Surrogate Euclid 90th quantile")
abline(v = PopTest[[1]][3,1], col = "Red", lwd = 2)
hist(PopTest[[3]][[3]][,2], breaks = 20, main = "Surrogate Phase 90th quantile")
abline(v = PopTest[[1]][3,2], col = "Red", lwd = 2)
hist(PopTest[[3]][[3]][,3], breaks = 20, main = "Surrogate Stepwise 90th quantile")
abline(v = PopTest[[1]][3,3], col = "Red", lwd = 2)
hist(PopTest[[3]][[4]][,1], breaks = 20, main = "Surrogate Euclid 10th quantile")
abline(v = PopTest[[1]][4,1], col = "Red", lwd = 2)
hist(PopTest[[3]][[4]][,2], breaks = 20, main = "Surrogate Phase 10th quantile")
abline(v = PopTest[[1]][4,2], col = "Red", lwd = 2)
hist(PopTest[[3]][[4]][,3], breaks = 20, main = "Surrogate Stepwise 10th quantile")
abline(v = PopTest[[1]][4,3], col = "Red", lwd = 2)

dev.off()
par(mfrow=c(1,1))

length(which(PopTest[[3]][[4]][,2] < PopTest[[1]][4,2]))
min(PopTest[[3]][[4]][,2])




SteelDemLDMat<-matrix(nrow = 16, ncol = 16)
rownames(SteelDemLDMat) <- SteelDemSub
colnames(SteelDemLDMat) <- SteelDemSub
SteelDemLSMat <- SteelDemLDMat
SteelDemPSMat <- SteelDemLDMat
for(i in 1:length(SteelDemPairs[,1])){

  LDk1 <- match(SteelDemPairs[i,1],SteelDemSub)
  LDk2 <- match(SteelDemPairs[i,2],SteelDemSub)
  
  SteelDemLDMat[LDk1,LDk2] <- ( (as.numeric(ReducedSteelDem[[i]][[7]])))
  SteelDemPSMat[LDk1,LDk2] <- ( (as.numeric(ReducedSteelDem[[i]][[14]])))
  SteelDemLSMat[LDk1,LDk2] <- ( (as.numeric(ReducedSteelDem[[i]][[21]])))
  
  SteelDemLDMat[LDk2,LDk1] <- ( (as.numeric(ReducedSteelDem[[i]][[7]])))
  SteelDemPSMat[LDk2,LDk1] <- ( (as.numeric(ReducedSteelDem[[i]][[14]])))
  SteelDemLSMat[LDk2,LDk1] <- ( (as.numeric(ReducedSteelDem[[i]][[21]])))
}
hist(log(SteelDemLDMat), main = "Log Euclid", breaks = 20)
hist(log(SteelDemPSMat), main = "Log Phase", breaks = 20)
hist(log(SteelDemLSMat), main = "Log Stepwise", breaks = 20)
plot(x = log(SteelDemLDMat), y = log(SteelDemPSMat), main = "Phase vs Euclid")
plot(x = log(SteelDemLDMat), y = log(SteelDemLSMat), main = "Stepwise vs Euclid")
plot(x = log(SteelDemPSMat), y = log(SteelDemLSMat), main = "Stepwise vs Phase")
for(i in 1:nrow(SteelDemLDMat)){
  SteelDemLDMat[i,i] <- 0
  SteelDemPSMat[i,i] <- 0
  SteelDemLSMat[i,i] <- 0
}
library(wsyn)

LDClust<-cluseigen(SteelDemLDMat)
PSClust<-cluseigen(SteelDemPSMat)
LSClust<-cluseigen(SteelDemLSMat)

modularity(SteelDemLDMat, LDClust[[length(LDClust)]], decomp = F)
modularity(SteelDemPSMat, PSClust[[length(PSClust)]], decomp = F)
modularity(SteelDemLSMat, LSClust[[length(LSClust)]], decomp = F)




SteelDemMTable<-MarkovFishTableSteel(FWS07LocDenseSub,"FishID", "Loc", "FirstDet","LastDet", "Golden Gate and Ocean")

SteelDemSurrs<-MarkovFishSurrogatesSteel(SteelDemMTable, InitPos = "Battle_Ck", 
                                      InitTime = 0, nFish = 1000, Reps = 2, "Golden Gate and Ocean")

SteelDemSigTest<-MFSig(na.omit(SteelDemPairs), FWS07LocDenseSub, ReducedSteelDem, SteelDemSurrs[[1]], SteelDemSurrs[[2]], Reps = 100000)

SigDemLDMat<-matrix(nrow = 16, ncol = 16)
rownames(SigDemLDMat) <- SteelDemSub
colnames(SigDemLDMat) <- SteelDemSub
SigDemLSMat <- SigDemLDMat
SigDemPSMat <- SigDemLDMat
for(i in 1:length(SteelDemPairs[,1])){
  
  LDk1 <- match(SteelDemPairs[i,1],SteelDemSub)
  LDk2 <- match(SteelDemPairs[i,2],SteelDemSub)
  
  SigDemLDMat[LDk1,LDk2] <- 1 - (SteelDemSigTest[i,3] / 1000)
  SigDemPSMat[LDk1,LDk2] <- 1 - (SteelDemSigTest[i,5] / 1000)
  SigDemLSMat[LDk1,LDk2] <- 1 - (SteelDemSigTest[i,7] / 1000)
  
  SigDemLDMat[LDk2,LDk1] <- SigDemLDMat[LDk1,LDk2]
  SigDemPSMat[LDk2,LDk1] <- SigDemPSMat[LDk1,LDk2]
  SigDemLSMat[LDk2,LDk1] <- SigDemLSMat[LDk1,LDk2]
}
hist(SigDemLDMat)
hist(SigDemLSMat)
hist(SigDemPSMat)
for(i in 1:nrow(SigDemLDMat)){
  SigDemLDMat[i,i] <- 0
  SigDemPSMat[i,i] <- 0
  SigDemLSMat[i,i] <- 0
}

image(log(SteelDemLDMat))
contour(SigDemLDMat, levels = c(.05, .95), col = c("Green", "Blue"), lwd = 5, add = TRUE)
title(main = "Euclidean Distance")
image(log(SteelDemPSMat))
contour(SigDemPSMat, levels = c(.05, .95), col = c("Green", "Blue"), lwd = 5, add = TRUE)
title(main = "Phase Offset")
image(log(SteelDemLSMat))
contour(SigDemLSMat, levels = c(.05, .95), col = c("Green", "Blue"), lwd = 5, add = TRUE)
title(main = "Stepwise")



for(i in 1:length(ReducedSteelDem)){
  cat("quack ")
}

SDLD<-cluseigen(SigDemLDMat)
SDPS<-cluseigen(SigDemPSMat)
SDLS<-cluseigen(SigDemLSMat)


modularity(SigDemLDMat, SDLS[[length(SDLS)]], decomp = T)





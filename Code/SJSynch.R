
DetCondenseSurrog <- function(DetFrame){
  # if(any(is.na(as.numeric(DetFrame[,tCol])))){
  #   stop("Error in DetCondense: Times must be numeric")
  # }
  tCol <- "FirstDet"
  tCol2 <- "LastDet"
  LocCol <- "Loc"
  FishID <- na.omit(unique(DetFrame[,"FishID"]))
  OutFrame<-data.frame("FishID" = NA, "Loc" = NA, "FirstDet" = NA, "LastDet" = NA, "NumDets" = NA)
  for(i in 1:length(FishID)){
    #cat(paste0(i, " fish started, "))
    FishFrame <- DetFrame[which(DetFrame[,"FishID"] == FishID[i]),]
    OrdFishFrame<-FishFrame[order(FishFrame["FirstDet"]),]
    
    if(length(unique(OrdFishFrame[,LocCol])) > 1){
      
      if(dim(OrdFishFrame)[1] > 2){
        FirstDetVec<-vector(mode = "list")
        for(j in 2:length(OrdFishFrame[,tCol])){
          FirstDetVec[j] <- OrdFishFrame[j,LocCol] == OrdFishFrame[j-1,LocCol]
        }
        FirstDetInd<-vector(mode = "list")
        FirstDetInd<-c(1,which(FirstDetVec %in% FALSE))
        #cat("FirstDetInd processed, ")
        
        LastDetVec<-vector(mode = "list")
        for(j in 1:(length(OrdFishFrame[,tCol])-1)){
          LastDetVec[j]<-OrdFishFrame[j,LocCol] == OrdFishFrame[j+1,LocCol]
        }
        LastDetInd<-vector(mode = "list")
        LastDetInd<-c(which(LastDetVec %in% FALSE),length(OrdFishFrame[,tCol2]))
        #cat("LastDetInd processed, ")
        NumDets<-matrix()
        for(j in 1:length(FirstDetInd)){
          NumDets[j]<-length(FirstDetInd[j]:LastDetInd[j])
        }}
      else{
        #print(OrdFishFrame)
        #cat("writing dets as is, ")
        
        FirstDetInd<-vector(mode = "numeric", length = 2)
        FirstDetInd[1]<-1
        #cat("writing dets as is, ")
        LastDetInd<-vector(mode = "numeric", length = 2)
        LastDetInd[1]<-1
        #cat("writing dets as is, ")
        NumDets<-vector(mode = "numeric")
        NumDets[1]<-1
        NumDets[2]<-1
        #cat("writing dets as is, ")
        #print("NEW FISH")
        FirstDetInd[2]<-2
        #print(FirstDetInd)

        #print(FirstDetInd)
        #cat("writing dets as is, ")
        LastDetInd[2]<-2
        #print(LastDetInd)

        #print(LastDetInd)
        #cat("writing dets as is, ")
        
      }} else{
        FirstDetInd<-vector(mode = "numeric", length = 1)
        FirstDetInd[1]<-1
        #cat("writing dets as is, ")
        LastDetInd<-vector(mode = "numeric", length = 1)
        LastDetInd[1]<-1
        #cat("writing dets as is, ")
        NumDets<-vector(mode = "numeric")
        NumDets[1]<-1
      }
      
      #cat("Starting FishFrame construction, ")
      FishFrame<-data.frame()
      FishFrame[1:length(FirstDetInd),"FishID"]<-FishID[i]
      #cat("FishID in, ")
      FishFrame[1:length(FirstDetInd),"Loc"]<-OrdFishFrame[FirstDetInd,LocCol]
      #cat("Loc in, ")
      FishFrame[1:length(FirstDetInd),"FirstDet"]<-OrdFishFrame[FirstDetInd,tCol]
      #cat("FD in, ")
      FishFrame[1:length(FirstDetInd),"LastDet"]<-OrdFishFrame[LastDetInd,tCol2]
      #cat("LD in, ")
      FishFrame[1:length(FirstDetInd),"NumDets"]<-NumDets
      
      #cat(" FishFrame constructed.\n")
      OutFrame<-rbind(FishFrame,OutFrame)
    
  }
  return(na.omit(OutFrame))
}


# Script for testing synchrony of the San Joaquin "surrogacy" questions






SJChin<-read.csv("Data/ChinookEventsFormatted/ALL_South_Delta_fall_Chinook_events_formatted_20220407.csv")
SJSteel<-read.csv("Data/SteelheadEventsFormatted/ALL_South_Delta_fall_Steelhead_events_formatted_20220415.csv")

SJChinFish <- cbind(SJChin$Release_date_time, SJChin$Tag_code)
table(SJChinFish[,1])

SJSteelFish <- cbind(SJSteel$Release_date_time, SJSteel$Tag_ID)
table(SJSteelFish[,1])

SJCRels<-na.omit(unique(SJChinFish[,1]))
SJSRels<-na.omit(unique(SJSteelFish[,1]))

SJCFish<-vector(mode = "list", length = length(SJCRels))
names(SJCFish) <- SJCRels
for(i in 1:length(SJCRels)){
  KeySet <- which(SJChinFish[,1] == SJCRels[i])
  print(KeySet)
  SJCFish[[i]] <- unique(SJChinFish[KeySet,2])
}


SJSFish<-vector(mode = "list", length = length(SJSRels))
names(SJSFish) <- SJSRels
for(i in 1:length(SJSRels)){
  KeySet <- which(SJSteelFish[,1] == SJSRels[i])
  SJSFish[[i]] <- unique(SJSteelFish[KeySet,2])
}

TAGArrayList <- c("A0","A2","A4","A5","B0","B1","B2","B4","C2","G1","A6","A7","A8","A11","A12","Rel")


SJCDets<-vector(mode = "list", length = length(SJCRels))
names(SJCDets) <- SJCRels
for(i in 1:length(SJCRels)){

  SubFrame <- SJChin[which(SJChin$Release_date_time == SJCRels[i]), c(2,23,10,11,12)]
  colnames(SubFrame) <- c("FishID","Loc","FirstDet","LastDet","NumDets")
  for(j in 1:length(SJCFish[[i]])){
    SubFrame <- rbind(SubFrame,c(SJCFish[[i]][j], "Rel", SJCRels[i], SJCRels[i], 1))
  }
  #print(SubFrame$FirstDet)
  SubFrame$FirstDet <- as.numeric(as.POSIXct(paste(substr(SubFrame$FirstDet, 1, 10)," ", substr(SubFrame$FirstDet, 12, 19)), format = "%Y-%m-%d %H:%M:%OS"))
  SubFrame$LastDet <-  as.numeric(as.POSIXct(paste(substr(SubFrame$LastDet, 1, 10)," ", substr(SubFrame$LastDet, 12, 19)), format = "%Y-%m-%d %H:%M:%OS"))
  SubFrame <- SubFrame[which(SubFrame$Loc %in% TAGArrayList),]
  #print(SubFrame$FirstDet)
  SubFrame <- DetCondenseSurrog(SubFrame)
  SJCDets[[i]] <- SubFrame
  
}

SJSDets<-vector(mode = "list", length = length(SJSRels))
names(SJSDets) <- SJSRels
for(i in 1:length(SJSRels)){
  
  SubFrame <- SJSteel[which(SJSteel$Release_date_time == SJSRels[i]), c(2,23,10,11,12)]
  
  colnames(SubFrame) <- c("FishID","Loc","FirstDet","LastDet","NumDets")
  #print(SubFrame$FishID)
  for(j in 1:length(SJSFish[[i]])){
    SubFrame <- rbind(SubFrame,c(SJSFish[[i]][j], "Rel", SJSRels[i], SJSRels[i], 1))
  }
  #print(SubFrame$FishID)
  #print(SubFrame$FirstDet)
  if(i < 313){
    psxformat <- "%Y-%m-%d %H:%M:%OS" 
  } else {
    SubFrame$FirstDet <- gsub("15 ", "2015 ", SubFrame$FirstDet)
    SubFrame$LastDet <- gsub("15 ", "2015 ", SubFrame$LastDet)
    psxformat <- "%m/%d/%Y %H:%M" 
  }
  SubFrame$FirstDet <- as.numeric(as.POSIXct(SubFrame$FirstDet, format = psxformat))
  SubFrame$LastDet <-  as.numeric(as.POSIXct(SubFrame$LastDet, format = psxformat))
  SubFrame <- SubFrame[which(SubFrame$Loc %in% TAGArrayList),]
  #print(SubFrame$FishID)
  #print(SubFrame$FirstDet)
  SubFrame <- DetCondenseSurrog(SubFrame)
  SJSDets[[i]] <- SubFrame[which(SubFrame$Loc %in% TAGArrayList),]
  
}

print(SJSDets[[312]])
print(SJSDets[[313]])
print(SJSDets[[314]])






SJCPairs<-vector(mode = "list", length = length(SJCRels))
names(SJCPairs) <- SJCRels
for(i in 1:length(SJCRels)){
  FishSet <- SJCFish[[i]]
  Pairdex <- 1
  NRows <- choose(length(FishSet),2)
  Pairs <- matrix(data = NA, nrow = NRows, ncol = 2)
  for(j in 1:(length(FishSet)-1)){
    for(k in (j+1):length(FishSet)){
      Pairs[Pairdex,1] <- FishSet[j]
      Pairs[Pairdex,2] <- FishSet[k]
      Pairdex <- Pairdex + 1
    }
  }
  SJCPairs[[i]] <- Pairs
}

SJSPairs<-vector(mode = "list", length = length(SJSRels))
names(SJSPairs) <- SJSRels
for(i in 1:length(SJSRels)){
  FishSet <- SJSFish[[i]]
  Pairdex <- 1
  NRows <- choose(length(FishSet),2)
  Pairs <- matrix(data = NA, nrow = NRows, ncol = 2)
  for(j in 1:(length(FishSet)-1)){
    for(k in (j+1):length(FishSet)){
      Pairs[Pairdex,1] <- FishSet[j]
      Pairs[Pairdex,2] <- FishSet[k]
      Pairdex <- Pairdex + 1
    }
  }
  SJSPairs[[i]] <- Pairs
}





saveRDS(SJCPairs,"Data/SJCPairs.rds")
saveRDS(SJSPairs,"Data/SJSPairs.rds")

SJCDebug<-function(Release, Run){
  print(SJCPairs[[Release]][Run,1])
  print(SJCPairs[[Release]][Run,2])
  cat("std::vector<std::string> Fish1Seq_ {", paste(shQuote(SJCDets[[Release]][which(SJCDets[[Release]]$FishID %in% SJCPairs[[Release]][Run,1]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ1")
  cat("std::vector<std::string> Fish2Seq_ {", paste(shQuote(SJCDets[[Release]][which(SJCDets[[Release]]$FishID %in% SJCPairs[[Release]][Run,2]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ2")
  cat("std::vector<double> TSeqArr1 {", paste(as.numeric(SJCDets[[Release]][which(SJCDets[[Release]]$FishID %in% SJCPairs[[Release]][Run,1]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ1")
  cat("std::vector<double> TSeqDep1 {", paste(as.numeric(SJCDets[[Release]][which(SJCDets[[Release]]$FishID %in% SJCPairs[[Release]][Run,1]),"LastDet"]), 
                                              collapse =", "), "};\n")
  #print("END DEPSEQ1")
  cat("std::vector<double> TSeqArr2 {", paste(as.numeric(SJCDets[[Release]][which(SJCDets[[Release]]$FishID %in% SJCPairs[[Release]][Run,2]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ2")
  cat("std::vector<double> TSeqDep2 {", paste(as.numeric(SJCDets[[Release]][which(SJCDets[[Release]]$FishID %in% SJCPairs[[Release]][Run,2]),"LastDet"]), 
                                              collapse =", "), "};\n")
}

SJSDebug<-function(Release, Run){
  print(SJSPairs[[Release]][Run,1])
  print(SJSPairs[[Release]][Run,2])
  cat("std::vector<std::string> Fish1Seq_ {", paste(shQuote(SJSDets[[Release]][which(SJSDets[[Release]]$FishID %in% SJSPairs[[Release]][Run,1]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ1")
  cat("std::vector<std::string> Fish2Seq_ {", paste(shQuote(SJSDets[[Release]][which(SJSDets[[Release]]$FishID %in% SJSPairs[[Release]][Run,2]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ2")
  cat("std::vector<double> TSeqArr1 {", paste(as.numeric(SJSDets[[Release]][which(SJSDets[[Release]]$FishID %in% SJSPairs[[Release]][Run,1]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ1")
  cat("std::vector<double> TSeqDep1 {", paste(as.numeric(SJSDets[[Release]][which(SJSDets[[Release]]$FishID %in% SJSPairs[[Release]][Run,1]),"LastDet"]), 
                                              collapse =", "), "};\n")
  #print("END DEPSEQ1")
  cat("std::vector<double> TSeqArr2 {", paste(as.numeric(SJSDets[[Release]][which(SJSDets[[Release]]$FishID %in% SJSPairs[[Release]][Run,2]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ2")
  cat("std::vector<double> TSeqDep2 {", paste(as.numeric(SJSDets[[Release]][which(SJSDets[[Release]]$FishID %in% SJSPairs[[Release]][Run,2]),"LastDet"]), 
                                              collapse =", "), "};\n")
}

library(Rcpp)

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")
source("Code/Functions/LCSCalc.R")
SJCSynch<-vector(mode = "list", length = length(SJCRels))

SJSSynch<-vector(mode = "list", length = length(SJSRels))

# "A180-1801-29822"
SJSDebug(313,426)

for(i in 1:length(SJCRels)){
  cat("Run number: ", i, "")
  SJCSynch[[i]] <- LCSCalc(SJCDets[[i]],SJCPairs[[i]])
}
saveRDS(SJCSynch, "Data/SJCSynch.rds")

for(i in 1:length(SJSRels)){
  cat("Run number: ", i, "")
  SJSSynch[[i]] <- LCSCalc(SJSDets[[i]],SJSPairs[[i]])
}
saveRDS(SJSSynch, "Data/SJSSynch.rds")


SJCTable<-vector(mode = "list", length = length(SJCRels))

SJSTable<-vector(mode = "list", length = length(SJSRels))

for(i in 1:length(SJCRels)){
  SJCTable[[i]] <- MarkovFishTable(SJCDets[[i]],"FishID","Loc","FirstDet","LastDet")
}
saveRDS(SJCTable, "Data/SJCTable.rds")

for(i in 1:length(SJSRels)){
  SJSTable[[i]] <- MarkovFishTable(SJSDets[[i]],"FishID","Loc","FirstDet","LastDet")
}
saveRDS(SJSTable, "Data/SJSTable.rds")



SJCSurrs<-vector(mode = "list", length = length(SJCRels))

SJSSurrs<-vector(mode = "list", length = length(SJSRels))

for(i in 1:length(SJCRels)){
  SJCSurrs[[i]] <- MarkovFishSurrogates(SJCTable[[i]],"Rel",0,10000,Reps = 1)
}
saveRDS(SJCSurrs, "Data/SJCSurrs.rds")

for(i in 1:length(SJSRels)){
  SJSSurrs[[i]] <- MarkovFishSurrogates(SJSTable[[i]],"Rel",0,10000,Reps = 1)
}
saveRDS(SJSSurrs, "Data/SJSSurrs.rds")

SJCSurrs<-readRDS("Data/SJCSurrs.rds")
SJSSurrs<-readRDS("Data/SJSSurrs.rds")
SJCSynch<-readRDS("Data/SJCSynch.rds")
SJSSynch<-readRDS("Data/SJSSynch.rds")
source("Code/Functions/MFPop.R")
SJCPop<-vector(mode = "list", length = length(SJCRels))

SJSPop<-vector(mode = "list", length = length(SJSRels))

for(i in 1:length(SJCRels)){
  SJCPop[[i]] <- MFPop(SJCSynch[[i]],SJCPairs[[i]],SJCSurrs[[i]][[1]], 100)
}
saveRDS(SJCPop, "Data/SJCPop.rds")

for(i in 1:length(SJSRels)){
  SJSPop[[i]] <- MFPop(SJCSynch[[i]],SJCPairs[[i]],SJCSurrs[[i]][[1]], 100)
}
saveRDS(SJSPop, "Data/SJSPop.rds")


# Distance stuff
SJDist<-read.csv("Data/Distance_matrix_SJ.csv")
rownames(SJDist) <- SJDist[,1]
SJDist<-SJDist[,-1]

SJCSynch<-readRDS("Data/SJCSynch.rds")
SJSSynch<-readRDS("Data/SJSSynch.rds")


SJCDistTable<-matrix(ncol = 7)
for(i in 1:length(SJCSynch)){
  Temp<-DistList(SJCSynch[[i]],SJDist)
  Temp<-cbind(Temp, rep_len(i, dim(Temp)[1]))
  SJCDistTable<-rbind(SJCDistTable,Temp)
}
SJCDistTable<-na.omit(SJCDistTable)



SJSDistTable<-matrix(ncol = 7)
for(i in 1:length(SJSSynch)){
  Temp<-DistList(SJSSynch[[i]],SJDist)
  Temp<-cbind(Temp, rep_len(i, dim(Temp)[1]))
  SJSDistTable<-rbind(SJSDistTable,Temp)
}
SJSDistTable<-na.omit(SJSDistTable)

pdf("Intermediate Stuff/PrelimDistSynchSJ.pdf")
par(mfrow=c(3,1))

plot(SJCDistTable[,4],SJCDistTable[,1], main = "Direct Chinook")
plot(SJCDistTable[,5],SJCDistTable[,2], main = "Phase Chinook")
plot(SJCDistTable[,6],SJCDistTable[,3], main = "Step Chinook")

plot(SJSDistTable[,4],SJSDistTable[,1], main = "Direct Steelhead")
plot(SJSDistTable[,5],SJSDistTable[,2], main = "Phase Steelhead")
plot(SJSDistTable[,6],SJSDistTable[,3], main = "Step Steelhead")

par(mfrow=c(1,1))
dev.off()


SteelTermMat<-matrix(ncol = 3, nrow = (length(unlist(SJSPairs))/2))
colnames(SteelTermMat)<-c("Dir","Pha","Ste")
MatDex<-1
for(i in 1:length(SJSSynch)){
  SubSynch<-SJSSynch[[i]]
  for(j in 1:length(SubSynch)){
    SteelTermMat[MatDex,1]<-SubSynch[[j]][[2]][length(SubSynch[[j]][[2]])]
    SteelTermMat[MatDex,2]<-SubSynch[[j]][[9]][length(SubSynch[[j]][[9]])]
    SteelTermMat[MatDex,3]<-SubSynch[[j]][[16]][length(SubSynch[[j]][[16]])]
    MatDex<-MatDex+1
  }
}
table(SteelTermMat[,1])
table(SteelTermMat[,2])
table(SteelTermMat[,3])

ChinTermMat<-matrix(ncol = 3, nrow = (length(unlist(SJCPairs))/2))
colnames(ChinTermMat)<-c("Dir","Pha","Ste")
MatDex<-1
for(i in 1:length(SJCSynch)){
  SubSynch<-SJCSynch[[i]]
  for(j in 1:length(SubSynch)){
    ChinTermMat[MatDex,1]<-SubSynch[[j]][[2]][length(SubSynch[[j]][[2]])]
    ChinTermMat[MatDex,2]<-SubSynch[[j]][[9]][length(SubSynch[[j]][[9]])]
    ChinTermMat[MatDex,3]<-SubSynch[[j]][[16]][length(SubSynch[[j]][[16]])]
    MatDex<-MatDex+1
  }
}
table(ChinTermMat[,1])
table(ChinTermMat[,2])
table(ChinTermMat[,3])

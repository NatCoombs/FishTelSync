
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
    OrdFishFrame<-FishFrame[order(FishFrame[,"FirstDet"]),]
    
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
  OutFrame <- na.omit(OutFrame)
  OrdOut<- OutFrame[order(OutFrame[,"FirstDet"]),]
  return(OrdOut)
}




# Script for testing synchrony of the San Joaquin "surrogacy" questions

SourceSJChin<-read.csv("Data/ChinookEventsFormatted/ALL_South_Delta_fall_Chinook_events_formatted_20220407.csv")
SourceSJSteel<-read.csv("Data/SteelheadEventsFormatted/ALL_South_Delta_fall_Steelhead_events_formatted_20220415.csv")

SJChin <- SourceSJChin
SJSteel <- SourceSJSteel

# Start by taking all serial numbers from SJBiomTable
SJBarrierTable<-read.csv("Data/SanJoaquin_category_file_03.csv")
SJBiomTable<-read.csv("Data/SJ_biometrics_all_years.csv")


for(i in 1:nrow(SJBiomTable)){
  SrnDexChin <- which(SJChin$Tag_ser_num %in% SJBiomTable$Serial.nr[i])
  SrnDexSteel <- which(SJSteel$Tag_ser_num %in% SJBiomTable$Serial.nr[i])
  if(length(SrnDexChin) > 0){
      if(SJBiomTable$Pathway[i] %in% c("mid")){
    SJChin$Tag_ID_1[SrnDexChin] <- SJBiomTable$Signal[i]
      } else {
        SJChin <- SJChin[-SrnDexChin,]
      }

  }
  if(length(SrnDexSteel > 0)){
    
      if(SJBiomTable$Pathway[i] %in% c("mid")){
        SJSteel$Tag_ID[SrnDexSteel] <- SJBiomTable$Signal[i]
      } else {
        SJSteel <- SJSteel[-SrnDexSteel,]
      }
      }
}
# 
# unique(SJChin$Tag_ser_num[which(! (SJChin$Tag_ser_num %in% SJBiomTable$Serial.nr))])
# unique(SJSteel$Tag_ser_num[which(! (SJSteel$Tag_ser_num %in% SJBiomTable$Serial.nr))])
# 
# SJChinBT <- SJChin[which(SJChin$Tag_ser_num %in% SJBiomTable$Serial.nr),]
# SJSteelBT <- SJSteel[which(SJSteel$Tag_ser_num %in% SJBiomTable$Serial.nr),]
# 
# SJChinCheck <- SJChin[-which(SJChin$Tag_ser_num %in% SJBiomTable$Serial.nr),]
# SJSteelCheck <- SJSteel[-which(SJSteel$Tag_ser_num %in% SJBiomTable$Serial.nr),]
# 
# which(SJSteelCheck$Tag_ser_num %in% SJBiomTable$Serial.nr)
# which(SJChinCheck$Tag_ser_num %in% SJBiomTable$Serial.nr)
# 
# which(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal)
# which(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal)
# # Sweet, let's try a cross-walk?
# table(SJBarrierTable[which((SJBarrierTable$Signal %% 2) == 1),"Species"])
# # Try sweeping by going down by one val on odds and then going up by one on evens? First dupe Tag_ID_2 to 1.
# which(which(is.na(SJChinCheck$Tag_ID_1)) %in% which(is.na(SJChinCheck$Tag_ID_2)))
# 
# for(i in 1:nrow(SJChinCheck)){
#   if(is.na(SJChinCheck$Tag_ID_1[i])){
#     SJChinCheck$Tag_ID_1[i] <- SJChinCheck$Tag_ID_2[i]
#   }
# }
# dim(SJChinCheck)
# ChinCheckSigs <- na.omit(unique(SJChinCheck$Tag_ID_1))
# ChinCheckSigsNMod2 <- ChinCheckSigs[which(as.logical(as.numeric(ChinCheckSigs) %% 2))]
# SJChinCheck
# for(i in 1:length(ChinCheckSigsNMod2)){
#   SCCross <- as.numeric(ChinCheckSigsNMod2[i])
#   SCCrossP <- SCCross-1
#   InDex <- c(which(SJChinCheck$Tag_ID_2 %in% SCCross), which(SJChinCheck$Tag_ID_1 %in% SCCross))
#   SJChinCheck$Tag_ID_1[InDex] <- SCCrossP
# }
# 
# dim(SJChinCheck)
# 
# SteelCheckSigs <- na.omit(unique(SJSteelCheck$Tag_ID))
# SteelCheckSigsNMod2 <- SteelCheckSigs[which(as.logical(as.numeric(SteelCheckSigs) %% 2))]
# #SJSteelCheck
# #SteelCheckSigsNMod2
# 
# 
# for(i in 1:length(SteelCheckSigsNMod2)){
#   SCCross <- as.numeric(SteelCheckSigsNMod2[i])
#   SCCrossP <- SCCross-1
#   InDex <- which(SJSteelCheck$Tag_ID %in% SCCross)
#   
#   SJSteelCheck$Tag_ID[InDex] <- SCCrossP
# }
# dim(SJSteelCheck)
# for(i in 1:nrow(SJBarrierTable)){
#   ChinDex <- which(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal[i])
#   SteelDex <- which(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal[i])
#   if(length(ChinDex > 0)){
#     if(SJBarrierTable$Barrier_status[i] %in% "Present"){
#       if(SJBarrierTable$Pathway[i] %in% c("mid")){
#         next
#       } else {
#         SJChinCheck <- SJChinCheck[-ChinDex,]
#       }
#     } else {
#       SJChinCheck <- SJChinCheck[-ChinDex,]
#     }
#   }
#   if(length(SteelDex > 0)){
#     if(SJBarrierTable$Barrier_status[i] %in% "Present"){
#       if(SJBarrierTable$Pathway[i] %in% c("mid")){
#         next
#       } else {
#         SJSteelCheck <- SJSteelCheck[-SteelDex,]
#       }
#     } else {
#       SJSteelCheck <- SJSteelCheck[-SteelDex,]
#     }
#   }
# }
# SJChinCM2 <- SJChinCheck[which(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal),]
# SJSteelCM2 <- SJSteelCheck[which(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal),]
# dim(SJChinCheck)
# dim(SJSteelCheck)
# dim(SJChinCM2)
# dim(SJSteelCM2)
# # Now remove the ones we just saved and repeat for mod 1
# SJChinCheck <- SJChinCheck[-which(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal),]
# SJSteelCheck <- SJSteelCheck[-which(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal),]
# dim(SJChinCheck)
# dim(SJSteelCheck)
# ChinCheckSigs <- na.omit(unique(SJChinCheck$Tag_ID_1))
# # Remember, only Mod2 remain now
# for(i in 1:length(ChinCheckSigs)){
#   SCCross <- as.numeric(ChinCheckSigs[i])
#   SCCrossP <- SCCross+1
#   InDex <- which(SJChinCheck$Tag_ID_1 %in% SCCross)
#   SJChinCheck$Tag_ID_1[InDex] <- SCCrossP
# }
# any(is.na(SJChinCheck$Tag_ID_1))
# 
# SteelCheckSigs <- na.omit(unique(SJSteelCheck$Tag_ID))
# for(i in 1:length(SteelCheckSigs)){
#   SCCross <- as.numeric(SteelCheckSigs[i])
#   SCCrossP <- SCCross+1
#   InDex <- which(SJSteelCheck$Tag_ID %in% SCCross)
#   
#   SJSteelCheck$Tag_ID[InDex] <- SCCrossP
# }
# 
# for(i in 1:nrow(SJBarrierTable)){
#   ChinDex <- which(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal[i])
#   SteelDex <- which(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal[i])
#   if(length(ChinDex > 0)){
#     if(SJBarrierTable$Barrier_status[i] %in% "Present"){
#       if(SJBarrierTable$Pathway[i] %in% c( "mid")){
#         next
#       } else {
#         SJChinCheck <- SJChinCheck[-ChinDex,]
#       }
#     } else {
#       SJChinCheck <- SJChinCheck[-ChinDex,]
#     }
#   }
#   if(length(SteelDex > 0)){
#     if(SJBarrierTable$Barrier_status[i] %in% "Present"){
#       if(SJBarrierTable$Pathway[i] %in% c("mid")){
#         next
#       } else {
#         SJSteelCheck <- SJSteelCheck[-SteelDex,]
#       }
#     } else {
#       SJSteelCheck <- SJSteelCheck[-SteelDex,]
#     }
#   }
# }
# SJChinCM1 <- SJChinCheck[which(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal),]
# SJSteelCM1 <- SJSteelCheck[which(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal),]
# 
# SJChinRem <- SJChinCheck[which(!(SJChinCheck$Tag_ID_1 %in% SJBarrierTable$Signal)),]
# SJSteelRem <- SJSteelCheck[which(!(SJSteelCheck$Tag_ID %in% SJBarrierTable$Signal)),]
# 
# RemChin <- unique(SJChinCheck$Tag_ID_1)[which(!(unique(SJChinCheck$Tag_ID_1) %in% SJBarrierTable$Signal))]
# RemSteel <- unique(SJSteelCheck$Tag_ID)[which(!(unique(SJSteelCheck$Tag_ID) %in% SJBarrierTable$Signal))]
# RemChin
# RemSteel
# 
# dim(SJChinCM2)
# dim(SJChinCM1)
# dim(SJChinBT)
# dim(SourceSJChin)
# 
# dim(SJSteelCM2)
# dim(SJSteelCM1)
# dim(SJSteelBT)
# dim(SourceSJSteel)
# # No steel remaining, so anything dropped from the source are for constraints on cleaning
# # Check for chin just in case?
# 
# which(SJChinRem$Tag_ID_1 %in% SJBiomTable$Signal)
# 
# # Yeah no, what's missing here is missing. Let's staple these together
# 
# 
# SJChin <- rbind(SJChinBT, SJChinCM1, SJChinCM2)
# SJSteel <- rbind(SJSteelBT, SJSteelCM1, SJSteelCM2)

SJChinSers <- unique(SJChin$Tag_ser_num)
SJSteelSers <- unique(SJSteel$Tag_ser_num)

SJCArrMat <- matrix(data = NA, nrow = length(SJChinSers), ncol = 2)
SJSArrMat <- matrix(data = NA, nrow = length(SJSteelSers), ncol = 2)

TAGArrayList <- c("A0","A2","A4","A5","B0","B1","B2","B4","C2","G1","A6","A7","A8","A11","A12","Rel")

for(i in 1:length(SJChinSers)){
  SJCArrMat[i,1] <- SJChinSers[i]
  Arrsi <- unique(SJChin[which(SJChin$Tag_ser_num %in% SJChinSers[i]),23])
  SJCArrMat[i,2] <- length(which(Arrsi %in% TAGArrayList))
}

for(i in 1:length(SJSteelSers)){
  SJSArrMat[i,1] <- SJSteelSers[i]
  Arrsi <- unique(SJSteel[which(SJSteel$Tag_ser_num %in% SJSteelSers[i]),24])
  SJSArrMat[i,2] <- length(which(Arrsi %in% TAGArrayList))
}

table(SJCArrMat[,2])
table(SJSArrMat[,2])

# # # Now we need to check if we need to drop eeeeven more fish. Yayyyyy.


SJChinFish <- cbind(SJChin$Release_date_time, SJChin$Tag_ser_num)
table(SJChinFish[,1])

SJSteelFish <- cbind(SJSteel$Release_date_time, SJSteel$Tag_ser_num)
table(SJSteelFish[,1])

SJCRels<-na.omit(unique(SJChinFish[,1]))
SJSRels<-na.omit(unique(SJSteelFish[,1]))

SJCFish<-vector(mode = "list", length = length(SJCRels))
names(SJCFish) <- SJCRels
for(i in 1:length(SJCRels)){
  KeySet <- which(SJChinFish[,1] == SJCRels[i])
  #print(KeySet)
  SJCFish[[i]] <- unique(SJChinFish[KeySet,2])
}


SJSFish<-vector(mode = "list", length = length(SJSRels))
names(SJSFish) <- SJSRels
for(i in 1:length(SJSRels)){
  KeySet <- which(SJSteelFish[,1] == SJSRels[i])
  SJSFish[[i]] <- unique(SJSteelFish[KeySet,2])
}


SJCRSMat<- matrix(data = NA, nrow = length(SJCFish), ncol = 2)
SJCRSMat[,1] <- names(SJCFish)
for(i in 1:length(SJCFish)){
  SJCRSMat[i,2] <- length(SJCFish[[i]])
}

SJSRSMat<- matrix(data = NA, nrow = length(SJSFish), ncol = 2)
SJSRSMat[,1] <- names(SJSFish)
for(i in 1:length(SJSFish)){
  SJSRSMat[i,2] <- length(SJSFish[[i]])
}

SJCRSMat
SJSRSMat
which(as.numeric(SJCRSMat[,2]) > 7)
# We still end up with 51 of the chinook releases if we use 8 as the cutoff. So we're using 8 as the cutoff, probably.
table(SJCRSMat[,2])[order(as.numeric(names(table(SJCRSMat[,2]))))]
table(SJSRSMat[,2])[order(as.numeric(names(table(SJSRSMat[,2]))))]

TAGArrayList <- c("A0","A2","A4","A5","B0","B1","B2","B4","C2","G1","A6","A7","A8","A11","A12","Rel")


SJCDets<-vector(mode = "list", length = length(SJCRels))
names(SJCDets) <- SJCRels
for(i in 1:length(SJCRels)){

  SubFrame <- SJChin[which(SJChin$Release_date_time == SJCRels[i]), c(1,23,10,11,12)]
  #print(SubFrame)
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
  #print(SubFrame)
  #SubFrame <- SubFrame[-which(SubFrame$NumDets == 1 & SubFrame$Loc != "Rel"),]
  #print(SubFrame)
  RelT <- as.numeric(as.POSIXct(paste(substr(SJCRels[i], 1, 10)," ", substr(SJCRels[i], 12, 19)), format = "%Y-%m-%d %H:%M:%OS"))
 
  DropVec <- which(SubFrame$FirstDet < RelT)
  if(length(DropVec) != 0){
    SubFrame <- SubFrame[-which(SubFrame$FirstDet < RelT),]
  } 
  #print(SubFrame)
  SJCDets[[i]] <- SubFrame
  
}

SJSDets<-vector(mode = "list", length = length(SJSRels))
names(SJSDets) <- SJSRels
for(i in 1:length(SJSRels)){
  
  SubFrame <- SJSteel[which(SJSteel$Release_date_time == SJSRels[i]), c(1,24,11,12,13)]
  
  colnames(SubFrame) <- c("FishID","Loc","FirstDet","LastDet","NumDets")
  #print(SubFrame$FishID)
  for(j in 1:length(SJSFish[[i]])){
    SubFrame <- rbind(SubFrame,c(SJSFish[[i]][j], "Rel", SJSRels[i], SJSRels[i], 1))
  }
  #print(SubFrame$FishID)
  #print(SubFrame$FirstDet)
  # # # # # # #NEED TO GET NEW BREAKPOINTS
  if(i < 292){
    psxformat <- "%Y-%m-%d %H:%M:%OS" 
    RelT <- as.numeric(as.POSIXct(SJSRels[i], format = psxformat))
  } else {
    SubFrame$FirstDet <- gsub("15 ", "2015 ", SubFrame$FirstDet)
    SubFrame$LastDet <- gsub("15 ", "2015 ", SubFrame$LastDet)
    
    SubFrame$FirstDet <- gsub("16 ", "2016 ", SubFrame$FirstDet)
    SubFrame$LastDet <- gsub("16 ", "2016 ", SubFrame$LastDet)
    
    psxformat <- "%m/%d/%Y %H:%M" 
    
    RelT <- as.numeric(as.POSIXct(gsub("16 ", "2016 ", gsub("15 ", "2015 ", SJSRels[i])), format = psxformat))
  }
  SubFrame$FirstDet <- as.numeric(as.POSIXct(SubFrame$FirstDet, format = psxformat))
  SubFrame$LastDet <-  as.numeric(as.POSIXct(SubFrame$LastDet, format = psxformat))
  SubFrame <- SubFrame[which(SubFrame$Loc %in% TAGArrayList),]
  #print(SubFrame$FishID)
  #print(SubFrame$FirstDet)
  SubFrame <- DetCondenseSurrog(SubFrame)
  #SubFrame <- SubFrame[-which(SubFrame$NumDets == 1 & SubFrame$Loc != "Rel"),]
  
  DropVec <- which(SubFrame$FirstDet < RelT)
  if(length(DropVec) != 0){
    SubFrame <- SubFrame[-which(SubFrame$FirstDet < RelT),]
  } 
  
  
  SJSDets[[i]] <- SubFrame[which(SubFrame$Loc %in% TAGArrayList),]
  
}

print(SJSDets[[312]])
print(SJSDets[[313]])
print(SJSDets[[314]])

SJCDets[[1]]




SJCPairs<-vector(mode = "list", length = length(SJCRels))
names(SJCPairs) <- SJCRels
for(i in 1:length(SJCRels)){
  FishSet <- SJCFish[[i]]
  if(length(FishSet) > 1){
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
}

SJSPairs<-vector(mode = "list", length = length(SJSRels))
names(SJSPairs) <- SJSRels
for(i in 1:length(SJSRels)){
  FishSet <- SJSFish[[i]]
  if(length(FishSet) > 1){
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
}


# Writing out detections & pairs to be used for synchrony testing on the cluster
if(!dir.exists("./Data/SJReduced1")){
  dir.create("./Data/SJReduced1")
}
if(!dir.exists("./Data/SJReduced1/Dets")){
  dir.create("./Data/SJReduced1/Dets")
}
if(!dir.exists("./Data/SJReduced1/Pairs")){
  dir.create("./Data/SJReduced1/Pairs")
}

AllDets<-vector(mode = "list", length = length(SJCPairs) + length(SJSPairs))
AllPairs<-vector(mode = "list", length = length(AllDets))

for(i in 1:length(SJCPairs)){
  AllDets[[i]] <- SJCDets[[i]]
  AllPairs[[i]] <- SJCPairs[[i]]
}
for(i in 1:length(SJSPairs)){
  j <- i + length(SJCPairs)
  AllDets[[j]] <- SJSDets[[i]]
  AllPairs[[j]] <- SJSPairs[[i]]
}

for(i in 1:length(AllDets)){
  write.csv(AllPairs[[i]], file = paste0("./Data/SJReduced1/Pairs/", i, ".csv"), row.names=FALSE)
  write.csv(AllDets[[i]], file = paste0("./Data/SJReduced1/Dets/", i, ".csv"), row.names=FALSE)
}




saveRDS(SJCPairs,"Data/SJCPairs.rds")
saveRDS(SJSPairs,"Data/SJSPairs.rds")
# And now we need to print out which indices to run on the cluster and get that going, but we're very close now!
SJCRS<-as.numeric(SJCRSMat[,2])
SJSRS<-as.numeric(SJSRSMat[,2])
ClusterRunVec <- c(which(SJCRS > 7), 232 + which(SJSRS > 7)) 
CompRunVec <- c(which(SJCRS > 5), 232 + which(SJSRS > 5)) 
cat("[", paste(ClusterRunVec, collapse = ","), "]", sep = "")

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
SJSDebug(1,22)

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
SJCDistTable<-SJCDistTable[-1,]

SJCDistTable<-apply(SJCDistTable,1:2,as.numeric)
SJCDistTable[,1:3]<-apply(SJCDistTable[,1:3],1:2,function(x) {
  if(x == 0){x} 
  else {1/x}})
SJCDistTable<-as.data.frame(SJCDistTable)
colnames(SJCDistTable) <- c("DSynch", "PSynch", "SSynch", "DDist",  "PDist",  "SDist",  "Alignment length")

SJSDistTable<-matrix(ncol = 7)
for(i in 1:length(SJSSynch)){
  Temp<-DistList(SJSSynch[[i]],SJDist)
  Temp<-cbind(Temp, rep_len(i, dim(Temp)[1]))
  SJSDistTable<-rbind(SJSDistTable,Temp)
}
SJSDistTable<-SJSDistTable[-1,]
SJSDistTable<-apply(SJSDistTable,1:2,as.numeric)
#print(SJSDistTable)

SJSDistTable[,1:3]<-apply(SJSDistTable[,1:3],1:2,function(x) {
  if(x == 0){x} 
  else {1/x}})
#print(SJSDistTable)
SJSDistTable<-as.data.frame(SJCDistTable)
colnames(SJSDistTable) <- c("DSynch", "PSynch", "SSynch", "DDist",  "PDist",  "SDist",  "Alignment length")

boxplot("DSynch", SJCDistTable)

pdf("Intermediate Stuff/PrelimDistSynchSJ.pdf")
par(mfrow=c(3,1))

#boxplot("Dir"~"DirDist", data = SJCDistTable)

plot(SJCDistTable[,4],SJCDistTable[,1], main = "Direct Chinook")
plot(SJCDistTable[,5],SJCDistTable[,2], main = "Phase Chinook")
plot(SJCDistTable[,6],SJCDistTable[,3], main = "Step Chinook")
plot(SJCDistTable[,7],SJCDistTable[,1], main = "Direct Chinook")
plot(SJCDistTable[,7],SJCDistTable[,2], main = "Phase Chinook")
plot(SJCDistTable[,7],SJCDistTable[,3], main = "Step Chinook")

plot(SJSDistTable[,4],SJSDistTable[,1], main = "Direct Steelhead")
plot(SJSDistTable[,5],SJSDistTable[,2], main = "Phase Steelhead")
plot(SJSDistTable[,6],SJSDistTable[,3], main = "Step Steelhead")
plot(SJCDistTable[,7],SJCDistTable[,1], main = "Direct Steelhead")
plot(SJCDistTable[,7],SJCDistTable[,2], main = "Phase Steelhead")
plot(SJCDistTable[,7],SJCDistTable[,3], main = "Step Steelhead")

par(mfrow=c(1,1))
dev.off()


SteelTermMat<-matrix(ncol = 6, nrow = (length(unlist(SJSPairs))/2))
SteelScrewyMat<-matrix(ncol = 2, nrow = (length(unlist(SJSPairs))/2))
colnames(SteelTermMat)<-c("Dir","Pha","Ste","DL","PL","SL")
MatDex<-1
ScrewyDex<-1
for(i in 1:length(SJSSynch)){
  SubSynch<-SJSSynch[[i]]
  for(j in 1:length(SubSynch)){
    SteelTermMat[MatDex,1]<-SubSynch[[j]][[2]][length(SubSynch[[j]][[2]])]
    SteelTermMat[MatDex,2]<-SubSynch[[j]][[9]][length(SubSynch[[j]][[9]])]
    SteelTermMat[MatDex,3]<-SubSynch[[j]][[16]][length(SubSynch[[j]][[16]])]
    SteelTermMat[MatDex,4]<-length(SubSynch[[j]][[2]])
    SteelTermMat[MatDex,5]<-length(SubSynch[[j]][[9]])
    SteelTermMat[MatDex,6]<-length(SubSynch[[j]][[16]])
    if("Rel" %in% SteelTermMat[MatDex,1:3] && !(1 %in% 
    SteelTermMat[MatDex,4:6])){
      SteelScrewyMat[ScrewyDex,1] <- i
      SteelScrewyMat[ScrewyDex,2] <- j
      ScrewyDex<-ScrewyDex+1
      print(c(i,j))
      # print(SubSynch[[j]][[2]])
      # print(SubSynch[[j]][[9]])
      # print(SubSynch[[j]][[16]])
    }
    MatDex<-MatDex+1
  }
}
table(SteelTermMat[,1])
table(SteelTermMat[,2])
table(SteelTermMat[,3])
# table(SteelTermMat[,4])
# table(SteelTermMat[,5])
# table(SteelTermMat[,6])

ChinTermMat<-matrix(ncol = 6, nrow = (length(unlist(SJCPairs))/2))
colnames(ChinTermMat)<-c("Dir","Pha","Ste","DL","PL","SL")
MatDex<-1
for(i in 1:length(SJCSynch)){
  SubSynch<-SJCSynch[[i]]
  for(j in 1:length(SubSynch)){
    ChinTermMat[MatDex,1]<-SubSynch[[j]][[2]][length(SubSynch[[j]][[2]])]
    ChinTermMat[MatDex,2]<-SubSynch[[j]][[9]][length(SubSynch[[j]][[9]])]
    ChinTermMat[MatDex,3]<-SubSynch[[j]][[16]][length(SubSynch[[j]][[16]])]
    ChinTermMat[MatDex,4]<-length(SubSynch[[j]][[2]])
    ChinTermMat[MatDex,5]<-length(SubSynch[[j]][[9]])
    ChinTermMat[MatDex,6]<-length(SubSynch[[j]][[16]])
    if("Rel" %in% c(ChinTermMat[MatDex,c(1,2,3)])){
      print(c(i,j))
      print(SubSynch[[j]][[2]])
      print(SubSynch[[j]][[9]])
      print(SubSynch[[j]][[16]])
    }
    MatDex<-MatDex+1
  }
}
table(ChinTermMat[,1])
table(ChinTermMat[,2])
table(ChinTermMat[,3])
# table(ChinTermMat[,4])
# table(ChinTermMat[,5])
# table(ChinTermMat[,6])

# 
# for(i in 1:length(SJCDets)){
#   sort(order)
# }
# 
# 
# 
# 
# 
par(mfrow=c(3,2))

#boxplot("Dir"~"DirDist", data = SJCDistTable)

plot(SJCDistTable[,4],SJCDistTable[,1], main = "Direct Chinook")
plot(SJSDistTable[,4],SJSDistTable[,1], main = "Direct Steelhead")
plot(SJCDistTable[,5],SJCDistTable[,2], main = "Phase Chinook")
plot(SJSDistTable[,5],SJSDistTable[,2], main = "Phase Steelhead")
plot(SJCDistTable[,6],SJCDistTable[,3], main = "Step Chinook")




plot(SJSDistTable[,6],SJSDistTable[,3], main = "Step Steelhead")


par(mfrow=c(1,1))



# Let's fix our clustering issues now.
ClustTestSynch1 <- SJCSynch[[72]]
ClustTestFish1 <- SJCFish[[72]]
ClustTestPairs1 <- SJCPairs[[72]]

ClustTestMat<-matrix(data = 0, nrow = length(ClustTestFish1), ncol = length(ClustTestFish1))

rownames(ClustTestMat) <- ClustTestFish1
colnames(ClustTestMat) <- ClustTestFish1
PairDex <- 1
for(i in 1:(length(ClustTestFish1)-1)){
  for(j in (i+1):length(ClustTestFish1)){
    ZCheck <- as.numeric(ClustTestSynch1[[PairDex]][[7]])
    if(ZCheck != 0){
      ZCheck <- 1/ZCheck
    }
    ClustTestMat[i,j] <- ZCheck
    ClustTestMat[j,i] <- ZCheck
    PairDex <- PairDex + 1
  }
}
image(ClustTestMat)

ClusTestEig<-wsyn::cluseigen(ClustTestMat)
ClusTestMembs<-ClusTestEig[[length(ClusTestEig)]]
wsyn::modularity(ClustTestMat,ClusTestMembs)




# Reading in the data from the cluster


OutFileSet <- list.files("/Users/nathanielcoombs/Desktop/Cluster_folders/SjSurrogCluster/Outputs/Pops/")
OutSet <- vector(mode = "character", length = length(OutFileSet))
for(i in 1:length(OutFileSet)){
  OutSet[i] <- strsplit(OutFileSet[i], ".", fixed = T)[[1]][1]
}


cat(paste0(which(!(1:638 %in% OutSet)),collapse = ","))

which(!(1:638 %in% OutSet))
length(which(which(!(1:638 %in% OutSet)) %in% ClusterRunVec))

# Check why these didn't go through
which(!(1:638 %in% OutSet))[which(which(!(1:638 %in% OutSet)) %in% ClusterRunVec)]

SJCRS
#<-as.numeric(SJCRSMat[,2])
SJSRS


SJSPairSig<-vector(mode = "list", length = length(which(SJSRS > 7)))
names(SJSPairSig) <- SJSRSMat[which(SJSRS > 7),1]

SJSPopSig<-vector(mode = "list", length = length(which(SJSRS > 7)))
names(SJSPopSig) <- SJSRSMat[which(SJSRS > 7),1]


SJCPairSig<-vector(mode = "list", length = length(which(SJCRS > 7)))
names(SJCPairSig) <- SJCRSMat[which(SJCRS > 7),1]

SJCPopSig<-vector(mode = "list", length = length(which(SJCRS > 7)))
names(SJCPopSig) <- SJCRSMat[which(SJCRS > 7),1]

#<-as.numeric(SJSRSMat[,2])
ClusterRunVec <- c(which(SJCRS > 7), 232 + which(SJSRS > 7)) 
ClusterRunRels <- c(SJCRSMat[which(SJCRS > 7),1], SJSRSMat[which(SJSRS > 7),1]) 

# # # Try refreshing the data and checking for NA's

SJCErr<-vector(mode = "numeric")
for(i in 1:length(ClusterRunVec)){
  if(!(ClusterRunVec[i] %in% as.numeric(OutSet))){
    next
  }
  if(ClusterRunVec[i] < 233){
  SJCPopSig[[i]] <- readRDS(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/SjSurrogCluster/Outputs/Pops/", ClusterRunVec[i], ".rds", sep = ""))
  SJCPairSig[[i]] <- read.csv(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/SjSurrogCluster/Outputs/Pairs/", ClusterRunVec[i], ".csv", sep = ""))
  a<-i
  } else {
    SJSPopSig[[i-a]] <- readRDS(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/SjSurrogCluster/Outputs/Pops/", ClusterRunVec[i], ".rds", sep = ""))
    SJSPairSig[[i-a]] <- read.csv(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/SjSurrogCluster/Outputs/Pairs/", ClusterRunVec[i], ".csv", sep = ""))
  }
}



SJSPopMat <- matrix(data = NA, nrow = length(SJSPopSig), ncol = 12)
colnames(SJSPopMat) <- c("MeanLD", "MeanPS", "MeanLS", "ModLD", "ModPS", "ModLS", "PMeanLD", "PMeanPS", "PMeanLS", "PModLD", "PModPS", "PModLS")

SJCPopMat <- matrix(data = NA, nrow = length(SJCPopSig), ncol = 12)
colnames(SJCPopMat) <- c("MeanLD", "MeanPS", "MeanLS", "ModLD", "ModPS", "ModLS", "PMeanLD", "PMeanPS", "PMeanLS", "PModLD", "PModPS", "PModLS")

rownames(SJCPopMat) <- names(SJCPopSig)

rownames(SJSPopMat) <- names(SJSPopSig)
# # # Try refreshing the data and checking for NA's

for(i in 1:length(SJSPopSig)){
  #print(i)
  PopSigTemp <- SJSPopSig[[i]]
  if(length(PopSigTemp) == 0){
    next
  }
  SJSPopMat[i,1] <- 1/PopSigTemp[[1]][1,1]
  SJSPopMat[i,2] <- 1/PopSigTemp[[1]][1,2]
  SJSPopMat[i,3] <- 1/PopSigTemp[[1]][1,3]
  
  SJSPopMat[i,4] <- PopSigTemp[[1]][2,1]
  SJSPopMat[i,5] <- PopSigTemp[[1]][2,2]
  SJSPopMat[i,6] <- PopSigTemp[[1]][2,3]
  
  
  
  SJSPopMat[i,7] <- PopSigTemp[[2]][1,1]/100
  SJSPopMat[i,8] <- PopSigTemp[[2]][1,2]/100
  SJSPopMat[i,9] <- PopSigTemp[[2]][1,3]/100
  
  SJSPopMat[i,10] <- 1-(PopSigTemp[[2]][2,1]/100)
  SJSPopMat[i,11] <- 1-(PopSigTemp[[2]][2,2]/100)
  SJSPopMat[i,12] <- 1-(PopSigTemp[[2]][2,3]/100)
}


for(i in 1:length(SJCPopSig)){
  #print(i)
  PopSigTemp <- SJCPopSig[[i]]
  if(length(PopSigTemp) == 0){
    #print(i)
    next
  }
  SJCPopMat[i,1] <- 1/PopSigTemp[[1]][1,1]
  SJCPopMat[i,2] <- 1/PopSigTemp[[1]][1,2]
  SJCPopMat[i,3] <- 1/PopSigTemp[[1]][1,3]
  
  SJCPopMat[i,4] <- PopSigTemp[[1]][2,1]
  SJCPopMat[i,5] <- PopSigTemp[[1]][2,2]
  SJCPopMat[i,6] <- PopSigTemp[[1]][2,3]
  
  
  
  SJCPopMat[i,7] <- PopSigTemp[[2]][1,1]/100
  SJCPopMat[i,8] <- PopSigTemp[[2]][1,2]/100
  SJCPopMat[i,9] <- PopSigTemp[[2]][1,3]/100
  
  SJCPopMat[i,10] <- 1-(PopSigTemp[[2]][2,1]/100)
  SJCPopMat[i,11] <- 1-(PopSigTemp[[2]][2,2]/100)
  SJCPopMat[i,12] <- 1-(PopSigTemp[[2]][2,3]/100)
}

#CorrSJBiomTable<-read.csv("/Users/nathanielcoombs/Downloads/All_year_size_summary.csv")
CorrSJBiomTable<-read.csv("/Users/nathanielcoombs/Downloads/All_year_size_summary (2).csv")
CorrSJChinTable<-read.csv("/Users/nathanielcoombs/Downloads/CHN_corrected_sizes_2012-2015.csv")

CorrSJChinTable<-CorrSJChinTable[!duplicated(CorrSJChinTable),]
CorrSJBiomTable<-CorrSJBiomTable[!duplicated(CorrSJBiomTable),]
InCorrChinVec<-which(CorrSJBiomTable$Group == "CHN")
for(i in InCorrChinVec){
  Repdex<- which(CorrSJChinTable$Serial.nr == CorrSJBiomTable$Serial.nr[i])
  CorrSJBiomTable[i,5] <- CorrSJChinTable[Repdex,2]
  CorrSJBiomTable[i,6] <- CorrSJChinTable[Repdex,3]
}

RawSJS16<-read.csv("/Users/nathanielcoombs/Downloads/2016_Steelhead_Tagging_Info.csv")
RawSJC16<-read.csv("/Users/nathanielcoombs/Downloads/2016_Chinook_Tagging_Info.csv")

SJC16Sub<-cbind(RawSJC16[,c(52,37,14,15,22,20)],"CHN", NA)
colnames(SJC16Sub) <- colnames(CorrSJBiomTable)

SJS16Sub<-cbind(RawSJS16[,c(65,90,13,14,21,20)],"STL", NA)
colnames(SJS16Sub) <- colnames(CorrSJBiomTable)

CorrSJBiomTable <- rbind(CorrSJBiomTable,SJS16Sub,SJC16Sub)

CorrSJBiomTable<-CorrSJBiomTable[!duplicated(CorrSJBiomTable),]
#CorrSJChin <- CorrSJBiomTable[which(CorrSJBiomTable$Group %in% "CHN"),]
#CorrSJChinSub <- CorrSJChin[which(CorrSJChin$Fish.Length < 85),]

# # # Need to recheck for potential NA sources
ClustBioms<-vector(mode = "list", length(ClusterRunRels))
names(ClustBioms) <- ClusterRunRels
for(i in 1:length(ClustBioms)){

  if(ClusterRunVec[i] < 233){
    Fish <- SJCFish[[ClusterRunVec[i]]]
  } else {
    Fish <- SJSFish[[ClusterRunVec[i] - 232]]
  }
  ClustBiomTable <- matrix(data = NA, nrow = length(Fish), ncol = 3)
  #print(Fish)
  colnames(ClustBiomTable) <- c("Fish", "Length", "Weight")
  #print(i)
  for(j in 1:length(Fish)){
    #print(j)
    ClustBiomTable[j,1] <- Fish[j]
    #print(Fish[j])
    # Now we if/else chain for l/w.
    FishBiomKey <- which(CorrSJBiomTable$Serial.nr %in% Fish[j])
    #print(FishBiomKey)
    #print(Fish[j])
    # Need a good way to do some clever setting of FishSignal as a value
    # if(ClusterRunVec[i] < 143) {
    #    FishSignalTemp <- 
    # } else {
    #   
    # }
    FishBarrierKey <- which(SJBarrierTable$Signal %in% Fish[j])
    #print(FishBiomKey)
    if(length(FishBiomKey) > 0){
      ClustBiomTable[j,2] <- CorrSJBiomTable$Fish.Length[FishBiomKey]
      ClustBiomTable[j,3] <- CorrSJBiomTable$Fish.Weight[FishBiomKey]
    } 
  }
  ClustBioms[[i]] <- ClustBiomTable
}



SJCLMean <- vector(mode = "numeric")
SJCWMean <- vector(mode = "numeric")
SJCLVar <- vector(mode = "numeric")
SJCWVar <- vector(mode = "numeric")

SJSLMean <- vector(mode = "numeric")
SJSWMean <- vector(mode = "numeric")
SJSLVar <- vector(mode = "numeric")
SJSWVar <- vector(mode = "numeric")
# # # Running into weird NA's for biometric data in here.
for(i in 1:length(ClusterRunVec)){

  #print(is.null(ClustBioms[[i]]))
  if(!(is.null(ClustBioms[[i]]))){
    Lmean <- mean(as.numeric(ClustBioms[[i]][,2]), na.rm = T)
    Lvar <- var(as.numeric(ClustBioms[[i]][,2]), na.rm = T)
    
    Wmean <- mean(as.numeric(ClustBioms[[i]][,3]), na.rm = T)
    Wvar <- var(as.numeric(ClustBioms[[i]][,3]), na.rm = T)}
  else {
    print(i)
    Lmean <- NA
    Lvar <- NA
    
    Wmean <- NA
    Wvar <- NA
    
  }
  if(ClusterRunVec[i] < 233){
    #print(i)
    SJCLMean <- c(SJCLMean, Lmean)
    SJCLVar <- c(SJCLVar, Lvar)
    
    SJCWMean <- c(SJCWMean, Wmean)
    SJCWVar <- c(SJCWVar, Wvar)
    #print(i)
  } else {
    
    SJSLMean <- c(SJSLMean, Lmean)
    SJSLVar <- c(SJSLVar, Lvar)
    
    SJSWMean <- c(SJSWMean, Wmean)
    SJSWVar <- c(SJSWVar, Wvar)
    #print(i)
  }
}


SJSPopMat <- cbind(SJSPopMat, SJSLMean, SJSLVar, SJSWMean, SJSWVar)
SJCPopMat <- cbind(SJCPopMat, SJCLMean, SJCLVar, SJCWMean, SJCWVar)



nrow(SJCPopMat)
nrow(SJSPopMat)
# # #

# LOOK AT AND FOR NA's'
# # #
# SJCPopMat<- na.omit(SJCPopMat)
# SJSPopMat<- na.omit(SJSPopMat)
# Temporarily dropping NA's solely based on synchrony outputs due to issues with biom file
SJCPopMat<-SJCPopMat[!is.na(SJCPopMat[,10]),]
SJSPopMat<-SJSPopMat[!is.na(SJSPopMat[,10]),]
nrow(SJCPopMat)
nrow(SJSPopMat)


#####
pdf(file = "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Intermediate stuff/SJSynchTempPop.pdf")
par(mfrow=c(2,2))
boxplot(SJSPopMat[,1:3])
title(main = "SJSPopRaw")
boxplot(SJSPopMat[,3+(1:3)])
boxplot(SJSPopMat[,6+(1:3)])
abline(h = c(.05, .95), col = "Red")
boxplot(SJSPopMat[,9+(1:3)])
abline(h = c(.05, .95), col = "Red")

par(mfrow=c(1,1))
plot(x = SJSPopMat[,1], y = SJSPopMat[,2])
title(main = "Phase vs Euclidean")
plot(x = SJSPopMat[,1], y = SJSPopMat[,3])
title(main = "Stepwise vs Euclidean")
plot(x = SJSPopMat[,2], y = SJSPopMat[,3])
title(main = "Stepwise vs Phase")

plot(x = log(SJSPopMat[,1]), y = log(SJSPopMat[,2]))
title(main = "Phase vs Euclidean")
plot(x = log(SJSPopMat[,1]), y = log(SJSPopMat[,3]))
title(main = "Stepwise vs Euclidean")
plot(x = log(SJSPopMat[,2]), y = log(SJSPopMat[,3]))
title(main = "Stepwise vs Phase")

plot(x = SJSPopMat[,1], y = SJSPopMat[,4])
title(main = "EucMod vs Euclidean")
plot(x = SJSPopMat[,2], y = SJSPopMat[,5])
title(main = "PhaseMod vs Phase")
plot(x = SJSPopMat[,3], y = SJSPopMat[,6])
title(main = "StepwiseMod vs Stepwise")

plot(x = log(SJSPopMat[,1]), y = SJSPopMat[,4])
title(main = "EucMod vs Euclidean")
plot(x = log(SJSPopMat[,2]), y = SJSPopMat[,5])
title(main = "PhaseMod vs Phase")
plot(x = log(SJSPopMat[,3]), y = SJSPopMat[,6])
title(main = "StepwiseMod vs Stepwise")

plot(x = SJSPopMat[,4], y = SJSPopMat[,5])
title(main = "PhaseMod vs EucMod")
plot(x = SJSPopMat[,4], y = SJSPopMat[,6])
title(main = "StepwiseMod vs EucMod")
plot(x = SJSPopMat[,5], y = SJSPopMat[,6])
title(main = "StepwiseMod vs PhaseMod")

plot(x = SJSPopMat[,1], y = SJSPopMat[,7])
abline(h = c(.05, .95), col = "Red")
title(main = "EucSig vs Euclidean")
plot(x = SJSPopMat[,2], y = SJSPopMat[,8])
abline(h = c(.05, .95), col = "Red")
title(main = "PhaseSig vs Phase")
plot(x = SJSPopMat[,3], y = SJSPopMat[,9])
abline(h = c(.05, .95), col = "Red")
title(main = "StepwiseSig vs Stepwise")

plot(x = log(SJSPopMat[,1]), y = SJSPopMat[,7])
abline(h = c(.05, .95), col = "Red")
title(main = "EucSig vs Euclidean")
plot(x = log(SJSPopMat[,2]), y = SJSPopMat[,8])
abline(h = c(.05, .95), col = "Red")
title(main = "PhaseSig vs Phase")
plot(x = log(SJSPopMat[,3]), y = SJSPopMat[,9])
abline(h = c(.05, .95), col = "Red")
title(main = "StepwiseSig vs Stepwise")

plot(x = SJSPopMat[,10], y = SJSPopMat[,4])
abline(v = c(.05, .95), col = "Red")
title(main = "EucMod vs EucModSig")
plot(x = SJSPopMat[,11], y = SJSPopMat[,5])
abline(v = c(.05, .95), col = "Red")
title(main = "PhaseMod vs PhaseModSig")
plot(x = SJSPopMat[,12], y = SJSPopMat[,6])
abline(v = c(.05, .95), col = "Red")
title(main = "StepwiseMod vs StepwiseModSig")

plot(x = SJSPopMat[,10], y = SJSPopMat[,7])
abline(v = c(.05, .95), h = c(.05, .95), col = "Red")
title(main = "EucSig vs EucModSig")
plot(x = SJSPopMat[,11], y = SJSPopMat[,8])
abline(v = c(.05, .95), h = c(.05, .95), col = "Red")
title(main = "PhaseSig vs PhaseModSig")
plot(x = SJSPopMat[,12], y = SJSPopMat[,9])
abline(v = c(.05, .95), h = c(.05, .95), col = "Red")
title(main = "StepwiseSig vs StepwiseModSig")



par(mfrow=c(2,2))
boxplot(SJCPopMat[,1:3])
title(main = "SJCPopRaw")
boxplot(SJCPopMat[,3+(1:3)])
boxplot(SJCPopMat[,6+(1:3)])
abline(h = c(.05, .95), col = "Red")
boxplot(SJCPopMat[,9+(1:3)])
abline(h = c(.05, .95), col = "Red")

par(mfrow=c(1,1))
plot(x = SJCPopMat[,1], y = SJCPopMat[,2])
title(main = "Phase vs Euclidean")
plot(x = SJCPopMat[,1], y = SJCPopMat[,3])
title(main = "Stepwise vs Euclidean")
plot(x = SJCPopMat[,2], y = SJCPopMat[,3])
title(main = "Stepwise vs Phase")

plot(x = log(SJCPopMat[,1]), y = log(SJCPopMat[,2]))
title(main = "Phase vs Euclidean")
plot(x = log(SJCPopMat[,1]), y = log(SJCPopMat[,3]))
title(main = "Stepwise vs Euclidean")
plot(x = log(SJCPopMat[,2]), y = log(SJCPopMat[,3]))
title(main = "Stepwise vs Phase")

plot(x = SJCPopMat[,1], y = SJCPopMat[,4])
title(main = "EucMod vs Euclidean")
plot(x = SJCPopMat[,2], y = SJCPopMat[,5])
title(main = "PhaseMod vs Phase")
plot(x = SJCPopMat[,3], y = SJCPopMat[,6])
title(main = "StepwiseMod vs Stepwise")

plot(x = log(SJCPopMat[,1]), y = SJCPopMat[,4])
title(main = "EucMod vs Euclidean")
plot(x = log(SJCPopMat[,2]), y = SJCPopMat[,5])
title(main = "PhaseMod vs Phase")
plot(x = log(SJCPopMat[,3]), y = SJCPopMat[,6])
title(main = "StepwiseMod vs Stepwise")

plot(x = SJCPopMat[,4], y = SJCPopMat[,5])
title(main = "PhaseMod vs EucMod")
plot(x = SJCPopMat[,4], y = SJCPopMat[,6])
title(main = "StepwiseMod vs EucMod")
plot(x = SJCPopMat[,5], y = SJCPopMat[,6])
title(main = "StepwiseMod vs PhaseMod")

plot(x = SJCPopMat[,1], y = SJCPopMat[,7])
abline(h = c(.05, .95), col = "Red")
title(main = "EucSig vs Euclidean")
plot(x = SJCPopMat[,2], y = SJCPopMat[,8])
abline(h = c(.05, .95), col = "Red")
title(main = "PhaseSig vs Phase")
plot(x = SJCPopMat[,3], y = SJCPopMat[,9])
abline(h = c(.05, .95), col = "Red")
title(main = "StepwiseSig vs Stepwise")

plot(x = log(SJCPopMat[,1]), y = SJCPopMat[,7])
title(main = "EucSig vs Euclidean")
plot(x = log(SJCPopMat[,2]), y = SJCPopMat[,8])
title(main = "PhaseSig vs Phase")
plot(x = log(SJCPopMat[,3]), y = SJCPopMat[,9])
title(main = "StepwiseSig vs Stepwise")

plot(x = SJCPopMat[,10], y = SJCPopMat[,4])
abline(v = c(.05, .95), col = "Red")
title(main = "EucMod vs EucModSig")
plot(x = SJCPopMat[,11], y = SJCPopMat[,5])
abline(v = c(.05, .95), col = "Red")
title(main = "PhaseMod vs PhaseModSig")
plot(x = SJCPopMat[,12], y = SJCPopMat[,6])
abline(v = c(.05, .95), col = "Red")
title(main = "StepwiseMod vs StepwiseModSig")

plot(x = SJCPopMat[,10], y = SJCPopMat[,7])
abline(v = c(.05, .95), h = c(.05, .95), col = "Red")
title(main = "EucSig vs EucModSig")
plot(x = SJCPopMat[,11], y = SJCPopMat[,8])
abline(v = c(.05, .95), h = c(.05, .95), col = "Red")
title(main = "PhaseSig vs PhaseModSig")
plot(x = SJCPopMat[,12], y = SJCPopMat[,9])
abline(v = c(.05, .95), h = c(.05, .95), col = "Red")
title(main = "StepwiseSig vs StepwiseModSig")

dev.off()


SJSModSub<-SJSPopMat[which(((SJSPopMat[,4] > .1) | (SJSPopMat[,5] > .1)) | (SJSPopMat[,6] > .1)),]

SJCModSub<-SJCPopMat[which(((SJCPopMat[,4] > .1) | (SJCPopMat[,5] > .1)) | (SJCPopMat[,6] > .1)),]

SJSPopRels<-rownames(SJSPopMat)
SJCPopRels<-rownames(SJCPopMat)
SJCDates<-vector(mode = "character", length = length(SJCPopRels))
SJSDates<-vector(mode = "character", length = length(SJSPopRels))
SJSHM<-vector(mode = "character", length = length(SJSPopRels))
for(i in 1:length(SJCDates)){
  Day<-strsplit(SJCPopRels[i], split = "T", fixed = T)[[1]][1]
  DaySplit <- strsplit(Day, split = "-", fixed = T)[[1]]
  SJCDates[i] <- paste(as.numeric(DaySplit[2]), as.numeric(DaySplit[3]), DaySplit[1], sep = "/")
}
for(i in 1:length(SJSDates)){
  Day<-strsplit(SJSPopRels[i], split = " ", fixed = T)[[1]][1]
  if(i < 155){
    DaySplit <- strsplit(Day, split = "-", fixed = T)[[1]]
    SJSDates[i] <- paste(as.numeric(DaySplit[2]), as.numeric(DaySplit[3]), DaySplit[1], sep = "/")
  } else {
    DaySplit <- strsplit(Day, split = "/", fixed = T)[[1]]
    #print(DaySplit[1])
    SJSDates[i] <- paste(as.numeric(DaySplit[1]), as.numeric(DaySplit[2]), paste0(20, DaySplit[3]), sep = "/")
    SJSHM[i] <- strsplit(SJSPopRels[i], split = " ", fixed = T)[[1]][2]
  }
}


SJCPopMat <- cbind(SJCPopMat, as.POSIXct(rownames(SJCPopMat)))

SJSPopMat <- cbind(SJSPopMat, c(as.POSIXct(rownames(SJSPopMat)[1:154]), as.POSIXct(paste(SJSDates[155:248], SJSHM[155:248],sep = " "), format = "%m/%d/%Y %H:%M")))
colnames(SJCPopMat)[17] <- "POSIXct"
colnames(SJSPopMat)[17] <- "POSIXct"
# Switching this over to a new system to make this work easier


SJFlowsRAW <- read.csv("/Users/nathanielcoombs/Downloads/SanJoaquinDelta_Dayflow_1996_2020.csv")
SJFlows <- SJFlowsRAW[,c(3,9)]

SJSFlows <- vector(length = length(SJSDates))
SJCFlows <- vector(length = length(SJCDates))
for(i in 1:length(SJSFlows)){
  Day<-SJSDates[i]
  Key<-which(SJFlows$Date %in% Day)
  #print(Key)
  SJSFlows[i] <- SJFlows$SJR[Key]
}

for(i in 1:length(SJCFlows)){
  Day<-SJCDates[i]
  Key<-which(SJFlows$Date == Day)
  SJCFlows[i] <- SJFlows$SJR[Key]
}
SJSPopMat<-cbind(SJSPopMat,SJSFlows)
SJCPopMat<-cbind(SJCPopMat,SJCFlows)


#####
pdf(file = "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Intermediate stuff/SJSynchMod.pdf")

par(mfrow = c(3,1))

plot(x = SJCPopMat[1:24,13], y = SJCPopMat[1:24,4], ylim = c(0,1), main = "Chinook 2012")
plot(x = SJCPopMat[1:24,13], y = SJCPopMat[1:24,5], ylim = c(0,1))
plot(x = SJCPopMat[1:24,13], y = SJCPopMat[1:24,6], ylim = c(0,1))

plot(x = SJCPopMat[25:35,13], y = SJCPopMat[25:35,4], ylim = c(0,1), main = "Chinook 2014")
plot(x = SJCPopMat[25:35,13], y = SJCPopMat[25:35,5], ylim = c(0,1))
plot(x = SJCPopMat[25:35,13], y = SJCPopMat[25:35,6], ylim = c(0,1))

plot(x = SJCPopMat[36:51,13], y = SJCPopMat[36:51,4], ylim = c(0,1), main = "Chinook 2016")
plot(x = SJCPopMat[36:51,13], y = SJCPopMat[36:51,5], ylim = c(0,1))
plot(x = SJCPopMat[36:51,13], y = SJCPopMat[36:51,6], ylim = c(0,1))

plot(x = SJSPopMat[1:49,13], y = SJSPopMat[1:49,4], ylim = c(0,1), main = "Steelhead 2012")
plot(x = SJSPopMat[1:49,13], y = SJSPopMat[1:49,5], ylim = c(0,1))
plot(x = SJSPopMat[1:49,13], y = SJSPopMat[1:49,6], ylim = c(0,1))

plot(x = SJSPopMat[50:72,13], y = SJSPopMat[50:72,4], ylim = c(0,1), main = "Steelhead 2014")
plot(x = SJSPopMat[50:72,13], y = SJSPopMat[50:72,5], ylim = c(0,1))
plot(x = SJSPopMat[50:72,13], y = SJSPopMat[50:72,6], ylim = c(0,1))

plot(x = SJSPopMat[74:91,13], y = SJSPopMat[74:91,4], ylim = c(0,1), main = "Steelhead 2016")
plot(x = SJSPopMat[74:91,13], y = SJSPopMat[74:91,5], ylim = c(0,1))
plot(x = SJSPopMat[74:91,13], y = SJSPopMat[74:91,6], ylim = c(0,1))

par(mfrow = c(3,2))

plot(x = SJCPopMat[,14], y = SJCPopMat[,4], ylim = c(0,1), main = "Chinook mod vs number fish")

plot(x = SJSPopMat[,14], y = SJSPopMat[,4], ylim = c(0,1), main = "Steelhead mod vs number fish")

plot(x = SJCPopMat[,14], y = SJCPopMat[,5], ylim = c(0,1))

plot(x = SJSPopMat[,14], y = SJSPopMat[,5], ylim = c(0,1))

plot(x = SJCPopMat[,14], y = SJCPopMat[,6], ylim = c(0,1))

plot(x = SJSPopMat[,14], y = SJSPopMat[,6], ylim = c(0,1))

par(mfrow = c(1,1))

dev.off()

colnames(SJCPopMat)

length(which(SJCPopMat[,7] < .05))

length(which(SJSPopMat[,7] < .05))

nrow(SJCPopMat)
nrow(SJSPopMat)

length(which(SJCPopMat[,10] < .05))

length(which(SJSPopMat[,10] < .05))

binom.test(2,120,.05, alternative = "greater")
binom.test(3,248,.05, alternative = "greater")

hist(SJCPopMat[,7], 20)
hist(SJSPopMat[,7], 20)
hist(SJCPopMat[,10], 20)
hist(SJSPopMat[,10], 20)

#####
pdf(file = "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Intermediate stuff/SJClustVsBiom.pdf")
par(mfrow = c(2,2))

plot(SJCPopMat[,13], SJCPopMat[,14])
title(main = "Chin length Var vs Mean")
plot(SJSPopMat[,13], SJSPopMat[,14])
title(main = "Steel length Var vs Mean")
plot(SJCPopMat[,15], SJCPopMat[,16])
title(main = "Chin weight Var vs Mean")
plot(SJSPopMat[,15], SJSPopMat[,16])
title(main = "Steel weight Var vs Mean")

plot(SJCPopMat[,13], SJCPopMat[,15])
title(main = "Chin mean length vs weight")
plot(SJSPopMat[,13], SJSPopMat[,15])
title(main = "Steel mean length vs weight")
plot(SJCPopMat[,14], SJCPopMat[,16])
title(main = "Chin var length vs weight")
plot(SJSPopMat[,14], SJSPopMat[,16])
title(main = "Steel var length vs weight")

# # # And now against sig clustering

plot(SJCPopMat[,13], SJCPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Euc vs length mean")
plot(SJCPopMat[,14], SJCPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Euc vs length var")
plot(SJCPopMat[,15], SJCPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Euc vs weight mean")
plot(SJCPopMat[,16], SJCPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Euc vs weight var")

plot(SJSPopMat[,13], SJSPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Euc vs length mean")
plot(SJSPopMat[,14], SJSPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Euc vs length var")
plot(SJSPopMat[,15], SJSPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Euc vs weight mean")
plot(SJSPopMat[,16], SJSPopMat[,10], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Euc vs weight var")



plot(SJCPopMat[,13], SJCPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Phase vs length mean")
plot(SJCPopMat[,14], SJCPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Phase vs length var")
plot(SJCPopMat[,15], SJCPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Phase vs weight mean")
plot(SJCPopMat[,16], SJCPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Phase vs weight var")

plot(SJSPopMat[,13], SJSPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Phase vs length mean")
plot(SJSPopMat[,14], SJSPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Phase vs length var")
plot(SJSPopMat[,15], SJSPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Phase vs weight mean")
plot(SJSPopMat[,16], SJSPopMat[,11], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Phase vs weight var")

plot(SJCPopMat[,13], SJCPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Step vs length mean")
plot(SJCPopMat[,14], SJCPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Step vs length var")
plot(SJCPopMat[,15], SJCPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Step vs weight mean")
plot(SJCPopMat[,16], SJCPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Chin Sig Step vs weight var")

plot(SJSPopMat[,13], SJSPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Step vs length mean")
plot(SJSPopMat[,14], SJSPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Step vs length var")
plot(SJSPopMat[,15], SJSPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Step vs weight mean")
plot(SJSPopMat[,16], SJSPopMat[,12], ylim = c(0,1))
abline(h = .05, col = "red")
title(main = "Steel Sig Step vs weight var")

par(mfrow = c(1,1))
dev.off()
#####
pdf(file = "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Intermediate stuff/SJClustVsFlows.pdf")
plot(x = SJCPopMat[,18], SJCPopMat[,10], main = "Chin - Euc sig vs flow", ylim = c(0,1))
abline(h = .05, col = "Red")

plot(x = SJCPopMat[,18], SJCPopMat[,11], main = "Chin - Phase sig vs flow", ylim = c(0,1))
abline(h = .05, col = "Red")

plot(x = SJCPopMat[,18], SJCPopMat[,12], main = "Chin - Step sig vs flow", ylim = c(0,1))
abline(h = .05, col = "Red")


plot(x = SJSPopMat[,18], SJSPopMat[,10], main = "Steel - Euc sig vs flow", ylim = c(0,1))
abline(h = .05, col = "Red")

plot(x = SJSPopMat[,18], SJSPopMat[,11], main = "Steel - Phase sig vs flow", ylim = c(0,1))
abline(h = .05, col = "Red")

plot(x = SJSPopMat[,18], SJSPopMat[,12], main = "Steel - Step sig vs flow", ylim = c(0,1))
abline(h = .05, col = "Red")


dev.off()


#####
# Logistic regression on flows
SJSPopMat<-cbind(SJSPopMat,(SJSPopMat[,10] < .05))
colnames(SJSPopMat)[19] <- "SigLog"

SJCPopMat<-cbind(SJCPopMat,(SJCPopMat[,10] < .05))
colnames(SJCPopMat)[19] <- "SigLog"

SFlowLog = glm(formula = SigLog ~ SJSFlows, data = as.data.frame(SJSPopMat), family = binomial)

CFlowLog = glm(formula = SigLog ~ SJCFlows, data = as.data.frame(SJCPopMat), family = binomial)

print(summary(SFlowLog))
print(summary(CFlowLog))
# Let's try beta Reg
library("betareg")
SFlowBet = betareg(PModLD ~ SJSFlows, data = as.data.frame(SJSPopMat))

CFlowBet = betareg(formula = PModLD ~ SJCFlows, data = as.data.frame(SJCPopMat))

print(summary(SFlowBet))
print(summary(CFlowBet))
# Let's do a quick t test on our splits for biometrics vs clustering
library(fitdistrplus)
library(MASS)
FlowMin <- min(c(SJCPopMat[,18],SJSPopMat[,18]))
FlowMax <- max(c(SJCPopMat[,18],SJSPopMat[,18]))
FlowBreaks <- seq(FlowMin, FlowMax, length.out = 6)
hist(SJCPopMat[,18], breaks = FlowBreaks)
hist(SJSPopMat[,18], breaks = FlowBreaks)

binom.test(3,51,.05, alternative = "greater")
binom.test(3,97,.05, alternative = "greater")

hist(SJCPopMat[,18], breaks = FlowBreaks)
hist(SJSPopMat[,18], breaks = FlowBreaks)

length(which(SJSPopMat[,7] < .05))

hist(SJCPopMat[,10], breaks = 20)
hist(SJSPopMat[,10], breaks = 20)

# # # Section for looking at old version of results
OutFileSetPrev <- list.files("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Data/SJReduced/Outputs/Pops/")
OutSetPrev <- vector(mode = "character", length = length(OutFileSetPrev))
for(i in 1:length(OutFileSetPrev)){
  OutSetPrev[i] <- strsplit(OutFileSetPrev[i], ".", fixed = T)[[1]][1]
}

PrevAllPopSig<-vector(mode = "list", length = length(OutSetPrev))
PrevAllPairSig<-vector(mode = "list", length = length(OutSetPrev))

SJCErr<-vector(mode = "numeric")
for(i in 1:length(PrevAllPopSig)){
  PrevAllPopSig[[i]] <- readRDS(paste("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Data/SJReduced/Outputs/Pops/", OutSetPrev[i], ".rds", sep = ""))
  PrevAllPairSig[[i]] <- read.csv(paste("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Data/SJReduced/Outputs/Pairs/", OutSetPrev[i], ".csv", sep = ""))
}

length(PrevAllPopSig)

PrevAllPairSig[[151]][,4:5]
# 151 is the breakpoint
PrevSJCPopSig <- vector(mode = "list", length = 151)
PrevSJSPopSig <- vector(mode = "list", length = length(PrevAllPopSig) - 151)
for(i in 1:length(PrevAllPopSig)){
  if(i < 152){
    PrevSJCPopSig[[i]] <- PrevAllPopSig[[i]]
  } else {
    PrevSJSPopSig[[i - 151]] <- PrevAllPopSig[[i]]
  }
}
PrevSJSPopMat <- matrix(data = NA, nrow = length(PrevSJSPopSig), ncol = 12)
colnames(PrevSJSPopMat) <- c("MeanLD", "MeanPS", "MeanLS", "ModLD", "ModPS", "ModLS", "PMeanLD", "PMeanPS", "PMeanLS", "PModLD", "PModPS", "PModLS")

PrevSJCPopMat <- matrix(data = NA, nrow = length(PrevSJCPopSig), ncol = 12)
colnames(PrevSJCPopMat) <- c("MeanLD", "MeanPS", "MeanLS", "ModLD", "ModPS", "ModLS", "PMeanLD", "PMeanPS", "PMeanLS", "PModLD", "PModPS", "PModLS")


# # # Try refreshing the data and checking for NA's

for(i in 1:length(PrevSJSPopSig)){
  #print(i)
  PopSigTemp <- PrevSJSPopSig[[i]]
  if(length(PopSigTemp) == 0){
    next
  }
  PrevSJSPopMat[i,1] <- 1/PopSigTemp[[1]][1,1]
  PrevSJSPopMat[i,2] <- 1/PopSigTemp[[1]][1,2]
  PrevSJSPopMat[i,3] <- 1/PopSigTemp[[1]][1,3]
  
  PrevSJSPopMat[i,4] <- PopSigTemp[[1]][2,1]
  PrevSJSPopMat[i,5] <- PopSigTemp[[1]][2,2]
  PrevSJSPopMat[i,6] <- PopSigTemp[[1]][2,3]
  
  
  
  PrevSJSPopMat[i,7] <- PopSigTemp[[2]][1,1]/1000
  PrevSJSPopMat[i,8] <- PopSigTemp[[2]][1,2]/1000
  PrevSJSPopMat[i,9] <- PopSigTemp[[2]][1,3]/1000
  
  PrevSJSPopMat[i,10] <- 1-(PopSigTemp[[2]][2,1]/1000)
  PrevSJSPopMat[i,11] <- 1-(PopSigTemp[[2]][2,2]/1000)
  PrevSJSPopMat[i,12] <- 1-(PopSigTemp[[2]][2,3]/1000)
}


for(i in 1:length(PrevSJCPopSig)){
  #print(i)
  PopSigTemp <- PrevSJCPopSig[[i]]
  if(length(PopSigTemp) == 0){
    #print(i)
    next
  }
  PrevSJCPopMat[i,1] <- 1/PopSigTemp[[1]][1,1]
  PrevSJCPopMat[i,2] <- 1/PopSigTemp[[1]][1,2]
  PrevSJCPopMat[i,3] <- 1/PopSigTemp[[1]][1,3]
  
  PrevSJCPopMat[i,4] <- PopSigTemp[[1]][2,1]
  PrevSJCPopMat[i,5] <- PopSigTemp[[1]][2,2]
  PrevSJCPopMat[i,6] <- PopSigTemp[[1]][2,3]
  
  
  
  PrevSJCPopMat[i,7] <- PopSigTemp[[2]][1,1]/1000
  PrevSJCPopMat[i,8] <- PopSigTemp[[2]][1,2]/1000
  PrevSJCPopMat[i,9] <- PopSigTemp[[2]][1,3]/1000
  
  PrevSJCPopMat[i,10] <- 1-(PopSigTemp[[2]][2,1]/1000)
  PrevSJCPopMat[i,11] <- 1-(PopSigTemp[[2]][2,2]/1000)
  PrevSJCPopMat[i,12] <- 1-(PopSigTemp[[2]][2,3]/1000)
}
PrevSJCPopMat<-na.omit(PrevSJCPopMat)
PrevSJSPopMat<-na.omit(PrevSJSPopMat)

nrow(PrevSJCPopMat)
nrow(PrevSJSPopMat)
length(which(PrevSJCPopMat[,10] < .05))
length(which(PrevSJSPopMat[,10] < .05))

binom.test(14,151,.05, alternative = "greater")
binom.test(72,433,.05, alternative = "greater")

hist(PrevSJCPopMat[,7], 20)
hist(PrevSJSPopMat[,7], 20)
hist(PrevSJCPopMat[,10], 20)
hist(PrevSJSPopMat[,10], 20)

# DropPairSig<-vector(mode = "numeric")
# for(i in 1:length(PrevAllPairSig)){
#   Temp<-PrevAllPairSig[[i]]
#   if(length(unique(Temp[,4])) < 4){
#     DropPairSig<-c(DropPairSig,i)
#   }
# }
# DropPairSig
# DropPairSig<-
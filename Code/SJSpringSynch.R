# Script for setting up synchrony calculations for spring run chinook in the San Joaquin
# Read in raw data
SJSpringRaw<-read.csv("./Data/SJSpring/Chinook_Juvenile_SpringRun_SanJoaquin_20210802 2.csv")
SJSpring17Clean<-read.csv("./Data/SJSpring/sj_reboot2 (2).csv",stringsAsFactors = F)


SpringFishByRelRaw<-unique(cbind(SJSpring17Clean$TagID,SJSpring17Clean$ReleaseGroup))
RelTimesRaw<-unique(cbind(SJSpring17Clean$ReleaseGroup,SJSpring17Clean$ReleaseDate))

# Check included release events & recode if needed
colnames(SJSpringRaw)
RelNames<-unique(SJSpringRaw$Release_group)
RelNames
RelTimes<-unique(cbind(SJSpringRaw$Release_group,SJSpringRaw$Date_tagged))
RelTimes                 
# A note on stupid, stupid nomenclature: 
#   While no release names are reeused across years, some get very close.
#   As we have four releases (as far as I'm aware) and two hatcheries for the two in 2017, I'll be using six standardized release names and then listing which groups correspond to which.
#   There will be a total of eight datasets processed for synchrony, however, as I will be investigating how well patterns hold across releases of fish from the same vs different hatcheries.
#   17UpsCA <- SJ SCARF1, 2017_SJ_SCARF1
#   17UpsI <- 2017_SJ_SIRF1, SJ SIRF1
#   17DownCA <- 2017_SJ_SCARF2
#   17DownI <- 2017_SJ_SIRF2
#   18UpsCA <- SJScarf1, SJSCARF1, SJSCARF1_TE
#   18DownCA <- SJSCARF2
RelMatch<-matrix(nrow = length(RelNames), ncol = 2)
RelMatch[,1] <- RelNames
RelMatch[1:2,2] <- "17UpsCA"
RelMatch[3:4,2] <- "17UpsI"
RelMatch[5,2] <- "17DownCA"
RelMatch[6,2] <- "17DownI"
RelMatch[c(7,9:10),2] <- "18UpsCA"
RelMatch[8,2] <- "18DownCA"

for(i in 1:nrow(RelMatch)){
  RelDex<-which(SJSpringRaw$Release_group == RelMatch[i,1])
  if(length(RelDex) > 0){
    SJSpringRaw$Release_group[RelDex] <- RelMatch[i,2]
  }
  RelDex<-which(SJSpring17Clean$ReleaseGroup == RelMatch[i,1])
  if(length(RelDex) > 0){
    SJSpring17Clean$ReleaseGroup[RelDex] <- RelMatch[i,2]
  }
}

SJSpring18<-SJSpringRaw[which(SJSpringRaw$Release_group %in% c("18UpsCA", "18DownCA")),]

# With releases now sensibly renamed/regrouped, I now need to figure out how to group locations.

#SJSpring18Clean<-SJSpring18[-which(SJSpring18$GPS_names == ""),]

#Locs17 <- unique(cbind(SJSpring17Clean$General.Location, SJSpring17Clean$Lat, SJSpring17Clean$Lon))
                 
#Locs18 <- unique(cbind(SJSpring18Clean$GPS_names, SJSpring18Clean$General_latitude, SJSpring18Clean$General_longitude))

#SpringLocs <- rbind(Locs17,Locs18)

#write.csv(SpringLocs, "./Data/SJSpring/LocsOut.csv")
AggSpringLocs<-read.csv("./Data/SJSpring/AggLocs.csv")

for(i in 1:nrow(AggSpringLocs)){
  # RelDex<-which(SJSpring18Clean$GPS_names == AggSpringLocs[i,2])
  # if(length(RelDex) > 0){
  #   SJSpring18Clean$GPS_names[RelDex] <- AggSpringLocs[i,1]
  # }
  RelDex<-which(SJSpring17Clean$General.Location == AggSpringLocs[i,2])
  if(length(RelDex) > 0){
    SJSpring17Clean$General.Location[RelDex] <- AggSpringLocs[i,1]
  }
}

# Alrighty, now we can cook a bit. Drop all detections without a Lat/Long
# SJSpring18Clean <- SJSpring18Clean[-which(is.na(SJSpring18Clean$General_latitude)),]
# SJSPring17Clean has none so no drop needed

# Next, add a vector for time how we want it
# 
# SJSpring18Clean$Detect_date_time[1000:2000]
# unique(SJSpring18Clean$Detect_date_time)
# Oh okay so 2018 just straight up does NOT work at all. Cool cool cool.

SJSpring17Clean$time<-as.POSIXct(SJSpring17Clean$dtf, tz = "US/Pacific" , format = "%m/%d/%Y %H:%M")

# Need to add a "release detection" for each fish and then condense like the dickens
SpringFishByRel<-unique(cbind(SJSpring17Clean$TagID,SJSpring17Clean$ReleaseGroup))
RelTimes<-unique(cbind(SJSpring17Clean$ReleaseGroup,SJSpring17Clean$ReleaseDate))

RelDummy<-SJSpring17Clean[1:nrow(SpringFishByRel),]
RelDummy$General.Location<-"Rel"
for(i in 1:nrow(SpringFishByRel)){
  RelDummy$TagID[i] <- SpringFishByRel[i,1]
  RelStart <- as.POSIXct(RelTimes[which(RelTimes[,1] == SpringFishByRel[i,2]),2], tz = "US/Pacific" , format = "%m/%d/%Y %H:%M")
  RelDummy$time[i] <- RelStart
  
  FishKey<-which(SJSpring17Clean$TagID == SpringFishByRel[i,1])
  DropTimes<-which(as.numeric(SJSpring17Clean$time[FishKey]) < as.numeric(RelStart))
  if(length(DropTimes) > 0){
    SJSpring17Clean<-SJSpring17Clean[-FishKey[DropTimes],]
  }
}

SJSpring17Clean<-rbind(SJSpring17Clean,RelDummy)

which(is.na(SJSpring17Clean$time))
table(SJSpring17Clean$TagID)

#SJSpring17Clean$time <- as.numeric(SJSpring17Clean$time)

DetCondenseMod<-function(DetFrame, FishID, LocCol, tCol, IDCol){
  if(!any(LocCol %in% colnames(DetFrame))){
    stop("Error in DetCondense: Location column selected not found in detection data frame")
  }
  if(!any(tCol %in% colnames(DetFrame))){
    stop("Error in DetCondense: Time column selected not found in detection data frame")
  }
  if(!any(IDCol %in% colnames(DetFrame))){
    stop("Error in DetCondense: Fish ID column selected not found in detection data frame")
  }
  if(!any(FishID %in% DetFrame[,IDCol])){
    stop("Error in DetCondense: FishID selected not found in detection data frame")
  }
  # if(any(is.na(as.numeric(DetFrame[,tCol])))){
  #   stop("Error in DetCondense: Times must be numeric")
  # }
  SubFrame<-DetFrame[which(!any(is.na(c(tCol,LocCol)))),which(IDCol %in% FishID)]
  DenseFrame<-data.frame("FishID" = NA, "Loc" = NA, "FirstDet" = NA, "LastDet" = NA, "NumDets" = NA)
  for(i in 1:length(FishID)){
    print(i)
    print(FishID[i])
    FullFishFrame<-DetFrame[which(DetFrame[,IDCol] == FishID[i]),c(IDCol,LocCol,tCol)]
    FullFishFrame<-FullFishFrame[which(complete.cases(FullFishFrame)),]
    OrdFishFrame<-FullFishFrame[order(as.numeric(FullFishFrame[,tCol])),]
    #print("Frame pulled")
    if(length(unique(OrdFishFrame[,LocCol])) > 1){
      
      if(dim(OrdFishFrame)[1] > 2){
        FirstDetVec<-vector(mode = "list")
        for(j in 2:length(OrdFishFrame[,tCol])){
          FirstDetVec[j] <- OrdFishFrame[j,LocCol] == OrdFishFrame[j-1,LocCol]
        }
        FirstDetInd<-vector(mode = "list")
        FirstDetInd<-c(1,which(FirstDetVec %in% FALSE))
        
        
        LastDetVec<-vector(mode = "list")
        for(j in 1:(length(OrdFishFrame[,tCol])-1)){
          LastDetVec[j]<-OrdFishFrame[j,LocCol] == OrdFishFrame[j+1,LocCol]
        }
        LastDetInd<-vector(mode = "list")
        LastDetInd<-c(which(LastDetVec %in% FALSE),length(OrdFishFrame[,tCol]))
        
        NumDets<-matrix()
        for(j in 1:length(FirstDetInd)){
          NumDets[j]<-length(FirstDetInd[j]:LastDetInd[j])
        }}
      else{
        FirstDetInd<-vector(mode = "numeric")
        FirstDetInd[1]<-1
        
        LastDetInd<-vector(mode = "numeric")
        LastDetInd[1]<-1
        
        NumDets<-matrix()
        NumDets[1]<-1
        NumDets[2]<-1
        
        FirstDetInd[2]<-2
        
        LastDetInd[2]<-2
      }
      #print("Forming dataframe")
      FishFrame<-data.frame()
      FishFrame[1:length(FirstDetInd),"FishID"]<-FishID[i]
      #print("Names added")
      #print(OrdFishFrame)
      FishFrame[1:length(FirstDetInd),"Loc"]<-OrdFishFrame[FirstDetInd,LocCol]
      #print("Locs added")
      FishFrame[1:length(FirstDetInd),"FirstDet"]<-OrdFishFrame[FirstDetInd,tCol]
      #print("FirstDets added")
      FishFrame[1:length(FirstDetInd),"LastDet"]<-OrdFishFrame[LastDetInd,tCol]
      #print("Lastdets added")
      FishFrame[1:length(FirstDetInd),"NumDets"]<-NumDets
      #print("Num dets added")
      DenseFrame<-rbind(FishFrame,DenseFrame)
    }
  }
  return(DenseFrame)
}

SJSpring17Dense <- DetCondenseMod(SJSpring17Clean,unique(SJSpring17Clean$TagID),"General.Location","time","TagID")


# Okay, now to subset for only fish which have at least N detection events

NDetsTable<-table(SJSpring17Dense$FishID)

length(which(NDetsTable >= 5))

# Let's go with five for now.

SpringFishSanityCheck<-SpringFishByRel
SpringFishSanityCheck[which(SpringFishSanityCheck[,1] %in% c("17UpsCA", "17UpsI")),2] <- "17UpsAll"
SpringFishSanityCheck[which(SpringFishSanityCheck[,1] %in% c("17UpsCA", "17UpsI")),2] <- "17UpsAll"

table(table(SpringFishByRel[,1]))
table(SpringFishByRel[,2])






NDMat<-matrix(nrow = length(NDetsTable), ncol = 2)
NDMat[,1]<-names(NDetsTable)
NDMat[,2]<-NDetsTable

SpringDets<-vector(mode = "list", length = 6)
names(SpringDets)<-c("17UpsCA","17UpsI","17DownCA","17DownI","17UpsAll","17DownAll")
SpringPairs<-SpringDets
SpringFish<-SpringPairs
for(i in 1:4){
  WorkingMat <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(WorkingMat) <- colnames(SJSpring17Dense)
  print(which(SpringFishByRel[,2] == names(SpringDets)[i]))
  SubFish<-SpringFishByRel[which(SpringFishByRel[,2] == names(SpringDets)[i]),1]
  print(SubFish)
  for(j in 1:length(SubFish)){
    if(length(which(NDMat[,1] == SubFish[j])) > 0){
      #print("Works")
    if(as.numeric(NDMat[which(NDMat[,1] == SubFish[j]),2]) >= 10){
      WorkingMat <- rbind(WorkingMat, SJSpring17Dense[which(SJSpring17Dense$FishID == SubFish[j]),])
    }}
  }
  SpringDets[[i]]<-WorkingMat
}
SpringDets[[5]]<-rbind(SpringDets[[1]],SpringDets[[2]])
SpringDets[[6]]<-rbind(SpringDets[[3]],SpringDets[[4]])

for(i in 1:6){
  SpringFish[[i]]<-unique(SpringDets[[i]]$FishID)
  SpringPairs[[i]]<-TestPairsGen(SpringDets[[i]])
  print(length(SpringFish[[i]]))
}

for(i in 1:length(SpringDets)){
  write.csv(SpringPairs[[i]], file = paste0("./Data/SJSpring/Data/Pairs/", i, ".csv"), row.names=FALSE)
  write.csv(SpringDets[[i]], file = paste0("./Data/SJSpring/Data/Dets/", i, ".csv"), row.names=FALSE)
}

TestRead <- read.csv("./Data/SJSpring/Data/Dets/1.csv")

SpringMTable<-MarkovFishTable(SpringDets[[1]], "FishID","Loc","FirstDet","LastDet")
MarkovFishSurrogates(SpringMTable, "Rel",0,10,Reps = 1)[[1]]

RCTCalc<-function(DenseTable, PairSet, PairDex){
  if(length(PairDex) == 1){
    RCTest<-LCSCalc(DenseTable,t(as.matrix(PairSet[PairDex,])))
  } else {
    RCTest<-LCSCalc(DenseTable,PairSet[PairDex,])
  }
  return(RCTest)
}

Rcpp::sourceCpp(file = "./Code/Functions/FullLCSExtractor.cpp")

RCTestS2 <- RCTCalc(SpringDets[[2]], SpringPairs[[2]], 1)

TempLCS<-LCSCalc(SpringDets[[2]], SpringPairs[[2]])

RCTPrint <- function(DenseTable, PairSet, PairDex){
  Key1 <- which(DenseTable[,"FishID"] %in% PairSet[PairDex,1])
  Key2 <- which(DenseTable[,"FishID"] %in% PairSet[PairDex,2])
  Out <- rbind(DenseTable[Key1,], DenseTable[Key2,])
  return(Out)
}

RCTPrint(SpringDets[[2]], SpringPairs[[2]], 1)
RCTPrint(SpringDets[[6]], SpringPairs[[6]], 299)



SJSPairSig <- vector(mode = "list", length = 6)
names(SJSPairSig) <- names(SpringDets)
SJSPopSig <- SJSPairSig

for(i in 1:length(SJSPairSig)){
  SJSPopSig[[i]] <-  readRDS(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/SJSpring/Outputs/Pops/", i, ".rds", sep = ""))
  SJSPairSig[[i]] <-  read.csv(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/SJSpring/Outputs/Pairs/", i, ".csv", sep = ""))
}

SJSCompMat <- matrix(data = NA, nrow = length(SJSPairSig), ncol = 12)
colnames(SJSCompMat) <- c("MeanLD", "MeanPS", "MeanLS", "ModLD", "ModPS", "ModLS", "PMeanLD", "PMeanPS", "PMeanLS", "PModLD", "PModPS", "PModLS")

rownames(SJSCompMat) <- names(SJSPairSig)
for(i in 1:length(SJSPopSig)){
  #print(i)
  PopSigTemp <- SJSPopSig[[i]]
  if(length(PopSigTemp) == 0){
    next
  }
  SJSCompMat[i,1] <- 1/PopSigTemp[[1]][1,1]
  SJSCompMat[i,2] <- 1/PopSigTemp[[1]][1,2]
  SJSCompMat[i,3] <- 1/PopSigTemp[[1]][1,3]
  
  SJSCompMat[i,4] <- PopSigTemp[[1]][2,1]
  SJSCompMat[i,5] <- PopSigTemp[[1]][2,2]
  SJSCompMat[i,6] <- PopSigTemp[[1]][2,3]
  
  
  
  SJSCompMat[i,7] <- 1-PopSigTemp[[2]][1,1]/1000
  SJSCompMat[i,8] <- 1-PopSigTemp[[2]][1,2]/1000
  SJSCompMat[i,9] <- 1-PopSigTemp[[2]][1,3]/1000
  
  SJSCompMat[i,10] <- 1-(PopSigTemp[[2]][2,1]/1000)
  SJSCompMat[i,11] <- 1-(PopSigTemp[[2]][2,2]/1000)
  SJSCompMat[i,12] <- 1-(PopSigTemp[[2]][2,3]/1000)
  
}

write.csv(SJSCompMat, "./Results/SpringRunChinookComp.csv")

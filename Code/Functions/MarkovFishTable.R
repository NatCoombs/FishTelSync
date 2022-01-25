# function MarkovFishTable

# # # # #

# Description: Takes a DenseTable object and extracts two lists: one showing the next 
# location and time to get there and one showing the hold.

# MarkovTest<-MarkovFishTable(FWS07LocDense, "FishID", "Loc", "FirstDet", "LastDet")

# # # Args: 
# DTable
# IDcol
# Loccol
# Arrs
# Deps

##### Function proper:
MarkovFishTable <- function(DTable, IDcol, Loccol, Arrs, Deps){
  MFTOut <- list()
  LocSet <- unique(DTable[,Loccol])[which(!is.na(unique(DTable[,Loccol])))]
  FishSet <- unique(DTable[,IDcol])[which(!is.na(unique(DTable[,IDcol])))]
  #print(LocSet)
  #print(FishSet)
  for(i in LocSet){
    MFTabSub <- data.frame()
    LocSub <- DTable[which(DTable[, Loccol] %in% i), ]
    HoldVec <- vector(mode = "numeric")
    NextVec <- vector(mode = "character")
    StepVec <- vector(mode = "numeric")
    for(j in FishSet){
      EventSub <- LocSub[which(LocSub[, IDcol] %in% j), ]
      #print(EventSub)
      FishSub <- DTable[which(DTable[, IDcol] %in% j), ]
      FishSub <- FishSub[order(FishSub[,Arrs]),]
      #print(FishSub)
      if(length(EventSub[,1]) != 0){
        for(k in 1:length(EventSub[,1])){
          HoldVec <- append(HoldVec, (EventSub[k,Deps]-EventSub[k,Arrs]))
          
          #print((EventSub[k,Deps]-EventSub[k,Arrs]))
          EvKey <- which(FishSub[,Arrs] %in% EventSub[k,Arrs])
          if(is.na(FishSub[EvKey + 1, Loccol])){
          NextVec <- append(NextVec, "END")
          StepVec <- append(StepVec, 0)
          } else {
            NextVec <- append(NextVec, FishSub[EvKey + 1, Loccol])
            StepVec <- append(StepVec, FishSub[EvKey + 1, Arrs] - FishSub[EvKey, Deps])
          }
          

        }
      }
      
    }
    for(j in 1:length(HoldVec)){
      MFTabSub[j,1] <- HoldVec[j]
      MFTabSub[j,2] <- NextVec[j]
      MFTabSub[j,3] <- StepVec[j]
    }
    
    colnames(MFTabSub) <- c("Holds", "NextLoc", "StepTime")

    
    # MFTabSub$Holds <- HoldVec
    # MFTabSub$NextLoc <- NextVec
    # MFTabSub$StepTime <- StepVec
    MFTOut[[match(i, LocSet)]] <- MFTabSub
  }
  names(MFTOut) <- LocSet
  return(MFTOut)
}

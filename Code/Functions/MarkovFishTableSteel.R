# function MarkovFishTableSteel

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
MarkovFishTableSteel <- function(DTable, IDcol, Loccol, Arrs, Deps, bFlag){
  MFTOut <- list()
  MFTOut2 <- list()
  LocSet <- unique(DTable[,Loccol])[which(!is.na(unique(DTable[,Loccol])))]
  FishSet <- unique(DTable[,IDcol])[which(!is.na(unique(DTable[,IDcol])))]
  #print(LocSet)
  #print(FishSet)
  BreakSet <- vector(mode = "logical", length = length(FishSet))
  for(i in 1:length(FishSet)){
    #print(match(bFlag, DTable[which(DTable[,IDcol] %in% FishSet[i]),Loccol]))
    BreakSet[i] <- match(bFlag, DTable[which(DTable[,IDcol] %in% FishSet[i]),Loccol])
  }
  #print(BreakSet)
  for(i in LocSet){
    MFTabSub <- data.frame()
    MFTabSubAft <- data.frame()
    LocSub <- DTable[which(DTable[, Loccol] %in% i), ]
    HoldVec <- vector(mode = "numeric")
    NextVec <- vector(mode = "character")
    StepVec <- vector(mode = "numeric")
    
    HoldVec2 <- vector(mode = "numeric")
    NextVec2 <- vector(mode = "character")
    StepVec2 <- vector(mode = "numeric")
    
    for(j in 1:length(FishSet)){
      #print(j)
      EventSub <- LocSub[which(LocSub[, IDcol] %in% FishSet[j]), ]
      #print(EventSub)
      FishSub <- DTable[which(DTable[, IDcol] %in% FishSet[j]), ]
      FishSub <- FishSub[order(FishSub[,Arrs]),]
      #print(FishSub)
      if(length(EventSub[,1]) != 0){
        for(k in 1:length(EventSub[,1])){
          EvKey <- which(FishSub[,Arrs] %in% EventSub[k,Arrs])
          
          if(!is.na(BreakSet[j])){
            if(EvKey >= BreakSet[j]){
              HoldVec2 <- append(HoldVec2, (EventSub[k,Deps]-EventSub[k,Arrs]))
              
              #print((EventSub[k,Deps]-EventSub[k,Arrs]))
              
              if(is.na(FishSub[EvKey + 1, Loccol])){
                NextVec2 <- append(NextVec2, "END")
                StepVec2 <- append(StepVec2, 0)
              } else {
                NextVec2 <- append(NextVec2, FishSub[EvKey + 1, Loccol])
                StepVec2 <- append(StepVec2, FishSub[EvKey + 1, Arrs] - FishSub[EvKey, Deps])
              }
            } else {
          HoldVec <- append(HoldVec, (EventSub[k,Deps]-EventSub[k,Arrs]))
          
          #print((EventSub[k,Deps]-EventSub[k,Arrs]))

          if(is.na(FishSub[EvKey + 1, Loccol])){
            NextVec <- append(NextVec, "END")
            StepVec <- append(StepVec, 0)
          } else {
            NextVec <- append(NextVec, FishSub[EvKey + 1, Loccol])
            StepVec <- append(StepVec, FishSub[EvKey + 1, Arrs] - FishSub[EvKey, Deps])
          }
          
            }
          } else {
            HoldVec <- append(HoldVec, (EventSub[k,Deps]-EventSub[k,Arrs]))
          
          #print((EventSub[k,Deps]-EventSub[k,Arrs]))

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
      
    }
    
    for(j in 1:length(HoldVec)){
      MFTabSub[j,1] <- HoldVec[j]
      MFTabSub[j,2] <- NextVec[j]
      MFTabSub[j,3] <- StepVec[j]
    }
    
    for(j in 1:length(HoldVec2)){
      MFTabSubAft[j,1] <- HoldVec2[j]
      MFTabSubAft[j,2] <- NextVec2[j]
      MFTabSubAft[j,3] <- StepVec2[j]
    }
    colnames(MFTabSub) <- c("Holds", "NextLoc", "StepTime")
    colnames(MFTabSubAft) <- c("Holds", "NextLoc", "StepTime")
    
    
    # MFTabSub$Holds <- HoldVec
    # MFTabSub$NextLoc <- NextVec
    # MFTabSub$StepTime <- StepVec
    MFTOut[[match(i, LocSet)]] <- MFTabSub
    MFTOut2[[match(i, LocSet)]] <- MFTabSubAft
    
  }
  names(MFTOut) <- LocSet
  names(MFTOut2) <- LocSet
  MFTOutTrue <- list()
  MFTOutTrue[[1]] <- MFTOut
  MFTOutTrue[[2]] <- MFTOut2
  names(MFTOutTrue) <- c("Before Ocean", "After Ocean")
  return(MFTOutTrue)
}

# # # function MarkovFishSteel


# Description: Takes a Markov Fish Table and generates surrogate fish based on the number of 
# fish and reps given. Returns the output as a list of dataframes, with each dataframe representing a run.
# MarkovTest2<-MarkovFishSurrogates(MarkovTest, "Battle_Ck", 0, 27, 100)

# Args:
# MFTable
# InitPos
# InitTime
# nFish
# Reps
MarkovFishSurrogatesSteel<-function(MFTable, InitPos, InitTime, nFish, Reps, fBreak,
                               Method = "SingleRelease", Seed = 0){
  
  Pdex <- 1
  set.seed(Seed)
  MFSOut<-list()
  for(i in 1:Reps){
    SurrDense <- data.frame()
    FishVec <- vector(mode = "character")
    LocVec <- vector(mode = "character")
    ArrVec <- vector(mode = "numeric")
    DepVec <- vector(mode = "numeric")
    #print(Method)
    #print(Method == "SingleRelease")
    if(Method == "SingleRelease"){
      
      for(j in 1:nFish){
        FishVec <- append(FishVec, j)
        LocVec <- append(LocVec, InitPos)
        ArrVec <- append(ArrVec, InitTime)
        
        flag <- "START"
        
        while(flag == "START"){
          
          # print(LocVec[length(LocVec)])
          # print(names(MFTable))
          # print(match(LocVec[length(LocVec)], names(MFTable)))
          #print(LocVec[length(LocVec)])
          #print(match(LocVec[length(LocVec)], names(MFTable[[1]])))
          MFSub <- MFTable[[1]][[match(LocVec[length(LocVec)], names(MFTable[[1]]))]]
          
          DepVec <- append(DepVec, (ArrVec[length(ArrVec)] + sample(MFSub$Holds)))
          
          StepKey <- sample(1:length(MFSub[,1]), 1)
          #print(MFSub)
          #print(StepKey)
          if(MFSub$NextLoc[StepKey] == "END"){
            flag <- "END"
            break()
          } else {
            
          if(MFSub$NextLoc[StepKey] == fBreak){
            flag <- "OCEAN"
          }
          
          
            FishVec <- append(FishVec, j)
            
            LocVec <- append(LocVec, MFSub$NextLoc[StepKey])
            
            ArrVec <- append(ArrVec, (DepVec[length(DepVec)] + MFSub$StepTime[StepKey]))
          }
        }
        while(flag == "OCEAN"){
          # print(LocVec[length(LocVec)])
          # print(names(MFTable))
          # print(match(LocVec[length(LocVec)], names(MFTable)))
          MFSub <- MFTable[[2]][[match(LocVec[length(LocVec)], names(MFTable[[2]]))]]
          
          DepVec <- append(DepVec, (ArrVec[length(ArrVec)] + sample(MFSub$Holds)))
          
          StepKey <- sample(1:length(MFSub[,1]), 1)
          #print(LocVec[length(LocVec)])
          #print(StepKey)
          #print(MFSub$NextLoc[StepKey])
            if(MFSub$NextLoc[StepKey] %in% "END"){
              flag <- "END"
              break()
            }  else {
              FishVec <- append(FishVec, j)
              
              LocVec <- append(LocVec, MFSub$NextLoc[StepKey])
              
              ArrVec <- append(ArrVec, (DepVec[length(DepVec)] + MFSub$StepTime[StepKey]))
            
        }}
        cat(j)
      }
    }
    #print(FishVec)
    
    for(j in 1:length(FishVec)){
      SurrDense[j,1] <- FishVec[j]
      SurrDense[j,2] <- LocVec[j]
      SurrDense[j,3] <- ArrVec[j]
      SurrDense[j,4] <- DepVec[j]
    }
    # SurrDense$IDcol <- FishVec
    # SurrDense$Loccol <- LocVec
    # SurrDense$T1col <- ArrVec
    # SurrDense$T2col <- DepVec
    colnames(SurrDense) <- c("IDcol", "Loccol", "T1col", "T2col")
    MFSOut[[i]] <- SurrDense
    cat("\n", Pdex, " Run(s) Completed\n")
    Pdex <- Pdex + 1
  }
  return(MFSOut)
}

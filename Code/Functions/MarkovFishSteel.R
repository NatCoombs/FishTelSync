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
MarkovFishSurrogates<-function(MFTable, InitPos, InitTime, nFish, Reps, 
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
    if(Method == "SingleRelease"){
      for(j in 1:nFish){
        FishVec <- append(FishVec, j)
        LocVec <- append(LocVec, InitPos)
        ArrVec <- append(ArrVec, InitTime)
        
        flag <- "START"
        while(flag != "END"){
          # print(LocVec[length(LocVec)])
          # print(names(MFTable))
          # print(match(LocVec[length(LocVec)], names(MFTable)))
          MFSub <- MFTable[[match(LocVec[length(LocVec)], names(MFTable))]]
          
          DepVec <- append(DepVec, (ArrVec[length(ArrVec)] + sample(MFSub$Holds)))
          
          StepKey <- sample(1:length(MFSub[,1]), 1)
          if(MFSub$NextLoc[StepKey] %in% "END"){
            flag <- "END"
            break()
          } else {
            FishVec <- append(FishVec, j)
            
            LocVec <- append(LocVec, MFSub$NextLoc[StepKey])
            
            ArrVec <- append(ArrVec, (DepVec[length(DepVec)] + MFSub$StepTime[StepKey]))
          }
        }
      }
    }
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
    print(paste0(Pdex, " Run(s) Completed"))
    Pdex <- Pdex + 1
  }
  return(MFSOut)
}
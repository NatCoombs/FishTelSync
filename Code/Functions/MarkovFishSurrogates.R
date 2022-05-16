# # # function MarkovFish


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
  MFTable <- na.omit(MFTable)
  Pdex <- 1
  set.seed(Seed)
  MFSOut<-list()
  
  for(i in 1:Reps){
    SurrDense <- data.frame(IDcol = character(), Loccol = character(), T1col = numeric(), T2col = numeric())

    
    if(Method == "SingleRelease"){
    for(j in 1:nFish){
      FishVec <- c(j)
      LocVec <- c(InitPos)
      ArrVec <- c(InitTime)
      DepVec <- vector(mode = "numeric")
      
      flag <- "START"
      while(flag != "END"){
        
        #print(match(LocVec[length(LocVec)], names(MFTable)))
        MFSub <- MFTable[[match(LocVec[length(LocVec)], names(MFTable))]]
        #cat("H ")
        #cat(LocVec[length(LocVec)], match(LocVec[length(LocVec)], names(MFTable)), "\n")
      
        
        DepVec <- append(DepVec, (ArrVec[length(ArrVec)] + sample(MFSub$Holds, size = 1)))
        #cat(DepVec, "\n")
        # cat(LocVec[length(LocVec)], " ")
        # 
        # cat(length(MFSub[,1]), " ")
        l <- length(MFSub[,1])
        
        #cat("S ")
        
        StepKey <- sample(1:l, 1)
        
        if(MFSub$NextLoc[StepKey] %in% "END"){
          
          flag <- "END"
          #cat(c(FishVec, LocVec, ArrVec, DepVec))
          TempTab<-cbind.data.frame(FishVec, LocVec, ArrVec, DepVec)
          
          # TempTab$IDcol <- FishVec
          # TempTab$Loccol <- LocVec
          # TempTab$T1col <- ArrVec
          # TempTab$T2col <- DepVec
          
          SurrDense <- rbind(SurrDense, TempTab)
          
          break()
        } else {
          FishVec <- append(FishVec, j)
          #cat(FishVec, "\n")
          
          LocVec <- append(LocVec, MFSub$NextLoc[StepKey])
          #cat(LocVec, "\n")
          
          ArrVec <- append(ArrVec, (DepVec[length(DepVec)] + MFSub$StepTime[StepKey]))
          #cat(ArrVec, "\n")
        }
      }
    }
    }

    colnames(SurrDense) <- c("IDcol", "Loccol", "T1col", "T2col")
    
    MFSOut[[i]] <- na.omit(SurrDense)
    print(paste0(Pdex, " Run(s) Completed"))
    Pdex <- Pdex + 1
  }
  return(MFSOut)
}

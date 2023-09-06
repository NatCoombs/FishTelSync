

MFPop <- function(RedSet, TruePairs, Surrs, Reps, Seed = 1){
  set.seed(Seed)
  MeanMat<-matrix(nrow = Reps, ncol = 3)
  #cat("Here ")
  colnames(MeanMat) <- c("LD", "PS", "LS")
  #cat("or here")
  
  #ClusMat<-MeanMat
  
  NintiethMat<-MeanMat
  
  TenthMat<-MeanMat
  
  FishSet<-na.omit(unique(Surrs[,"IDcol"]))
  # Section for pulling "reference" levels for the population of concern
  RefScores<-matrix(data = NA, nrow = length(RedSet), ncol = 3)
  
  for(i in 1:length(RedSet)){
    RefScores[i,1] <- as.numeric(RedSet[[i]][7])
    RefScores[i,2] <- as.numeric(RedSet[[i]][14])
    RefScores[i,3] <- as.numeric(RedSet[[i]][21])
  }
  
  Refs<-matrix(data = NA, nrow = 4, ncol = 3)
  colnames(Refs) <- c("LD", "PS", "LS")
  rownames(Refs) <- c("Mean", "Modularity", "90th","10th")
  Ranks<-Refs
  
  TFish<-na.omit(unique(c(TruePairs[,1],TruePairs[,2])))
  
  
  for(i in 1:3){
    RefAdj<-matrix(nrow = length(TFish), ncol = length(TFish), data = 0)
    Refs[1,i] <- mean(RefScores[,i])
    Percs<-quantile(RefScores[,i], c(.9,.1))
    Refs[3,i] <- Percs[1]
    Refs[4,i] <- Percs[2]

    PairDex <- 1
    for(k in 1:(length(TFish)-1)){
      for(j in (k+1):length(TFish)){
        ZCheck <- as.numeric(RedSet[[PairDex]][[i*7]])
        if(ZCheck != 0){
          ZCheck <- 1/ZCheck
        }
        RefAdj[k,j] <- ZCheck
        RefAdj[j,k] <- ZCheck
        PairDex <- PairDex + 1
      }
    }

    RefClusEig<-wsyn::cluseigen(RefAdj)
    RefClusMemb<- RefClusEig[[length(RefClusEig)]]

    Refs[2,i]<-wsyn::modularity(RefAdj,RefClusMemb)
  }
  # Okay, we have our basic reference pipeline... Now what?
  nFish<-length(TFish)
  cat("Number of surrogate sets evaluated: ")
  Printer <- 1
  
  for(i in 1:Reps){
    #cat("BARK ")
    FishSub<-sample(FishSet, nFish)
    #print(FishSub)
    Pairs<-data.frame()
    PairDex<-1
    for(k in 1:(length(FishSub)-1)){
      for(j in (k+1):length(FishSub)){
        Pairs[PairDex,1] <- FishSub[k]
        Pairs[PairDex,2] <- FishSub[j]
        PairDex <- PairDex + 1
      }  
    }
    
    #print(Pairs)
    
    ReducedSurr<-list()
    
    for(j in 1:length(Pairs[,1])){
      ReducedSurr[[j]]<-LCSExtract(Surrs[which(Surrs[,"IDcol"] %in% Pairs[j,1]),"Loccol"],
                                   Surrs[which(Surrs[,"IDcol"] %in% Pairs[j,2]),"Loccol"],
                                   as.numeric(Surrs[which(Surrs[,"IDcol"] %in% Pairs[j,1]),"T1col"]),
                                   as.numeric(Surrs[which(Surrs[,"IDcol"] %in% Pairs[j,1]),"T2col"]),
                                   as.numeric(Surrs[which(Surrs[,"IDcol"] %in% Pairs[j,2]),"T1col"]),
                                   as.numeric(Surrs[which(Surrs[,"IDcol"] %in% Pairs[j,2]),"T2col"]),
                                   c("All"))
    }
    
    SurrScores<-matrix(data = NA, nrow = length(ReducedSurr), ncol = 3)
    
    for(j in 1:length(ReducedSurr)){
      SurrScores[j,1] <- as.numeric(ReducedSurr[[j]][7])
      SurrScores[j,2] <- as.numeric(ReducedSurr[[j]][14])
      SurrScores[j,3] <- as.numeric(ReducedSurr[[j]][21])
    }
    #print(SurrScores)
    #cat("BORK ")
    for(j in 1:3){
      MeanMat[i,j] <- mean(SurrScores[,j])
      Percs<-quantile(SurrScores[,j], c(.9,.1))
      NintiethMat[i,j] <- Percs[1]
      TenthMat[i,j] <- Percs[2]
      
      SurrAdj<-matrix(nrow = nFish, ncol = nFish, data = 0)
      
      for(k in 1:(nFish-1)){
        for(l in (k+1):nFish){
          ZCheck <- as.numeric(SurrScores[PairDex,j])
          if(ZCheck != 0){
            ZCheck <- 1/ZCheck
          }
          SurrAdj[k,l] <- ZCheck
          SurrAdj[l,k] <- ZCheck
          PairDex <- PairDex + 1
        }
      }
      
      SurrClusEig<-wsyn::cluseigen(SurrAdj)
      SurrClusMemb<-SurrClusEig[[length(SurrClusEig)]]
      
      ClusMat[i,j]<-wsyn::modularity(SurrAdj,SurrClusMemb)
      
      #cat("BURK ")
      # for(k in 1:length(Pairs[,1])){
      # 
      #   LDk1 <- match(Pairs[k,1],FishSub)
      #   LDk2 <- match(Pairs[k,2],FishSub)
      #   #print(c(LDk1,LDk2))
      #   SurrAdj[LDk1,LDk2] <- as.numeric(ReducedSurr[[k]][[j*7]])
      #   #print(SurrAdj[LDk1,LDk2])
      #   SurrAdj[LDk2,LDk1] <- SurrAdj[LDk1,LDk2]
      # }
      # for(k in 1:length(TFish)){
      #   SurrAdj[k,k] <- 0
      # }
      # #scan()
      # # issue is currently at cluseigen
      # # There are, somehow, NAs being introduced.
      # print(nrow(SurrAdj))
      # print(any(is.na(SurrAdj)))
      # SurrClust<-cluseigen(SurrAdj)
      # # cat("BIRK ")
      # # scan()
      # ClusMat[i,j]<-modularity(SurrAdj,SurrClust[[length(SurrClust)]])
    }
    
    cat(Printer, " ")
    Printer<-Printer+1
  }
  
  for(i in 1:3){
    Ranks[1,i] <- length(which(MeanMat[,i] < Refs[1,i]))
    Ranks[2,i] <- length(which(ClusMat[,i] < Refs[2,i]))
    Ranks[3,i] <- length(which(NintiethMat[,i] < Refs[3,i]))
    Ranks[4,i] <- length(which(TenthMat[,i] < Refs[4,i]))
  }
  OutList<-list()
  OutList[[1]] <- Refs
  OutList[[2]] <- Ranks
  
  DistList<-list()
  DistList[[1]] <- MeanMat
  DistList[[2]] <- ClusMat
  DistList[[3]] <- NintiethMat
  DistList[[4]] <- TenthMat
  
  OutList[[3]] <- DistList
  
  return(OutList)
}

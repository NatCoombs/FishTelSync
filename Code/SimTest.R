# Simulation for tool testing
TestSimNeut<-function(nFish, nSites){
  nSites<-nSites+1
  mat <- as.data.frame(matrix(data = NA, nrow = nFish*nSites, ncol = 5))
  mat[,1] <- rep(1:nFish, each = nSites)
  mat[,2] <- rep(c("Rel",1:(nSites-1)), times = nFish)
  mat[,5] <- 2
  for(i in 1:nFish){
    mat[(i-1)*nSites + 1,3] <- 0
    # Uses 10 as the standard distance between sites
    mat[(i-1)*nSites + 1,4] <- 0
    
    for(j in 2:nSites){
      # Death
      if(sample(1:20, 1) == 20){
        break
      }
      
      # Move
      SampSp <- -1
      while(SampSp < .5){
        SampSp <- rnorm(1, mean = 3, sd = 1)
      }
      mat[(i-1)*nSites + j,3] <- 10/SampSp + mat[(i-1)*nSites + j-1,4]
      
      # Hold
      SampHo <- -1
      while(SampHo < .0000001){
        SampHo <- rnorm(1, mean = .2, sd = .05)
      }
      mat[(i-1)*nSites + j,4] <- SampHo + mat[(i-1)*nSites + j,3]
      
    }
  }
  DropVec <-which(is.na(mat[,3]))
  if(length(DropVec) > 0){
    mat <- mat[-DropVec,]
  }
  df <- as.data.frame(mat)
  colnames(df) <- c("FishID", "Loc", "FirstDet", "LastDet", "NumDets")
  df$FishID <- as.character(df$FishID)
  df$Loc <- as.character(df$Loc)
  
  DropVec2 <- which(df$FishID %in% as.character(which(table(sort(as.numeric(df[,1]))) < 5)))
  if(length(DropVec2) > 0){
    df <- df[-DropVec2,]
  }
  return(df)
}

TestSimSpeed<-function(nFish, nSites, SplitAt, SpeDiff){
  nSites<-nSites+1
  mat <- as.data.frame(matrix(data = NA, nrow = nFish*nSites, ncol = 5))
  mat[,1] <- rep(1:nFish, each = nSites)
  mat[,2] <- rep(c("Rel",1:(nSites-1)), times = nFish)
  mat[,5] <- 2
  for(i in 1:nFish){
    mat[(i-1)*nSites + 1,3] <- 0
    # Uses 10 as the standard distance between sites
    mat[(i-1)*nSites + 1,4] <- 0
    
    for(j in 2:nSites){
      # Death
      if(sample(1:20, 1) == 20){
        break
      }
      
      # Move
      SampSp <- -1
      while(SampSp < .5){
        SampSp <- rnorm(1, mean = 3, sd = 1)
      }
      mat[(i-1)*nSites + j,3] <- 10/SampSp + mat[(i-1)*nSites + j-1,4]
      
      # Hold
      SampHo <- -1
      while(SampHo < .0000001){
        SampHo <- rnorm(1, mean = .2, sd = .05)
      }
      mat[(i-1)*nSites + j,4] <- SampHo + mat[(i-1)*nSites + j,3]
      
    }
  }
  # Second speed class
  for(i in SplitAt:nFish){
    mat[(i-1)*nSites + 1,3] <- 0
    # Uses 10 as the standard distance between sites
    mat[(i-1)*nSites + 1,4] <- 0
    
    for(j in 2:nSites){
      # Death
      if(sample(1:20, 1) == 20){
        break
      }
      
      # Move
      SampSp <- -1
      while(SampSp < .5){
        SampSp <- rnorm(1, mean = 3 * SpeDiff, sd = 1)
      }
      mat[(i-1)*nSites + j,3] <- 10/SampSp + mat[(i-1)*nSites + j-1,4]
      
      # Hold
      SampHo <- -1
      while(SampHo < .0000001){
        SampHo <- rnorm(1, mean = .2, sd = .05)
      }
      mat[(i-1)*nSites + j,4] <- SampHo + mat[(i-1)*nSites + j,3]
      
    }
  }
  DropVec <-which(is.na(mat[,3]))
  if(length(DropVec) > 0){
    mat <- mat[-DropVec,]
  }
  df <- as.data.frame(mat)
  colnames(df) <- c("FishID", "Loc", "FirstDet", "LastDet", "NumDets")
  df$FishID <- as.character(df$FishID)
  df$Loc <- as.character(df$Loc)
  
  DropVec2 <- which(df$FishID %in% as.character(which(table(sort(as.numeric(df[,1]))) < 5)))
  if(length(DropVec2) > 0){
    df <- df[-DropVec2,]
  }
  return(df)
}

TestSimHold<-function(nFish, nSites, SplitAt, HolDiff){
  nSites<-nSites+1
  mat <- as.data.frame(matrix(data = NA, nrow = nFish*nSites, ncol = 5))
  mat[,1] <- rep(1:nFish, each = nSites)
  mat[,2] <- rep(c("Rel",1:(nSites-1)), times = nFish)
  mat[,5] <- 2
  for(i in 1:nFish){
    mat[(i-1)*nSites + 1,3] <- 0
    # Uses 10 as the standard distance between sites
    mat[(i-1)*nSites + 1,4] <- 0
    
    for(j in 2:nSites){
      # Death
      if(sample(1:20, 1) == 20){
        break
      }
      
      # Move
      SampSp <- -1
      while(SampSp < .5){
        SampSp <- rnorm(1, mean = 3, sd = 1)
      }
      mat[(i-1)*nSites + j,3] <- 10/SampSp + mat[(i-1)*nSites + j-1,4]
      
      # Hold
      SampHo <- -1
      while(SampHo < .0000001){
        SampHo <- rnorm(1, mean = .2, sd = .05)
      }
      mat[(i-1)*nSites + j,4] <- SampHo + mat[(i-1)*nSites + j,3]
      
    }
  }
  
  # Second hold class
  for(i in SplitAt:nFish){
    mat[(i-1)*nSites + 1,3] <- 0
    # Uses 10 as the standard distance between sites
    mat[(i-1)*nSites + 1,4] <- 0
    
    for(j in 2:nSites){
      # Death
      if(sample(1:20, 1) == 20){
        break
      }
      
      # Move
      SampSp <- -1
      if(j %in% 1:3){
        while(SampSp < .5){
          SampSp <- rnorm(1, mean = 3/HolDiff, sd = 1)
        }
      } else {
        while(SampSp < .5){
        SampSp <- rnorm(1, mean = 3, sd = 1)
        }
      }
      mat[(i-1)*nSites + j,3] <- 10/SampSp + mat[(i-1)*nSites + j-1,4]
      
      # Hold
      SampHo <- -1
      meanHo <- .2
      if(j %in% 1:3){
        meanHo = .2 * HolDiff
      }
     
      while(SampHo < .0000001){
        SampHo <- rnorm(1, mean = meanHo, sd = .05)
      }
      mat[(i-1)*nSites + j,4] <- SampHo + mat[(i-1)*nSites + j,3]
      
    }
  }
  DropVec <-which(is.na(mat[,3]))
  if(length(DropVec) > 0){
    mat <- mat[-DropVec,]
  }
  df <- as.data.frame(mat)
  colnames(df) <- c("FishID", "Loc", "FirstDet", "LastDet", "NumDets")
  df$FishID <- as.character(df$FishID)
  df$Loc <- as.character(df$Loc)
  
  DropVec2 <- which(df$FishID %in% as.character(which(table(sort(as.numeric(df[,1]))) < 5)))
  if(length(DropVec2) > 0){
    df <- df[-DropVec2,]
  }
  return(df)
}

ControlTable <- matrix(data = NA, nrow = 18, ncol = 5)
ControlTable[,1] <- rep(c(10,30,50), each = 6)
ControlTable[,2] <- rep(rep(c(10,20), each = 3), times = 3)
ControlTable[,3] <- rep(c(6,16,26), each = 6)
ControlTable[,4] <- rep(c(0,2,0), times = 6)
ControlTable[,5] <- rep(c(0,0,3), times = 6)
ControlTable<-rbind(ControlTable,ControlTable,ControlTable,ControlTable,ControlTable)
# Three cases: Neutral synchrony, Holding synchrony, and Speed synchrony
set.seed(404)
TestDets <- vector(mode = "list", length = nrow(ControlTable))
for(i in 1:length(TestDets)){
  if((i %% 3 == 1)){
    TestDets[[i]] <- TestSimNeut(ControlTable[i,1],ControlTable[i,2])
  } else   if((i %% 3 == 2)){
    TestDets[[i]] <- TestSimSpeed(ControlTable[i,1],ControlTable[i,2],ControlTable[i,3],ControlTable[i,4])
  } else   if((i %% 3 == 0)){
    TestDets[[i]] <- TestSimHold(ControlTable[i,1],ControlTable[i,2],ControlTable[i,3],ControlTable[i,5])
  }
}

TestPairsGen<-function(DetMat){
  FishVec <- as.character(unique(DetMat[,1]))
  n <- length(FishVec)
  mat <- matrix(data = NA, nrow = choose(n,2), ncol = 2)
  k <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      mat[k,1] <- FishVec[i]
      mat[k,2] <- FishVec[j]
      k<-k+1
    }
  }
  return(mat)
}

TestPairs <- vector(mode = "list", length = length(TestDets))
for(i in 1:length(TestDets)){
  TestPairs[[i]] <- TestPairsGen(TestDets[[i]])
}




# Now to print it out and send it to the cluster
if(!dir.exists("./Data/TestSim")){
  dir.create("./Data/TestSim")
}
if(!dir.exists("./Data/TestSim/Dets")){
  dir.create("./Data/TestSim/Dets")
}
if(!dir.exists("./Data/TestSim/Pairs")){
  dir.create("./Data/TestSim/Pairs")
}

for(i in 1:length(TestDets)){
  write.csv(TestPairs[[i]], file = paste0("./Data/TestSim/Pairs/", i, ".csv"), row.names=FALSE)
  write.csv(TestDets[[i]], file = paste0("./Data/TestSim/Dets/", i, ".csv"), row.names=FALSE)
}

# Read in results we have off of the cluster:

OutFileSet <- list.files("/Users/nathanielcoombs/Desktop/Cluster_folders/TestSim/Outputs/Pops/")
OutSet <- vector(mode = "character", length = length(OutFileSet))
for(i in 1:length(OutFileSet)){
  OutSet[i] <- strsplit(OutFileSet[i], ".", fixed = T)[[1]][1]
}

SimPairSig <- vector(mode = "list", length = length(OutFileSet))
SimPopSig <- SimPairSig

for(i in 1:length(SimPairSig)){
  SimPopSig[[i]] <-  readRDS(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/TestSim/Outputs/Pops/", OutSet[i], ".rds", sep = ""))
  SimPairSig[[i]] <-  read.csv(paste("/Users/nathanielcoombs/Desktop/Cluster_folders/TestSim/Outputs/Pairs/", OutSet[i], ".csv", sep = ""))
}

SimPopMat <- matrix(data = NA, nrow = length(SimPopSig), ncol = 17)
colnames(SimPopMat) <- c("MeanLD", "MeanPS", "MeanLS", "ModLD", "ModPS", "ModLS", "PMeanLD", "PMeanPS", "PMeanLS", "PModLD", "PModPS", "PModLS", "nFishGen", "nSites","SplitAt","SpdDiff","HoldDiff")
rownames(SimPopMat) <- OutSet
for(i in 1:length(SimPopSig)){
  #print(i)
  PopSigTemp <- SimPopSig[[i]]
  if(length(PopSigTemp) == 0){
    next
  }
  SimPopMat[i,1] <- 1/PopSigTemp[[1]][1,1]
  SimPopMat[i,2] <- 1/PopSigTemp[[1]][1,2]
  SimPopMat[i,3] <- 1/PopSigTemp[[1]][1,3]
  
  SimPopMat[i,4] <- PopSigTemp[[1]][2,1]
  SimPopMat[i,5] <- PopSigTemp[[1]][2,2]
  SimPopMat[i,6] <- PopSigTemp[[1]][2,3]
  
  
  
  SimPopMat[i,7] <- 1-PopSigTemp[[2]][1,1]/1000
  SimPopMat[i,8] <- 1-PopSigTemp[[2]][1,2]/1000
  SimPopMat[i,9] <- 1-PopSigTemp[[2]][1,3]/1000
  
  SimPopMat[i,10] <- 1-(PopSigTemp[[2]][2,1]/1000)
  SimPopMat[i,11] <- 1-(PopSigTemp[[2]][2,2]/1000)
  SimPopMat[i,12] <- 1-(PopSigTemp[[2]][2,3]/1000)
}

SimPopMat[,13:17] <- ControlTable[as.numeric(OutSet),]

SimPopMat<-SimPopMat[order(SimPopMat[,16],SimPopMat[,17],SimPopMat[,13],SimPopMat[,14],decreasing = F),]

write.csv(SimPopMat, "./Results/Sim.csv")

# Then test by dropping fish & releases iteratively

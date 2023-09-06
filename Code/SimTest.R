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
  for(i in 1:(SplitAt-1)){
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
  # Come back to this later
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
  for(i in 1:(SplitAt-1)){
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
      if(j %in% 2:4){
        while(SampSp < (.5/HolDiff)){
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
      if(j %in% 2:4){
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

SimPopMatNeut<-SimPopMat[1:30,]
SimPopMatHold<-SimPopMat[31:60,]
SimPopMatSped<-SimPopMat[61:90,]
par(mfrow=c(2,1))

SimPlotTempSens<-function(Mat, Title){
  plot(x = Mat[,13], y = Mat[,1], main = paste(Title, "- Mean Synchrony vs nFish"), xlim = c(9,65))
  # text(x = 10, y = 0, paste("nSig =", length(which(Mat[1:10,7] < .05))), pos = 4)
  # text(x = 30, y = 0, paste("nSig =", length(which(Mat[11:20,7] < .05))), pos = 4)
  # text(x = 50, y = 0, paste("nSig =", length(which(Mat[21:30,7] < .05))), pos = 4)
  # 
  # text(x = 10, y = 1, paste("nSig =", length(which(Mat[1:10,7] > .95))), pos = 4)
  # text(x = 30, y = 1, paste("nSig =", length(which(Mat[11:20,7] > .95))), pos = 4)
  # text(x = 50, y = 1, paste("nSig =", length(which(Mat[21:30,7] > .95))), pos = 4)
  #text(x = 40, y = 1, paste("Total Sig =", length(which(Mat[,7] < .05))))
  # abline(h = .95, col = "Blue")
  # abline(h = .05, col = "red")
  plot(x = Mat[,14], y = Mat[,1], main = paste(Title, "- Mean Synchrony vs nSites"), xlim = c(9,25))
  # abline(h = .95, col = "Blue")
  # abline(h = .05, col = "red")
  # text(x = 10, y = 0, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),7] < .05))), pos = 4)
  # text(x = 20, y = 0, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),7] < .05))), pos = 4)
  # 
  # text(x = 10, y = 1, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),7] > .95))), pos = 4)
  # text(x = 20, y = 1, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),7] > .95))), pos = 4)
  plot(x = Mat[,13], y = Mat[,4], main = paste(Title, "- Modularity vs nFish"), xlim = c(9,65))
  # text(x = 10, y = 0, paste("nSig =", length(which(Mat[1:10,10] < .05))), pos = 4)
  # text(x = 30, y = 0, paste("nSig =", length(which(Mat[11:20,10] < .05))), pos = 4)
  # text(x = 50, y = 0, paste("nSig =", length(which(Mat[21:30,10] < .05))), pos = 4)
  # 
  # text(x = 10, y = 1, paste("nSig =", length(which(Mat[1:10,10] > .95))), pos = 4)
  # text(x = 30, y = 1, paste("nSig =", length(which(Mat[11:20,10] > .95))), pos = 4)
  # text(x = 50, y = 1, paste("nSig =", length(which(Mat[21:30,10] > .95))), pos = 4)
  #text(x = 40, y = 1, paste("Total Sig =", length(which(Mat[,10] < .05))))
  # abline(h = .95, col = "Blue")
  # abline(h = .05, col = "red")
  plot(x = Mat[,14], y = Mat[,4], main = paste(Title, "- Modularity vs nSites"), xlim = c(9,25))
  # abline(h = .95, col = "Blue")
  # abline(h = .05, col = "red")
  # text(x = 10, y = 0, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),10] < .05))), pos = 4)
  # text(x = 20, y = 0, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),10] < .05))), pos = 4)
  # 
  # text(x = 10, y = 1, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),10] > .95))), pos = 4)
  # text(x = 20, y = 1, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),10] > .95))), pos = 4)
  # 
  # 
  
  plot(x = Mat[,13], y = Mat[,7], main = paste(Title, "- pMean Synchrony vs nFish"), xlim = c(9,65), ylim = c(1.1,-.1))
  text(x = 10, y = 0, paste("nSig =", length(which(Mat[1:10,7] < .05))), pos = 4)
  text(x = 30, y = 0, paste("nSig =", length(which(Mat[11:20,7] < .05))), pos = 4)
  text(x = 50, y = 0, paste("nSig =", length(which(Mat[21:30,7] < .05))), pos = 4)
  
  text(x = 10, y = 1, paste("nSig =", length(which(Mat[1:10,7] > .95))), pos = 4)
  text(x = 30, y = 1, paste("nSig =", length(which(Mat[11:20,7] > .95))), pos = 4)
  text(x = 50, y = 1, paste("nSig =", length(which(Mat[21:30,7] > .95))), pos = 4)
  #text(x = 40, y = 1, paste("Total Sig =", length(which(Mat[,7] < .05))))
  abline(h = .95, col = "Blue")
  abline(h = .05, col = "red")
  plot(x = Mat[,14], y = Mat[,7], main = paste(Title, "- pMean Synchrony vs nSites"), xlim = c(9,25), ylim = c(1.1,-.1))
  abline(h = .95, col = "Blue")
  abline(h = .05, col = "red")
  text(x = 10, y = 0, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),7] < .05))), pos = 4)
  text(x = 20, y = 0, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),7] < .05))), pos = 4)
  
  text(x = 10, y = 1, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),7] > .95))), pos = 4)
  text(x = 20, y = 1, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),7] > .95))), pos = 4)
  plot(x = Mat[,13], y = Mat[,10], main = paste(Title, "- pModularity vs nFish"), xlim = c(9,65), ylim = c(1.1,-.1))
  text(x = 10, y = 0, paste("nSig =", length(which(Mat[1:10,10] < .05))), pos = 4)
  text(x = 30, y = 0, paste("nSig =", length(which(Mat[11:20,10] < .05))), pos = 4)
  text(x = 50, y = 0, paste("nSig =", length(which(Mat[21:30,10] < .05))), pos = 4)
  
  text(x = 10, y = 1, paste("nSig =", length(which(Mat[1:10,10] > .95))), pos = 4)
  text(x = 30, y = 1, paste("nSig =", length(which(Mat[11:20,10] > .95))), pos = 4)
  text(x = 50, y = 1, paste("nSig =", length(which(Mat[21:30,10] > .95))), pos = 4)
  #text(x = 40, y = 1, paste("Total Sig =", length(which(Mat[,10] < .05))))
  abline(h = .95, col = "Blue")
  abline(h = .05, col = "red")
  plot(x = Mat[,14], y = Mat[,10], main = paste(Title, "- pModularity vs nSites"), xlim = c(9,25), ylim = c(1.1,-.1))
  abline(h = .95, col = "Blue")
  abline(h = .05, col = "red")
  text(x = 10, y = 0, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),10] < .05))), pos = 4)
  text(x = 20, y = 0, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),10] < .05))), pos = 4)
  
  text(x = 10, y = 1, paste("nSig =", length(which(Mat[c(1:5,11:15,21:25),10] > .95))), pos = 4)
  text(x = 20, y = 1, paste("nSig =", length(which(Mat[c(6:10,16:20,26:30),10] > .95))), pos = 4)
}

SimPlotTempSens(SimPopMatHold, "Holding Difference")



SimPlotTempX<-function(Mat, Title){
  plot(x = Mat[,7], y = Mat[,4], main = paste(Title, "- Modularity vs pMean Sychrony"), xlim = c(1.1,-.1), ylim = c(-.1,1.1))
  #abline(h = .95, col = "Blue")
  abline(v = .05, col = "Red")
}

SimPlotTempX(SimPopMatNeut, "No Difference")



pdf("./Results/SimOuts.pdf")
par(mfrow=c(2,1))

if(TRUE){
plot(x = SimPopMatNeut[,13], y = SimPopMatNeut[,7], main =  "All - pMean Synchrony vs nFish", xlim = c(9,65), ylim = c(1.1,-.1), col = "gold4")
points(x = SimPopMatHold[,13], y = SimPopMatHold[,7], col = "Blue")
points(x = SimPopMatSped[,13], y = SimPopMatSped[,7], col = "chartreuse4")
text(x = 10, y = 0, paste("nSig =", length(which(SimPopMat[c(1:10,31:40,61:70),7] < .05))), pos = 4)
text(x = 30, y = 0, paste("nSig =", length(which(SimPopMat[c(11:20,41:50,71:80),7] < .05))), pos = 4)
text(x = 50, y = 0, paste("nSig =", length(which(SimPopMat[c(21:30,51:60,81:90),7] < .05))), pos = 4)
text(x = 40, y = 1, paste("Total Sig =", length(which(SimPopMat[,7] < .05))))
abline(h = .95, col = "Purple")
abline(h = .05, col = "red")
#legend(x = "topleft", legend = c("Hold","Neut","Speed"), col = c("Blue","gold","chartreuse4"))
plot(x = SimPopMatNeut[,14], y = SimPopMatNeut[,7], main =  "All - pMean Synchrony vs nSites", xlim = c(9,25), ylim = c(1.1,-.1), col = "gold4")
points(x = SimPopMatHold[,14], y = SimPopMatHold[,7], col = "Blue")
points(x = SimPopMatSped[,14], y = SimPopMatSped[,7], col = "chartreuse4")
text(x = 10, y = 0, paste("nSig =", length(which(SimPopMat[c(c(1:5,11:15,21:25),c(1:5,11:15,21:25)+30,c(1:5,11:15,21:25)+60),7] < .05))), pos = 4)
text(x = 20, y = 0, paste("nSig =", length(which(SimPopMat[c(c(6:10,16:20,26:30),c(6:10,16:20,26:30)+30,c(6:10,16:20,26:30)+60),7] < .05))), pos = 4)
abline(h = .95, col = "Purple")
abline(h = .05, col = "red")
}

if(TRUE){
plot(x = SimPopMatNeut[,13], y = SimPopMatNeut[,4], main =  "All - Modularity vs nFish", xlim = c(9,65), ylim = c(-.1,1.1), col = "gold4")
points(x = SimPopMatHold[,13], y = SimPopMatHold[,4], col = "Blue")
points(x = SimPopMatSped[,13], y = SimPopMatSped[,4], col = "chartreuse4")
# text(x = 10, y = 0, paste("nSig =", length(which(SimPopMat[c(1:10,31:40,61:70),10] < .05))), pos = 4)
# text(x = 30, y = 0, paste("nSig =", length(which(SimPopMat[c(11:20,41:50,71:80),10] < .05))), pos = 4)
# text(x = 50, y = 0, paste("nSig =", length(which(SimPopMat[c(21:30,51:60,81:90),10] < .05))), pos = 4)
#text(x = 40, y = 1, paste("Total Sig =", length(which(SimPopMat[,10] < .05))))
# abline(h = .95, col = "Purple")
# abline(h = .05, col = "red")
#legend(x = "topleft", legend = c("Hold","Neut","Speed"), col = c("Blue","gold","chartreuse4"))
plot(x = SimPopMatNeut[,14], y = SimPopMatNeut[,4], main =  "All - Modularity vs nSites", xlim = c(9,25), ylim = c(-.1,1.1), col = "gold4")
points(x = SimPopMatHold[,14], y = SimPopMatHold[,4], col = "Blue")
points(x = SimPopMatSped[,14], y = SimPopMatSped[,4], col = "chartreuse4")
# text(x = 10, y = 0, paste("nSig =", length(which(SimPopMat[c(c(1:5,11:15,21:25),c(1:5,11:15,21:25)+30,c(1:5,11:15,21:25)+60),10] < .05))), pos = 4)
# text(x = 20, y = 0, paste("nSig =", length(which(SimPopMat[c(c(6:10,16:20,26:30),c(6:10,16:20,26:30)+30,c(6:10,16:20,26:30)+60),10] < .05))), pos = 4)
# abline(h = .95, col = "Purple")
# abline(h = .05, col = "red")
}

SimPlotTempSens(SimPopMatNeut, "No Difference")
SimPlotTempSens(SimPopMatHold, "Holding Difference")
SimPlotTempSens(SimPopMatSped, "Speed Difference")

par(mfrow=c(1,1))

plot(x = SimPopMatNeut[,7], y = SimPopMatNeut[,4], main =  "All - Modularity vs pMean Synchrony", xlim = c(1.1,-.1), ylim = c(-.1,1.1), col = "gold")
points(x = SimPopMatHold[,7], y = SimPopMatHold[,4], col = "Blue")
points(x = SimPopMatSped[,7], y = SimPopMatSped[,4], col = "chartreuse4")
abline(v = .05, col = "Red")

SimPlotTempX(SimPopMatNeut, "No Difference")
SimPlotTempX(SimPopMatHold, "Holding Difference")
SimPlotTempX(SimPopMatSped, "Speed Difference")

dev.off()


# Generate and plot some surrogate fish vs real releases
MTableSet<-vector(mode = "list", length = length(TestDets))
SurrogatesSet<-MTableSet
set.seed(333)
for(i in 1:length(TestDets)){
  print(i)
  MTableSet[[i]] <- MarkovFishTable(TestDets[[i]], "FishID","Loc","FirstDet","LastDet")
  SurrogatesSet[[i]] <- MarkovFishSurrogates(MTableSet[[i]], "Rel",0,1000,Reps = 1)[[1]]
}

PlotSurrsSim<-function(Dets, Surrs, ControlTable, RelNo){
  par(mfrow = c(2,1))
  TrueFish<-unique(Dets[,1])
  FishSplit<-vector(mode = "list", length = length(TrueFish))
  SurrFish<-unique(Surrs[,1])
  SurrSplit<-vector(mode = "list", length = length(SurrFish))
  
  pal<-colorRampPalette(c("Red","Yellow","Blue"))
  
  TruePal<-pal(length(TrueFish))
  
  SurrPal<-pal(length(SurrFish))
  # Setting it up to plot faster
  Dets[which(Dets[,2] == "Rel"),2] <- "0"
  Dets[,2] <- as.character(-as.numeric(Dets[,2]))
  Surrs[which(Surrs[,2] == "Rel"),2] <- "0"
  Surrs[,2] <- as.character(-as.numeric(Surrs[,2]))
  ylims<-c(min(as.numeric(Surrs[,2])),0)
  xlims<-c(0,max(Surrs[,4]))
  for(i in 1:length(FishSplit)){
    TempFish<-Dets[which(Dets[,1] == TrueFish[i]),]
    TempMat<-matrix(data = NA, nrow = 2*nrow(TempFish), ncol = 2)
    for(j in 1:nrow(TempFish)){
      TempMat[2*j-1,1] <- as.numeric(TempFish[j,2])
      TempMat[2*j-1,2] <- as.numeric(TempFish[j,3])
    
      TempMat[2*j,1] <- as.numeric(TempFish[j,2])
      TempMat[2*j,2] <- as.numeric(TempFish[j,4])
    }
    TempMat[order(TempMat[,2],decreasing=FALSE),]
    colnames(TempMat) <- c("Loc","Time")

    FishSplit[[i]] <- TempMat
    
  }
  
  for(i in 1:length(SurrSplit)){
    TempFish<-Surrs[which(Surrs[,1] == SurrFish[i]),]
    TempMat<-matrix(data = NA, nrow = 2*nrow(TempFish), ncol = 2)
    for(j in 1:nrow(TempFish)){
      TempMat[2*j-1,1] <- as.numeric(TempFish[j,2])
      TempMat[2*j-1,2] <- as.numeric(TempFish[j,3])
      
      TempMat[2*j,1] <- as.numeric(TempFish[j,2])
      TempMat[2*j,2] <- as.numeric(TempFish[j,4])
    }
    TempMat[order(TempMat[,2],decreasing=FALSE),]
    colnames(TempMat) <- c("Loc","Time")
    
    SurrSplit[[i]]<-TempMat
  }
  # And now we can plot

  plot(x = as.numeric(FishSplit[[1]][,2]), y = as.numeric(FishSplit[[1]][,1]), main = "Simulated Release", col = TruePal[1], type = "l", ylim = ylims, xlim = xlims)
  
  for(i in 2:length(FishSplit)){
    lines(x = FishSplit[[i]][,2], y = FishSplit[[i]][,1], col = TruePal[i])
  }
  ControlVec<-ControlTable[RelNo,]
  #print(ControlVec)
  text(x = 10, y = (ylims[1] + (ylims[2]-ylims[1])/4), paste0("NFish: ", ControlVec[1], ", NSites: ", ControlVec[2], ", SpDiff: ", ControlVec[4], ", HolDiff: ", ControlVec[5]), cex = .5)
  
  
  plot(x = SurrSplit[[1]][,2], y = SurrSplit[[1]][,1], main = "Surrogate Fish", col = SurrPal[1], type = "l", ylim = ylims, xlim = xlims)
  for(i in 2:length(SurrSplit)){
    lines(x = SurrSplit[[i]][,2], y = SurrSplit[[i]][,1], col = SurrPal[i])
  }
  
  par(mfrow = c(1,1))
}


pdf("./Results/SimSurrComp.pdf")
for(i in 1:length(TestDets)){
  print(i)
  PlotSurrsSim(TestDets[[i]],SurrogatesSet[[i]],ControlTable, i)
}
dev.off()


TestPairs[[1]]

SynchMatStitchLD<-function(Synch,Pairs, Split){
  TFish<-na.omit(unique(c(Pairs[,1],Pairs[,2])))
  RefAdj<-matrix(nrow = length(TFish), ncol = length(TFish), data = 0)
  PairDex <- 1
  
  for(k in 1:(length(TFish)-1)){
    for(j in (k+1):length(TFish)){
      print(PairDex)
      ZCheck <- as.numeric(Synch[PairDex,1])
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
  RefSet <- vector(mode = "list", length = 3)
  RefSet[[1]] <- RefAdj
  RefSet[[2]] <- RefClusMemb
  
  RefMatch<-matrix(nrow = length(TFish), ncol = length(TFish), data = 0)
  PairDex <- 1
  
  for(k in 1:(length(TFish)-1)){
    for(j in (k+1):length(TFish)){
      ZCheck <- 0
      if(as.numeric(TFish[k]) < Split){
        if(as.numeric(TFish[j]) < Split){
          ZCheck <- 1
        } else {
          ZCheck <- -1
        }
      } else {
        if(as.numeric(TFish[j]) < Split){
          ZCheck <- -1
        } else {
          ZCheck <- 1
        }
      }
      RefMatch[k,j] <- ZCheck
      RefMatch[j,k] <- ZCheck
      PairDex <- PairDex + 1
    }
  }
  
  RefSet[[3]] <- RefMatch
  
  return(RefSet)
}

TestClustMats<-vector(mode = "list", length = 90)
for(i in 1:90){
  print(i)
  TestClustMats[[i]] <-SynchMatStitchLD(SimPairSig[[i]], TestPairs[[i]], ControlTable[i,3])
}

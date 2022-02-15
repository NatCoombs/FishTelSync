# Funct MFSig

# Description: Takes two sets of surrogate fish and
# compares an actual set of LCS values for fish against 
# values achieved by inputting surrogate fish with the same
# terminal detection location as the real fish.

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

# Currently assumes that you're using all synchrony metrics.

MFSig<-function(FishPairSet, RealDense, LCSList, MFS1, MFS2, Reps, Seed = 1){
  # Start by building a set of fish and terminal detections?
  cat("WARNING: CURRENT VERSION ASSUMES THAT FISHPAIRSET MATCHES THE ORDER OF THE LCSLIST \n")

  set.seed(Seed)
  FishSet<-unique(c(FishPairSet[,1],FishPairSet[,2]))
  FishSet<-FishSet[which(!is.na(FishSet))]
  FishTerm <- matrix(data = NA, nrow = length(FishSet), ncol = 2)
  
  for(i in 1:length(FishSet)){
    FishTerm[i,1] <- FishSet[i]
    DenseSub <- RealDense[RealDense[,"FishID"] %in% FishSet[i],]
    FishTerm[i,2] <- DenseSub[,"Loc"][length(DenseSub[,1])]
  }
  
  TermSet <- unique(FishTerm[,2])
  TermSet <- TermSet[which(!is.na(TermSet))]

  
  cat("Empty matrices built \n")
  # Now that we have a set of terminal locations bound to fish
  # We can start to build a set of fish from the synthetic dense
  # tables to use for our LCS's.
  
  SynthTerms <- matrix(data = NA, nrow = length(unique(MFS1[,"IDcol"])),
                       ncol = 4)
  for(i in 1:length(unique(MFS1[,"IDcol"]))){
   # cat(i, "\n")
    SynthTerms[i,1] <- i
    
    SynthTerms[i,3] <- i
    
    SynthSub1 <- MFS1[MFS1[,"IDcol"] %in% i,]
    
    SynthSub2 <- MFS2[MFS2[,"IDcol"] %in% i,]
    
    SynthTerms[i,2] <- SynthSub1[,"Loccol"][length(SynthSub1[,"Loccol"])]
    
    SynthTerms[i,4] <- SynthSub2[,"Loccol"][length(SynthSub2[,"Loccol"])]
  }
  cat("Terminal locations of surrogate fish found \n")
  
  cat(dim(SynthTerms)," \n")
  TermPairs <- matrix(data = NA, nrow = length(FishPairSet[,1]),
                      ncol = 2)
  for(i in 1:length(FishPairSet[,1])){
    TermPairs[i,1] <- FishTerm[match(FishPairSet[i,1],FishTerm[,1]),2]
    TermPairs[i,2] <- FishTerm[match(FishPairSet[i,2],FishTerm[,1]),2]
  }
  #cat(TermPairs, " \n")
  cat("Beginning LCS calculations \n")
  
  LCSComp<-list()
  LCSComp[[1]] <- NA
  
  LCSTab<-matrix(data = NA, ncol = 2)
  
  SynchSigOut<-data.frame()
  
  for(i in 1:length(TermPairs[,1])){
    SynchSigOut[i,1] <- FishPairSet[i,1]
    SynchSigOut[i,2] <- FishPairSet[i,2]
    
    # cat(TermPairs[i,1], "\n")
    # cat(TermPairs[i,2], "\n")
    # cat(match(TermPairs[i,1],LCSTab[,1]), "\n")
    # cat(match(TermPairs[i,2],LCSTab[,2]), "\n")
    # cat(match(match(TermPairs[i,1],LCSTab[,1]),
    #           match(TermPairs[i,2],LCSTab[,2])), "\n")
    
    if(is.na( (match(TermPairs[i,1] ,LCSTab[,1]) && match(TermPairs[i,2],LCSTab[,2])))){
      
      ListKey <- NA 
      cat(ListKey, "\n")
      } else {
        
    ListKey <- which(LCSTab[,1] %in% TermPairs[i,1]) [which(LCSTab[,1] %in% TermPairs[i,1]) %in%
                             which(LCSTab[,2] %in% TermPairs[i,2])]
    cat(ListKey, "\n")
      }
    cat(ListKey, "\n")
    #cat(ListKey, "\n")
    if(is.na(ListKey)){
      # Append a new row to LCSTab
      LCSTab <- rbind(LCSTab, c(TermPairs[i,1],TermPairs[i,2]))
      
      ListKey<-length(LCSTab[,1])
      # Begin the nightmare that is the LCS testing process

      
      cat("Beginning an LCS synchrony evaluation block \n")
      F1Set <- which(SynthTerms[,2] %in% TermPairs[i,1])
      cat("F1Set is ", length(F1Set), " long \n")

      F2Set <- which(SynthTerms[,4] %in% TermPairs[i,2])

      #scan()
      PairBlock <- matrix(data = NA, nrow = length(F1Set)*length(F2Set), ncol = 2)
      PairDex <- 1
      for(k in 1:length(F1Set)){
        for(j in 1:length(F2Set)){
          PairBlock[PairDex,1] <- F1Set[k]
          PairBlock[PairDex,2] <- F2Set[j]
          PairDex <- PairDex + 1
        }  
      }
      
      LDVec<-vector(mode = "numeric", length = Reps)
      PSVec<-vector(mode = "numeric", length = Reps)
      LSVec<-vector(mode = "numeric", length = Reps)
      #cat("The surrogate pairs are: ", PairBlock, " \n")
      cat("There are ", dim(PairBlock)[1], " surrogate pairs \n")
      #cat("Abandon all hope, ye who run code past here \n")
      #scan()
      Printdex <- 1
      #cat(PairBlock)
      Testkey <- sample(1:length(PairBlock[,1]), Reps)
     
      for(j in 1:Reps){
        #scan()
        cat("The terminal locations are:", TermPairs[i,], "\n")
        cat("The index for the subset of surrogate pairs is:", Testkey[j], "\n")
        MFS1Sub <- MFS1[which(MFS1[,"IDcol"] %in% PairBlock[Testkey[j],1]),]
        #scan()
        #typeof(MFS1Sub)
        #cat(MFS1Sub, "\n")
        #scan()
        MFS2Sub <- MFS2[which(MFS2[,"IDcol"] %in% PairBlock[Testkey[j],2]),]
        #cat(MFS2Sub, "\n")
        
        #scan()
        ReducedBlock <- LCSExtract(MFS1Sub[,"Loccol"],
                                        MFS2Sub[,"Loccol"],
                                        MFS1Sub[,"T1col"],
                                        MFS1Sub[,"T2col"],
                                        MFS2Sub[,"T1col"],
                                        MFS2Sub[,"T2col"],c("All"))
        LDVec[j]<-ReducedBlock[[7]]
        PSVec[j]<-ReducedBlock[[14]]
        LSVec[j]<-ReducedBlock[[21]]
        
        cat("The completed run #: ", Printdex, "\n")
        Printdex <- Printdex+1
      }
      
      SynchFrame<-data.frame(LD = LDVec, PS = PSVec, LS = LSVec)
      
      
      LCSComp[[ListKey]] <- SynchFrame
      
      cat(ListKey - 1, " pairs of terminal locations have been evaluated \n")
      names(LCSComp) <- 1:ListKey
    }
    
    #cat(LCSList[[i]][[7]], "\n")
    #cat(LCSComp[[ListKey]][,"LD"], "\n")
    #cat(which(LCSComp[[ListKey]][,"LD"] < LCSList[[i]][[7]]), "\n")

    # print(dim(LCSComp))
    # print(dimnames(LCSComp))
    # print(LCSComp[[ListKey]][,"LD"])
    # print(names(LCSComp))
     # print(LCSComp[[ListKey]])
     # print(ListKey)
     # scan()
    #print(names(LCSComp))
    #print(LCSComp[[as.character(ListKey)]])
    #scan()
    #print(length(which(LCSComp[[ListKey]][,"LD"] < LCSList[[i]][[7]])))
    #scan()
    SynchSigOut[i,3] <- length(which(LCSComp[[ListKey]][,"LD"] < LCSList[[i]][[7]]))
    #scan()
    SynchSigOut[i,4] <- length(LCSComp[[ListKey]][,"LD"])
    #scan()
    SynchSigOut[i,5] <- length(which(LCSComp[[ListKey]][,"PS"] < LCSList[[i]][[14]]))
    SynchSigOut[i,6] <- length(LCSComp[[ListKey]][,"PS"])
    
    SynchSigOut[i,7] <- length(which(LCSComp[[ListKey]][,"LS"] < LCSList[[i]][[21]]))
    SynchSigOut[i,8] <- length(LCSComp[[ListKey]][,"LS"])
    
    cat(i, " pairs of fish have been evaluated. \n")
    #scan()
  }
  
  colnames(SynchSigOut)<-c("Fish1","Fish2","LD Under",
                           "LD Reps","PS Under","PS Reps",
                           "LS Under","LS Reps")
  return(SynchSigOut)
}








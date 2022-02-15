# Steelhead smolt LCS stuff

RelApp <- function(DetTable, DetCol, FishCol, RelCol, LocCol){
  FishSet<-unique(DetTable[,FishCol])
  FishSet<-FishSet[-which(is.na(FishSet))]
  for(i in 1:FishSet){
    
  }
}

unique(SHK[,c(4,12)])

SHsub<-SHK[which(! SHK$StudyID %in% c("EBMUD-01", "USFWS-BN-01", "USFWS-BN-02", 
                                      "USFWS-BN-03", "DWR-FR-STH-01")),]

GGBvec<-c("Golden Gate West Line","Golden Gate East Line")

GGBdex<-which(SHsub$General_Location %in% GGBvec)

SHsub[GGBdex,"General_Location"]<-"Golden Gate and Ocean"

unique(SHsub[,c(4,12)])


StudyFish<-table(unique(SHsub[,c(4,2)])[,1])

DateFish<-table(unique(SHsub[,c(10,2)])[,1])

TestRel1<-"2007-12-14 18:05:00.000"

SHTest1<-SHsub[which(SHsub$Date_Released %in% TestRel1),]
#unique(SHTest1$FishID)
SHTest1Dense<-DetCondense(SHTest1, unique(SHTest1$FishID), "General_Location", "DetectDate", 
                          "FishID")

SHTestPairs<-data.frame(data = NA)
PairDex<-1
SHTest1PairChain<-unique(SHTest1Dense$FishID)
SHTest1PairChain<-SHTest1PairChain[which(!is.na(SHTest1PairChain))]
for(i in 1:(length(SHTest1PairChain)-1)){
  for(j in (i+1):length(SHTest1PairChain)){
    SHTestPairs[PairDex,1] <- SHTest1PairChain[i]
    SHTestPairs[PairDex,2] <- SHTest1PairChain[j]
    PairDex <- PairDex + 1
  }  
}
SHTestPairs<-SHTestPairs[which(!is.na(SHTestPairs[,2])),]


SHTest1Red<-list()

SHTest1Dense$FirstDet<-as.numeric(as.POSIXct(SHTest1Dense$FirstDet))
SHTest1Dense$LastDet<-as.numeric(as.POSIXct(SHTest1Dense$LastDet))

# print(FWS07Pairs[23,])
Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

Printdex <- 1


for(i in 1:length(SHTestPairs[,1])){
  cat("The fish pair: ", SHTestPairs[i,1]," ", SHTestPairs[i,2], "\n")
  
  cat("The LCS info: \n")

  SHTest1Red[[i]]<-LCSExtract(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,1]),"Loc"],
                                SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,2]),"Loc"],
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,1]),"FirstDet"]),
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,1]),"LastDet"]),
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,2]),"FirstDet"]),
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,2]),"LastDet"]),
                                c("All"))
  cat("The completed run #: ", Printdex, "\n")
  
  Printdex <- Printdex+1
  
}

LCSlens<-numeric(0)
LDSyn<-numeric(0)
for(i in 1:length(SHTestPairs[,1])){
  LCSlens<-c(LCSlens,length(SHTest1Red[[i]][[2]]))
  LDSyn<-c(LDSyn, (as.numeric(SHTest1Red[[i]][[7]])))
}

SHTestMTable<-MarkovFishTable(SHTest1Dense, "FishID", "Loc", "FirstDet","LastDet")
SHTestSurr<-MarkovFishSurrogates(SHTestMTable, InitPos = "SR_BlwOrd", 
                                 InitTime = 0, nFish = 10000, Reps = 1)










SHTestLDMat<-matrix(nrow = 20, ncol = 20)
rownames(SHTestLDMat) <- SHTest1PairChain
colnames(SHTestLDMat) <- SHTest1PairChain
SHTestLSMat <- SHTestLDMat
SHTestPSMat <- SHTestLDMat
for(i in 1:190){
  LDk1 <- match(SHTestPairs[i,1],SHTest1PairChain)
  LDk2 <- match(SHTestPairs[i,2],SHTest1PairChain)
  
  SHTestLDMat[LDk1,LDk2] <- ( log(as.numeric(SHTest1Red[[i]][[7]])))
  SHTestPSMat[LDk1,LDk2] <- ( log(as.numeric(SHTest1Red[[i]][[14]])))
  SHTestLSMat[LDk1,LDk2] <- ( log(as.numeric(SHTest1Red[[i]][[21]])))
  
  SHTestLDMat[LDk2,LDk1] <- ( log(as.numeric(SHTest1Red[[i]][[7]])))
  SHTestPSMat[LDk2,LDk1] <- ( log(as.numeric(SHTest1Red[[i]][[14]])))
  SHTestLSMat[LDk2,LDk1] <- ( log(as.numeric(SHTest1Red[[i]][[21]])))
}

TestLDClustOrd <- order.dendrogram(as.dendrogram(hclust(as.dist(SHTestLDMat))))

TestPSClustOrd <- order.dendrogram(as.dendrogram(hclust(as.dist(SHTestPSMat))))

TestLSClustOrd <- order.dendrogram(as.dendrogram(hclust(as.dist(SHTestLSMat))))


SHTestLDMatOrdered <- matrix(nrow = 20, ncol = 20)
rownames(SHTestLDMatOrdered) <- SHTest1PairChain[TestLDClustOrd]
colnames(SHTestLDMatOrdered) <- SHTest1PairChain[TestLDClustOrd]
SHTestLSMatOrdered <- SHTestLDMatOrdered
rownames(SHTestLSMatOrdered) <- SHTest1PairChain[TestLSClustOrd]
colnames(SHTestLSMatOrdered) <- SHTest1PairChain[TestLSClustOrd]
SHTestPSMatOrdered <- SHTestLDMatOrdered
rownames(SHTestPSMatOrdered) <- SHTest1PairChain[TestPSClustOrd]
colnames(SHTestPSMatOrdered) <- SHTest1PairChain[TestPSClustOrd]

SHTestLDMatOrderedInt <- matrix(nrow = 20, ncol = 20)
SHTestLSMatOrderedInt <- SHTestLDMatOrderedInt
SHTestPSMatOrderedInt <- SHTestLDMatOrderedInt

for(i in 1:23){
  SHTestLDMatOrderedInt[i,] <- SHTestLDMat[TestLDClustOrd[i],]
  SHTestPSMatOrderedInt[i,] <- SHTestPSMat[TestPSClustOrd[i],]
  SHTestLSMatOrderedInt[i,] <- SHTestLSMat[TestLSClustOrd[i],]
}
for(i in 1:23){
  SHTestLDMatOrdered[,i] <- SHTestLDMatOrderedInt[,TestLDClustOrd[i]]
  SHTestPSMatOrdered[,i] <- SHTestPSMatOrderedInt[,TestPSClustOrd[i]]
  SHTestLSMatOrdered[,i] <- SHTestLSMatOrderedInt[,TestLSClustOrd[i]]
}
image(SHTestLDMatOrdered)
image(SHTestPSMatOrdered)
image(SHTestLSMatOrdered)

STHTest1CppDebug<-function(index){
  cat("std::vector<std::string> Fish1Seq_ {", paste(shQuote(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[index,1]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ1")
  cat("std::vector<std::string> Fish2Seq_ {", paste(shQuote(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[index,2]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ2")
  cat("std::vector<double> TSeqArr1 {", paste(as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[index,1]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ1")
  cat("std::vector<double> TSeqDep1 {", paste(as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[index,1]),"LastDet"]), 
                                              collapse =", "), "};\n")
  #print("END DEPSEQ1")
  cat("std::vector<double> TSeqArr2 {", paste(as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[index,2]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ2")
  cat("std::vector<double> TSeqDep2 {", paste(as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[index,2]),"LastDet"]), 
                                              collapse =", "), "};\n")
}


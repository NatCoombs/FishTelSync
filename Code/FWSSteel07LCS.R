# LCS's for the FWS07 release,  but again





FWS07200Raw<-SHK[which(SHK$FishID %in% USFWS200Rkm07$FishID),]


FWS07200LocDense<-DetCondense(DetFrame = FWS07200Raw, FishID = USFWS200Rkm07$FishID, LocCol = "General_Location",tCol = "DetectDate",
            IDCol = "FishID")

FWS07200LocDense$FirstDet<-as.POSIXct(FWS07200LocDense$FirstDet)
FWS07200LocDense$LastDet<-as.POSIXct(FWS07200LocDense$LastDet)

LCSDemo1<-DetReducer(DenseFrame = FWS07200LocDense, FishSet = c("STH0452","STH0461"), fCol = "FishID", 
                     t1Col = "FirstDet", t2Col = "LastDet",SiteCol = "Loc")

Reduced07200SHKSet<-list()

LCSDemo0<-LCSExtract(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"Loc"],
                     FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0461"),"Loc"],
                     as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"FirstDet"]),
                     as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"LastDet"]),
                     as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0461"),"FirstDet"]),
                     as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0461"),"LastDet"]))

cat(paste(shQuote(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"Loc"], 
                  type = "cmd"), collapse =", "))
cat(paste(shQuote(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0461"),"Loc"], 
                  type = "cmd"), collapse =", "))

cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"FirstDet"]), 
                  collapse =", "))
cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"LastDet"]), 
                   collapse =", "))
cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0461"),"FirstDet"]), 
                   collapse =", "))
cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0461"),"LastDet"]), 
                   collapse =", "))

print(FWS07200LocDense[which(FWS07200LocDense$FishID %in% "STH0452"),"Loc"])
# It took ____ time to finish running the LCS and selection
FWS07200CppDebug<-function(index){
  cat(paste(shQuote(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[index,1]),"Loc"], 
                    type = "cmd"), collapse =", "))
  print("END FSEQ1")
  cat(paste(shQuote(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[index,2]),"Loc"], 
                    type = "cmd"), collapse =", "))
  print("END FSEQ2")
  cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[index,1]),"FirstDet"]), 
            collapse =", "))
  print("END ARRSEQ1")
  cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[index,1]),"LastDet"]), 
            collapse =", "))
  print("END DEPSEQ1")
  cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[index,2]),"FirstDet"]), 
            collapse =", "))
  print("END ARRSEQ2")
  cat(paste(as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[index,2]),"LastDet"]), 
            collapse =", "))
  print("END DEPSEQ2")
}

#Reduced07200SHKSet[[1]]<-LCSDemo0
Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

Printdex <- 1
for(i in 1:10){
  Reduced07200SHKSet[[i]]<-LCSExtract(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"Loc"],
                                      FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"Loc"],
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"LastDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"LastDet"]),
                                      c("All"))
  print(Printdex)
  Printdex <-Printdex+1
}
Reduced07200SHKSet[[11]]<- "Memory pressure issue"
Printdex<-12
for(i in 12:13){
  Reduced07200SHKSet[[i]]<-LCSExtract(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"Loc"],
                                      FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"Loc"],
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"LastDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"LastDet"]),
                                      c("All"))
  print(Printdex)
  Printdex <-Printdex+1
}
Reduced07200SHKSet[[14]]<- "Taking too long, running in C++"
Reduced07200SHKSet[[15]]<- "Taking too long, running in C++"
Printdex<-16
for(i in 16:35){
  Reduced07200SHKSet[[i]]<-LCSExtract(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"Loc"],
                                      FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"Loc"],
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"LastDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"LastDet"]),
                                      c("All"))
  print(Printdex)
  Printdex <-Printdex+1
}





for(i in c(1:10,12:13)){
  names(Reduced07200SHKSet[[i]])<-c("Loc","F1Arr","F1Dep","F2Arr","F2Dep","EuclidDist")
}

ReducedDemoPlot<-list()
RedDex<-1
for(i in c(1:10,12:13)){
  Pairmat<-matrix(data = NA, ncol = 4, nrow = 2 * length(Reduced07200SHKSet[[i]][[1]]))
  for(j in 1:length(Reduced07200SHKSet[[i]][[1]])){
    Pairmat[2*j-1,1]<-Reduced07200SHKSet[[i]][[1]][j]
    Pairmat[2*j,1]<-Reduced07200SHKSet[[i]][[1]][j]
    Pairmat[2*j-1,2]<-Reduced07200SHKSet[[i]][[2]][j]
    Pairmat[2*j,2]<-Reduced07200SHKSet[[i]][[3]][j]
    Pairmat[2*j-1,3]<-Reduced07200SHKSet[[i]][[4]][j]
    Pairmat[2*j,3]<-Reduced07200SHKSet[[i]][[5]][j]
    Pairmat[2*j-1,4]<-as.character(((as.numeric(Pairmat[2*j-1,2])-as.numeric(Pairmat[2*j-1,3]))^2)/2)
    Pairmat[2*j,4]<-as.character(((as.numeric(Pairmat[2*j,2])-as.numeric(Pairmat[2*j,3]))^2)/2)
  }
  Pairmat<-as.data.frame(Pairmat)
  Pairmat[,2]<-as.numeric(Pairmat[,2])/86400
  Pairmat[,3]<-as.numeric(Pairmat[,3])/86400
  Pairmat[,4]<-log(as.numeric(Pairmat[,4])/86400)
  ReducedDemoPlot[[RedDex]]<-Pairmat
  RedDex<-RedDex+1
    
}
NameVec<-PlotSet[c(1:10,12:13),]
for(i in 1:length(ReducedDemoPlot)){
  plot(x = ReducedDemoPlot[[i]][,2], y = ReducedDemoPlot[[i]][,4], type = "l", col = "red", 
       xlim = c(min(c(ReducedDemoPlot[[i]][,2],ReducedDemoPlot[[i]][,3])),max(c(ReducedDemoPlot[[i]][,2],ReducedDemoPlot[[i]][,3]))))
  lines(x = ReducedDemoPlot[[i]][,3], y = ReducedDemoPlot[[i]][,4], type = "l", col = "dark green")
  title(main = i)
}
# Might also be sticking on 15
#Sticking on 14.
# Let's test 11 and 14 in C++ for now.
# The 11th one EATS memory.
Reduced07200SHKSet<-list()

Printdex <- 1


for(i in 1:35){
  Reduced07200SHKSet[[i]]<-LCSExtract(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"Loc"],
                                      FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"Loc"],
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,1]),"LastDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"FirstDet"]),
                                      as.numeric(FWS07200LocDense[which(FWS07200LocDense$FishID %in% PlotSet[i,2]),"LastDet"]),
                                      c("All"))
  print(Printdex)
  Printdex <-Printdex+1
}








FWSSHK<-SHK[which(substr(SHK$StudyID,1,5) %in% "USFWS"),]
FWSSHK<-FWSSHK[which(!is.na(FWSSHK$FishID)),]
FWS07Raw<-FWSSHK[which(substr(FWSSHK$Date_Released,1,4) %in% "2007"),]

GGBvec<-c("Golden Gate West Line","Golden Gate East Line")

GGBdex<-which(FWS07Raw$General_Location %in% GGBvec)

FWS07Raw[GGBdex,"General_Location"]<-"Golden Gate and Ocean"

FWS07LocDense<-DetCondense(DetFrame = FWS07Raw, FishID = unique(FWS07Raw$FishID), LocCol = "General_Location",tCol = "DetectDate",
                              IDCol = "FishID")
FWS07Pairs<-data.frame(data = NA)
PairDex<-1
FWS07PairChain<-unique(FWS07LocDense$FishID)
FWS07PairChain<-FWS07PairChain[1:23]
for(i in 1:(length(FWS07PairChain)-1)){
  for(j in (i+1):length(FWS07PairChain)){
    FWS07Pairs[PairDex,1] <- FWS07PairChain[i]
    FWS07Pairs[PairDex,2] <- FWS07PairChain[j]
    PairDex <- PairDex + 1
  }  
}

#which(is.na(FWS07Pairs[,1]))

FWS07Pairs<-FWS07Pairs[which(!is.na(FWS07Pairs[,2])),]

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

ReducedFWS07<-list()

FWS07LocDense$FirstDet<-as.numeric(as.POSIXct(FWS07LocDense$FirstDet))
FWS07LocDense$LastDet<-as.numeric(as.POSIXct(FWS07LocDense$LastDet))

# print(FWS07Pairs[23,])

Printdex <- 1

# Now 84 is giving me trouble.
for(i in 1:length(FWS07Pairs[,1])){
  cat("The fish pair: ", FWS07Pairs[i,1]," ", FWS07Pairs[i,2], "\n")

  cat("The LCS info: \n")
  ReducedFWS07[[i]]<-LCSExtract(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[i,1]),"Loc"],
                                FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[i,2]),"Loc"],
                                      as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[i,1]),"FirstDet"]),
                                      as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[i,1]),"LastDet"]),
                                      as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[i,2]),"FirstDet"]),
                                      as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[i,2]),"LastDet"]),
                                      c("All"))
  cat("The completed run #: ", Printdex, "\n")
  Printdex <- Printdex+1
}

LCSlens<-numeric(0)
LDSyn<-numeric(0)
for(i in 1:253){
  LCSlens<-c(LCSlens,length(ReducedFWS07[[i]][[2]]))
  LDSyn<-c(LDSyn, (as.numeric(ReducedFWS07[[i]][[7]])))
}


FWS07LDMat<-matrix(nrow = 23, ncol = 23, data = 0)
rownames(FWS07LDMat) <- FWS07PairChain
colnames(FWS07LDMat) <- FWS07PairChain
FWS07LSMat <- FWS07LDMat
FWS07PSMat <- FWS07LDMat
for(i in 1:253){
  LDk1 <- match(FWS07Pairs[i,1],FWS07PairChain)
  LDk2 <- match(FWS07Pairs[i,2],FWS07PairChain)
  
  FWS07LDMat[LDk1,LDk2] <- ( (as.numeric(ReducedFWS07[[i]][[7]])))
  FWS07PSMat[LDk1,LDk2] <- ( (as.numeric(ReducedFWS07[[i]][[14]])))
  FWS07LSMat[LDk1,LDk2] <- ( (as.numeric(ReducedFWS07[[i]][[21]])))
  
  FWS07LDMat[LDk2,LDk1] <- ( (as.numeric(ReducedFWS07[[i]][[7]])))
  FWS07PSMat[LDk2,LDk1] <- ( (as.numeric(ReducedFWS07[[i]][[14]])))
  FWS07LSMat[LDk2,LDk1] <- ( (as.numeric(ReducedFWS07[[i]][[21]])))
}
library(wsyn)
cluseigen(FWS07LDMat)
cluseigen(FWS07PSMat)
cluseigen(FWS07LSMat)

TestLDClustOrd <- order.dendrogram(as.dendrogram(hclust(as.dist(FWS07LDMat))))

TestPSClustOrd <- order.dendrogram(as.dendrogram(hclust(as.dist(FWS07PSMat))))

TestLSClustOrd <- order.dendrogram(as.dendrogram(hclust(as.dist(FWS07LSMat))))


FWS07LDMatOrdered <- matrix(nrow = 23, ncol = 23)
rownames(FWS07LDMatOrdered) <- FWS07PairChain[TestLDClustOrd]
colnames(FWS07LDMatOrdered) <- FWS07PairChain[TestLDClustOrd]
FWS07LSMatOrdered <- FWS07LDMatOrdered
rownames(FWS07LSMatOrdered) <- FWS07PairChain[TestLSClustOrd]
colnames(FWS07LSMatOrdered) <- FWS07PairChain[TestLSClustOrd]
FWS07PSMatOrdered <- FWS07LDMatOrdered
rownames(FWS07PSMatOrdered) <- FWS07PairChain[TestPSClustOrd]
colnames(FWS07PSMatOrdered) <- FWS07PairChain[TestPSClustOrd]

FWS07LDMatOrderedInt <- matrix(nrow = 23, ncol = 23)
FWS07LSMatOrderedInt <- FWS07LDMatOrderedInt
FWS07PSMatOrderedInt <- FWS07LDMatOrderedInt

for(i in 1:23){
  FWS07LDMatOrderedInt[i,] <- FWS07LDMat[TestLDClustOrd[i],]
  FWS07PSMatOrderedInt[i,] <- FWS07PSMat[TestPSClustOrd[i],]
  FWS07LSMatOrderedInt[i,] <- FWS07LSMat[TestLSClustOrd[i],]
}
for(i in 1:23){
  FWS07LDMatOrdered[,i] <- FWS07LDMatOrderedInt[,TestLDClustOrd[i]]
  FWS07PSMatOrdered[,i] <- FWS07PSMatOrderedInt[,TestPSClustOrd[i]]
  FWS07LSMatOrdered[,i] <- FWS07LSMatOrderedInt[,TestLSClustOrd[i]]
}
image(FWS07LDMatOrdered)
title(main = "FWS 2007 kelts - least distance")
image(FWS07PSMatOrdered)
title(main = "FWS 2007 kelts - phase similarity")
image(FWS07LSMatOrdered)
title(main = "FWS 2007 kelts - least step difference")

FWS07MTable<-MarkovFishTableSteel(FWS07LocDense,"FishID", "Loc", "FirstDet","LastDet", "Golden Gate and Ocean")

FWS07Surrs<-MarkovFishSurrogatesSteel(FWS07MTable, InitPos = "Battle_Ck", 
                                 InitTime = 0, nFish = 1000, Reps = 2, "Golden Gate and Ocean")

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

FWS07SigTest<-MFSig(FWS07Pairs, FWS07LocDense, ReducedFWS07, FWS07Surrs[[1]], FWS07Surrs[[2]], Reps = 1000)

typeof(FWS07Surrs[[1]][,5])


FWS07CppDebug<-function(index){
  cat("std::vector<std::string> Fish1Seq_ {", paste(shQuote(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[index,1]),"Loc"], 
                    type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ1")
  cat("std::vector<std::string> Fish2Seq_ {", paste(shQuote(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[index,2]),"Loc"], 
                    type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ2")
  cat("std::vector<double> TSeqArr1 {", paste(as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[index,1]),"FirstDet"]), 
            collapse =", "), "};\n")
  #print("END ARRSEQ1")
  cat("std::vector<double> TSeqDep1 {", paste(as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[index,1]),"LastDet"]), 
            collapse =", "), "};\n")
  #print("END DEPSEQ1")
  cat("std::vector<double> TSeqArr2 {", paste(as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[index,2]),"FirstDet"]), 
            collapse =", "), "};\n")
  #print("END ARRSEQ2")
  cat("std::vector<double> TSeqDep2 {", paste(as.numeric(FWS07LocDense[which(FWS07LocDense$FishID %in% FWS07Pairs[index,2]),"LastDet"]), 
            collapse =", "), "};\n")
  #print("END DEPSEQ2")
}

FWS07CppDebug(1)

SigDebugPull<-function(MFS1,MFS2,Term1,Term2,Index){
  
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
  
  F1Set <- which(SynthTerms[,2] %in% Term1)
  F2Set <- which(SynthTerms[,4] %in% Term2)
  
  PairBlock <- matrix(data = NA, nrow = length(F1Set)*length(F2Set), ncol = 2)
  PairDex <- 1
  for(k in 1:length(F1Set)){
    for(j in 1:length(F2Set)){
      PairBlock[PairDex,1] <- F1Set[k]
      PairBlock[PairDex,2] <- F2Set[j]
      PairDex <- PairDex + 1
    }  
  }
  
  MFS1Sub <- MFS1[which(MFS1[,"IDcol"] %in% PairBlock[Index,1]),]
  MFS2Sub <- MFS2[which(MFS2[,"IDcol"] %in% PairBlock[Index,2]),]
  
  cat("std::vector<std::string> Fish1Seq_ {", paste(shQuote(MFS1Sub[,"Loccol"], type = "cmd"), collapse = ", "), "}; \n")
  cat("std::vector<std::string> Fish2Seq_ {", paste(shQuote(MFS2Sub[,"Loccol"], type = "cmd"), collapse = ", "), "}; \n")
  
  cat("std::vector<double> TSeqArr1 {", paste(as.numeric(MFS1Sub[,"T1col"]), 
                                              collapse = ", "), "}; \n")
  cat("std::vector<double> TSeqDep1 {", paste(as.numeric(MFS1Sub[,"T2col"]), 
                                            collapse = ", "), "}; \n")
  
  cat("std::vector<double> TSeqArr2 {", paste(as.numeric(MFS2Sub[,"T1col"]), 
                                              collapse = ", "), "}; \n")
  cat("std::vector<double> TSeqDep2 {", paste(as.numeric(MFS2Sub[,"T2col"]), 
                                              collapse = ", "), "}; \n")
  
# This will serve as a void function and cat out what we want it to instead
}

SigDebugPull(FWS07Surrs[[1]],FWS07Surrs[[2]],"Battle_Ck","SR_AbvBattleCk",630)


SigDebugPull(FWS07Surrs[[1]],FWS07Surrs[[2]],"Golden Gate and Ocean","Benicia Bridge",10694)

SigDebugPull(FWS07Surrs[[1]],FWS07Surrs[[2]],"Golden Gate and Ocean","Benicia Bridge",660)

SigDebugPull(FWS07Surrs[[1]],FWS07Surrs[[2]],"Golden Gate and Ocean","Benicia Bridge",3684)


Test<-matrix(data = NA, ncol = 2)
Test
Test<-rbind(Test, c(1,2))


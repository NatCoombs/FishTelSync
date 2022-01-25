# Steelhead smolt LCS stuff


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
  print("The fish pair:")
  print(c(SHTestPairs[i,1],SHTestPairs[i,2]))
  print("The LCS info:")
  SHTest1Red[[i]]<-LCSExtract(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,1]),"Loc"],
                                SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,2]),"Loc"],
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,1]),"FirstDet"]),
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,1]),"LastDet"]),
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,2]),"FirstDet"]),
                                as.numeric(SHTest1Dense[which(SHTest1Dense$FishID %in% SHTestPairs[i,2]),"LastDet"]),
                                c("All"))
  print("The completed run #:")
  print(Printdex)
  Printdex <- Printdex+1
  
}

SHTestMTable<-MarkovFishTable(SHTest1Dense, "FishID", "Loc", "FirstDet","LastDet")
SHTestSurr<-MarkovFishSurrogates(SHTestMTable, InitPos = "SR_BlwOrd", 
                                 InitTime = 0, nFish = 49, Reps = 1000)

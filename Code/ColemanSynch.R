# This script is for going through and evaluating synchrony for the Coleman releases
ColemanDets<-read.csv("Data/ColemanRel.csv")
Rels<-read.csv("Data/Releases.csv")
FalseDets<-read.csv("Data/Cull.csv")
Locs69kHz<-read.csv("Data/69kHzLocs.csv")


# Also make a table pairing location and general location for this




SHKLocs<-unique(SHK$DetectionLocation)
ColeLocs<-unique(ColemanDets$Location_name)

GenLocTable<-matrix(data = NA, nrow = length(SHKLocs), ncol = 2)
for(i in 1:length(SHKLocs)){
  for(j in 1:length(SHK[,1])){
    if(SHK$DetectionLocation[j] %in% SHKLocs[i]){
      GenLocTable[i,1] <- SHK$General_Location[j]
      GenLocTable[i,2] <- SHKLocs[i]
      break
    }
  }
}

ColeLocTable<-matrix(data = NA, nrow = length(ColeLocs), ncol = 4)
colnames(ColeLocTable)<-c("GenLoc","SpecLoc","Lat","Lon")
for(i in 1:length(ColeLocs)){
  ColeLocTable[i,2] <- ColeLocs[i]
  MatchKey<-which(ColeLocs[i] == GenLocTable[,2])
  if(length(MatchKey) > 0){
    ColeLocTable[i,1] <- GenLocTable[MatchKey,1]
  }
}

# Now begins filling in general locations
ColeLocTable[125,1] <- "Vallejo Marina"
ColeLocTable[c(307,295,308),1] <- rep_len("Tracy Fish Fac",3)
ColeLocTable[300,1] <- "ThreeMile"
ColeLocTable[c(397,302),1] <- rep_len("SR_SutterSteam", 2)
ColeLocTable[c(7,305,303,297),1] <- rep_len("SR_SutterSl", 4)
ColeLocTable[291,1] <- "SR_SteamboatSl"
ColeLocTable[89,1] <- "Woodson Temp"
ColeLocTable[c(56,62),1] <- "Tower Bridge"
ColeLocTable[c(268,111,267,223,270,271,274,273,275,262,272,269),1] <- rep_len("SR_SRWTPD",12)
ColeLocTable[c(124,127,142,136,141,134),1] <- rep_len("SR_RV",6)
ColeLocTable[c(258,259,260,255,254,256,257,265,264,243,245,241,242,244,248,246,250,247,249,252,253,251,124,127,142),1] <- rep_len("SR_RM",25)
ColeLocTable[292,1] <- "Riverview Marina"
ColeLocTable[100,1] <- "SR_MeridianBr"
ColeLocTable[91,1] <- "Massacre Flat"
ColeLocTable[322,1] <- "SR_KnightsBr"
ColeLocTable[126,1] <- "SR_KL"
ColeLocTable[c(129,131,132,133),1] <- rep_len("SR_KK",4)
ColeLocTable[c(210,221,222),1] <- rep_len("SR_IstBridge",3)
ColeLocTable[c(130,138,139,140,137,135),1] <- rep_len("SR_GB",6)
ColeLocTable[372,1] <- "SR_ElkSlough"
ColeLocTable[128,1] <- "SR_EH"
ColeLocTable[293,1] <- "SR_DCC"
ColeLocTable[311,1] <- "SR_I-80/50Br"
ColeLocTable[97,1] <- "SR_BoulderHole"
ColeLocTable[98,1] <- "SR_SaltCk"
ColeLocTable[288,1] <- "SR_BlwGeorgiana"
ColeLocTable[88,1] <- "SR_BlwAntelopeCk"
ColeLocTable[12,1] <- "SR_BlwAmerican"
ColeLocTable[c(209,213,218,215,212,211,214,217,366),1] <- rep_len("Battle_Ck",9)
ColeLocTable[96,1] <- "SR_AbvToomes"
ColeLocTable[95,1] <- "SR_AbvTisdale"
ColeLocTable[87,1] <- "SR_AbvGCID"
ColeLocTable[c(16,381),1] <- rep_len("SR_AbvFeather",2)
ColeLocTable[92,1] <- "SR_AbvAntelopeCk"
ColeLocTable[c(299,294),1] <- rep_len("SR_AbvAmerican",2)
ColeLocTable[c(224,226,228,229,231,232,235,115),1] <- rep_len("SP_Flats_Array",8)
ColeLocTable[c(167,182,176,187,147,121,179,44),1] <- rep("SP_Control",8)
ColeLocTable[c(184,189,180,202),1] <- rep_len("SP_Buoy", 4)
ColeLocTable[c(168,169,170,200,112,174,183,168,173,171,178,34,186,144,188,204,148),1] <- rep_len("SP_Array",17)
ColeLocTable[109,1] <- "SJ_JerseyPoint"
ColeLocTable[c(175,120,145,177),1] <- rep_len("SF9",4)
ColeLocTable[c(204,117,113,152),1] <- rep_len("SF_Control",4)
ColeLocTable[82,1] <- "SandMound"
ColeLocTable[c(39,225,263,151,234,152,119,154,146,227,238,239,237,223,230,240,236,150,195,192,190,196,197,199,193,194,191,155,143),1] <- rep_len("Richmond Bridge",29)
ColeLocTable[c(114,116),1] <- rep_len("Raccoon", 2)
ColeLocTable[78,1] <- "Quimby"
ColeLocTable[c(281,283,285,286,276,284,278,279,280,277,282),1] <- rep_len("Pt_Reyes",11)
ColeLocTable[93,1] <- "Potato Slough"
ColeLocTable[79,1] <- "OR_DiscovBay"
ColeLocTable[38,1] <- "Montezuma"
ColeLocTable[c(83,85),1] <- rep_len("Miner Slough",2)
ColeLocTable[c(24,77),1] <- rep_len("MidR",2)
ColeLocTable[94,1] <- "LittleConnection"
ColeLocTable[290,1] <- "Georg_SloughN"
ColeLocTable[c(27,29),1] <- rep_len("FR_Verona",2)
ColeLocTable[c(28,208),1] <- "FR_Beer_Can"
ColeLocTable[3,1] <- "Decker_IsN"
ColeLocTable[357,1] <- "Clifton Court"
ColeLocTable[c(107,31,341,345,347,385,81,5),1] <- rep_len("Chipps Island",8)
ColeLocTable[291,1] <- "CarSt"
ColeLocTable[c(166,185),1] <- rep_len("Carquinez",2)
ColeLocTable[90,1] <- "Benicia Bridge"
ColeLocTable[c(149,36,156,158,203,161,157,159,163,172,162,165,160,164,198),1] <- "Bay Bridge"
ColeLocTable[181,1] <- "AlcatrazSE"
ColeLocTable[118,1] <- "AlcatrazNW"
ColeLocTable[205,1] <- "AlcatrazNE"
ColeLocTable[126,1] <- "AlcatrazSW"
ColeLocTable[266,1] <- "Nameless"


GGBvec<-c("Golden Gate West Line","Golden Gate East Line")

GGBdex<-which(ColeLocTable[,1] %in% GGBvec)

ColeLocTable[GGBdex,1]<-"Golden Gate and Ocean"


RepVec<-c("SR_KK")

Repdex<-which(ColeLocTable[,1] %in% RepVec)

ColeLocTable <- ColeLocTable[-Repdex,]





RepVec<-c("Decker_IsN", "Decker_IsS", "Horseshoe_Bend")

Repdex<-which(ColeLocTable[,1] %in% RepVec)

ColeLocTable[Repdex,1]<-"Decker and Horseshoe"


for(i in 1:length(ColeLocTable[,2])){
  Key <- which(Locs69kHz[,1] %in% ColeLocTable[i,2])
  if(length(Key) != 0){
  ColeLocTable[i,3] <- Locs69kHz[Key,2]
  ColeLocTable[i,4] <- Locs69kHz[Key,3]
  }
}

CheckVec <- c("SR_DCC", "SR_KK", "Battle_Ck", "SR_SturgeonHole", "Decker_IsN", "Decker_IsS", "Horseshoe_Bend")

CheckSub <- ColeLocTable[ColeLocTable[,1] %in% CheckVec,]

write.csv(CheckSub,"Data/CoordsForGabe.csv")
write.csv(ColeLocTable, "Data/FullLocs.csv")
ColeLocTable <- read.csv("Data/GenLocsQedFIN.csv")
# Let's check wet years first, then dry years?

Rels<-Rels[order(Rels$Total_tagged_released, decreasing = T),]

ColemanRels<-Rels[which(Rels$Release_location %in% "BtlCkCNFHWeir"),]
# 2007 has plenty of data, but it's split across releases... Maybe another angle to pursue?

CNFHgr30<-ColemanRels[which(ColemanRels$Total_tagged_released > 30),]

CNFHgr30$Water_year<-c("10-11","09-10","10-11","05-06","04-05")
# Resubsetting to remove the steelhead releases

CNFHChin<-CNFHgr30[1:3,]


# Pull the associated subset of detections

ColemanSub<-ColemanDets[which(ColemanDets$Release_date_time %in% CNFHChin$Release_date_time),
                        c("Fish_ID", "Detect_date_time", "Location_name", "Release_date_time")]

ColemanIDs<-list()
ind<-1
for(i in CNFHChin$Release_date_time){
  ColemanIDs[[ind]] <- unique(ColemanSub[which(ColemanSub$Release_date_time == i),"Fish_ID"])
  ind<-ind+1
}



# Prepend releases as detections and then condense like mad
ind<-1
ColemanIDsVec<-vector(mode = "character")
for(i in ColemanIDs){
  for(j in i) {
    ColemanSub<-rbind(ColemanSub,c(j, CNFHChin$Release_date_time[ind], "BtlCkCNFHWeir"))
    ColemanIDsVec<-c(ColemanIDsVec,j)
  }
  ind <- ind + 1
}
GenLocTable<-rbind(GenLocTable, c("ColemanRel","BtlCkCNFHWeir"))
# Let's adjust ColemanSub to match the locations as indicated in the GenLocTable

for(i in 1:length(ColeLocTable[,2])){
  Site <- ColeLocTable[i,2]
  Key <- which(ColemanSub$Location_name %in% ColeLocTable[i,3])
  ColemanSub$Location_name[Key] <- rep_len(Site, length(Key))
}

# And here's where we'll adjust the one SP_Array site we missed and remove the KK sites.
# As well as overwrite and bin the Decker & Horseshoe sites after all.
KK_Sites <- c("SR_KK345R", "SR_KK269L", "SR_KK250L", "SR_KK240L")
KK_Key <- which(ColemanSub$Location_name %in% KK_Sites)
ColemanSub <- ColemanSub[-KK_Key,]

SP_Key <- which(ColemanSub$Location_name == "SP_Array_2F")
ColemanSub$Location_name[SP_Key] <- "SP_Array"

BC3_Key <- which(ColemanSub$Location_name == "BattleCk03")
ColemanSub$Location_name[BC3_Key] <- "Upper_Battle_Ck"

DH_Sites <- c("Decker and Horseshoe SW", "Decker and Horseshoe N","Decker and Horseshoe M")
DH_Key <- which(ColemanSub$Location_name %in% DH_Sites)
ColemanSub$Location_name[DH_Key] <- "Decker and Horseshoe"

CNFHChinDense<-DetCondense(ColemanSub, ColemanIDsVec, "Location_name", "Detect_date_time", "Fish_ID")

# Need to drop "LFC3077" from the second of the releases for now
ProblemFish <- c("LFC3077", "LFC3147", "LFC3093")
ProblemKey <- which(ColemanIDs[[2]] %in% ProblemFish)
ColemanIDs[[2]] <- ColemanIDs[[2]][-ProblemKey]

SacMinTrav<-c("BtlCkCNFHWeir", "Upper_Battle_Ck", "Battle_Ck") 

MTravDrop<-vector(mode = "character")
for(i in unique(CNFHChinDense$FishID)){
  SubDense<-CNFHChinDense[CNFHChinDense$FishID %in% i,]
  
  MTravLocs<-unique(SubDense$Loc)
  
  if(all(MTravLocs %in% SacMinTrav)){
    MTravDrop <- c(MTravDrop, i)
  }
}

for(i in 1:length(ColemanIDs)){
  DropVec <- which(ColemanIDs[[i]] %in% MTravDrop)
  ColemanIDs[[i]] <- ColemanIDs[[i]][-DropVec]
}



ColemanPairs<-list()
for(k in 1:length(ColemanIDs)){
  RelPairs<-data.frame(data = NA)
  PairDex <- 1
  RelFish<-ColemanIDs[[k]]
  for(i in 1:(length(RelFish)-1)){
    for(j in (i+1):length(RelFish)){
      RelPairs[PairDex,1] <- RelFish[i]
      RelPairs[PairDex,2] <- RelFish[j]
      PairDex <- PairDex + 1
    }
  }
  ColemanPairs[[k]] <- RelPairs
}

ColemanPairs[[2]][2118,]
DebugCole(2,2118)

CNFHChinDense$FirstDet<-as.numeric(as.POSIXct(CNFHChinDense$FirstDet))
CNFHChinDense$LastDet<-as.numeric(as.POSIXct(CNFHChinDense$LastDet))
CNFHChinDense<-na.omit(CNFHChinDense)

if(!dir.exists("./Data/SacChinReduced")){
  dir.create("./Data/SacChinReduced")
}
if(!dir.exists("./Data/SacChinReduced/Dets")){
  dir.create("./Data/SacChinReduced/Dets")
}
if(!dir.exists("./Data/SacChinReduced/Pairs")){
  dir.create("./Data/SacChinReduced/Pairs")
}

# 1 is the Dec 2010 release, 2 is the Dec 2009 release, and 3 is the Jan 2011 release. We just ignored 2 in our presenting stuff because it wasn't interesting at the time.
# Write out so we can throw it to the cluster

for(i in 1:length(ColemanPairs)){
  CDets <- CNFHChinDense[CNFHChinDense$FishID %in% ColemanIDs[[i]],]
  
  DropVec <- which(CDets$FirstDet < as.numeric(as.POSIXct(CNFHChin$Release_date_time[i])))
  if(length(DropVec) != 0){
    SubFrame <- SubFrame[-which(SubFrame$FirstDet < RelT),]
  } 
  
  write.csv(CDets, file = paste0("./Data/SacChinReduced/Dets/", i,".csv"), row.names=FALSE)
  write.csv(ColemanPairs[[i]], file = paste0("./Data/SacChinReduced/Pairs/", i,".csv"), row.names=FALSE)
}



source("Code/Functions/LCSCalc.R")
Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

#LCSCalc(CNFHChinDense, ColemanPairs[[2]][1,])


LCSCole101<-LCSCalc(CNFHChinDense, ColemanPairs[[1]])
saveRDS(LCSCole101, "Data/LCSCole101.rds")
LCSCole09<-LCSCalc(CNFHChinDense, ColemanPairs[[2]])
saveRDS(LCSCole09, "Data/LCSCole09.rds")
LCSCole102<-LCSCalc(CNFHChinDense, ColemanPairs[[3]])
saveRDS(LCSCole102, "Data/LCSCole102.rds")
LCSCole102 <- readRDS("Data/LCSCole102.rds")

ColeTable101<-MarkovFishTable(CNFHChinDense[CNFHChinDense$FishID %in% ColemanIDs[[1]],], "FishID", "Loc", "FirstDet","LastDet")
saveRDS(ColeTable101, "Data/ColeTable101.rds")
ColeTable09<-MarkovFishTable(CNFHChinDense[CNFHChinDense$FishID %in% ColemanIDs[[2]],], "FishID", "Loc", "FirstDet","LastDet")
saveRDS(ColeTable09, "Data/ColeTable09.rds")
ColeTable102<-MarkovFishTable(CNFHChinDense[CNFHChinDense$FishID %in% ColemanIDs[[3]],], "FishID", "Loc", "FirstDet","LastDet")
saveRDS(ColeTable102, "Data/ColeTable102.rds")

# Leaving Cole101Surr at 10000 fish for now, but I'll switch it over to 1000 later when I can rerun everything.
Cole101Surr<-MarkovFishSurrogates(ColeTable101, "BtlCkCNFHWeir",0,10000,Reps = 1)
#unique(Cole101Surr[[1]][,"FishVec"])
saveRDS(Cole101Surr, "Data/Cole101Surr.rds")
Cole09Surr<-MarkovFishSurrogates(ColeTable09, "BtlCkCNFHWeir",0,1000,Reps = 1)
saveRDS(Cole09Surr, "Data/Cole09Surr.rds")
Cole102Surr<-MarkovFishSurrogates(ColeTable102, "BtlCkCNFHWeir",0,1000,Reps = 1)
saveRDS(Cole102Surr, "Data/Cole102Surr.rds")
Cole102Surr <- readRDS("Data/Cole102Surr.rds")
#library(wsyn)
Cole101PopTest<-MFPop(LCSCole101, ColemanPairs[[1]], Cole101Surr[[1]],1000)
saveRDS(Cole101PopTest, "Data/Cole101PopTest.rds")
Cole101PopTest <- readRDS("Data/Cole101PopTest.rds")
Cole09PopTest<-MFPop(LCSCole09, ColemanPairs[[2]], Cole09Surr[[1]],1000)
saveRDS(Cole09PopTest, "Data/Cole09PopTest.rds")
Cole102PopTest<-MFPop(LCSCole102, ColemanPairs[[3]], Cole102Surr[[1]],1000)
saveRDS(Cole102PopTest, "Data/Cole102PopTest.rds")
Cole102PopTest <- readRDS("Data/Cole102PopTest.rds")


pdf("Intermediate stuff/Cole101SurrHists.pdf")

par(mfrow = c(3,3))
hist(Cole101PopTest[[3]][[1]][,1], breaks = 10, main = "Surrogate Euclid Mean")
abline(v = Cole101PopTest[[1]][1,1], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[1]][,2], breaks = 10, main = "Surrogate Phase Mean")
abline(v = Cole101PopTest[[1]][1,2], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[1]][,3], breaks = 10, main = "Surrogate Stepwise Mean")
abline(v = Cole101PopTest[[1]][1,3], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[3]][,1], breaks = 10, main = "Surrogate Euclid 90th quantile")
abline(v = Cole101PopTest[[1]][3,1], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[3]][,2], breaks = 10, main = "Surrogate Phase 90th quantile")
abline(v = Cole101PopTest[[1]][3,2], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[3]][,3], breaks = 10, main = "Surrogate Stepwise 90th quantile")
abline(v = Cole101PopTest[[1]][3,3], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[4]][,1], breaks = 10, main = "Surrogate Euclid 10th quantile")
abline(v = Cole101PopTest[[1]][4,1], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[4]][,2], breaks = 10, main = "Surrogate Phase 10th quantile")
abline(v = Cole101PopTest[[1]][4,2], col = "Red", lwd = 2)
hist(Cole101PopTest[[3]][[4]][,3], breaks = 10, main = "Surrogate Stepwise 10th quantile")
abline(v = Cole101PopTest[[1]][4,3], col = "Red", lwd = 2)

dev.off()
par(mfrow=c(1,1))


pdf("Intermediate stuff/Cole09SurrHists.pdf")

par(mfrow = c(3,3))
hist(Cole09PopTest[[3]][[1]][,1], breaks = 10, main = "Surrogate Euclid Mean")
abline(v = Cole09PopTest[[1]][1,1], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[1]][,2], breaks = 10, main = "Surrogate Phase Mean")
abline(v = Cole09PopTest[[1]][1,2], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[1]][,3], breaks = 10, main = "Surrogate Stepwise Mean")
abline(v = Cole09PopTest[[1]][1,3], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[3]][,1], breaks = 10, main = "Surrogate Euclid 90th quantile")
abline(v = Cole09PopTest[[1]][3,1], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[3]][,2], breaks = 10, main = "Surrogate Phase 90th quantile")
abline(v = Cole09PopTest[[1]][3,2], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[3]][,3], breaks = 10, main = "Surrogate Stepwise 90th quantile")
abline(v = Cole09PopTest[[1]][3,3], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[4]][,1], breaks = 10, main = "Surrogate Euclid 10th quantile")
abline(v = Cole09PopTest[[1]][4,1], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[4]][,2], breaks = 10, main = "Surrogate Phase 10th quantile")
abline(v = Cole09PopTest[[1]][4,2], col = "Red", lwd = 2)
hist(Cole09PopTest[[3]][[4]][,3], breaks = 10, main = "Surrogate Stepwise 10th quantile")
abline(v = Cole09PopTest[[1]][4,3], col = "Red", lwd = 2)

dev.off()
par(mfrow=c(1,1))


pdf("Intermediate stuff/Cole102SurrHists.pdf")

par(mfrow = c(3,3))
hist(Cole102PopTest[[3]][[1]][,1], breaks = 10, main = "Surrogate Euclid Mean")
abline(v = Cole102PopTest[[1]][1,1], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[1]][,2], breaks = 10, main = "Surrogate Phase Mean")
abline(v = Cole102PopTest[[1]][1,2], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[1]][,3], breaks = 10, main = "Surrogate Stepwise Mean")
abline(v = Cole102PopTest[[1]][1,3], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[3]][,1], breaks = 10, main = "Surrogate Euclid 90th quantile")
abline(v = Cole102PopTest[[1]][3,1], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[3]][,2], breaks = 10, main = "Surrogate Phase 90th quantile")
abline(v = Cole102PopTest[[1]][3,2], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[3]][,3], breaks = 10, main = "Surrogate Stepwise 90th quantile")
abline(v = Cole102PopTest[[1]][3,3], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[4]][,1], breaks = 10, main = "Surrogate Euclid 10th quantile")
abline(v = Cole102PopTest[[1]][4,1], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[4]][,2], breaks = 10, main = "Surrogate Phase 10th quantile")
abline(v = Cole102PopTest[[1]][4,2], col = "Red", lwd = 2)
hist(Cole102PopTest[[3]][[4]][,3], breaks = 10, main = "Surrogate Stepwise 10th quantile")
abline(v = Cole102PopTest[[1]][4,3], col = "Red", lwd = 2)

dev.off()
par(mfrow=c(1,1))

RankMat101<-matrix(data = NA, nrow = 3, ncol = 3)
RankMat102<-RankMat101
PosVec <- c(1,3,4)
for(i in 1:3){
  k <- PosVec[i]
  for(j in 1:3){
    Rank101 <- mean(Cole101PopTest[[3]][[k]][,j] < Cole101PopTest[[1]][k,j])
    Rank102 <- mean(Cole102PopTest[[3]][[k]][,j] < Cole102PopTest[[1]][k,j])

    RankMat101[i,j] <- Rank101
    RankMat102[i,j] <- Rank102
  }
}

hist(1/Cole101PopTest[[3]][[1]][,1], breaks = 20, main = NULL, xlab = NULL, ylab = NULL)
abline(v = 1/Cole101PopTest[[1]][1,1], col = "Red", lwd = 2)
text(x = 1/Cole101PopTest[[1]][1,1] + 5E-13, y = 15, labels = paste("p = ", RankMat101[1,1]), col = "red", cex = 4)

hist(1/Cole101PopTest[[3]][[1]][,3], breaks = 20, main = NULL, xlab = NULL, ylab = NULL)
abline(v = 1/Cole101PopTest[[1]][1,3], col = "Red", lwd = 2)
text(x = 1/Cole101PopTest[[1]][1,3] + 3E-12, y = 15, labels = paste("p = ", RankMat101[1,3]), col = "red", cex = 4)


hist(1/Cole102PopTest[[3]][[1]][,1], breaks = 20, main = NULL, xlab = NULL, ylab = NULL)
abline(v = 1/Cole102PopTest[[1]][1,1], col = "Red", lwd = 2)
text(x = 1/Cole102PopTest[[1]][1,1] + 1E-11, y = 15, labels = paste("p = ", RankMat102[1,1]), col = "red", cex = 4)

hist(1/Cole102PopTest[[3]][[1]][,3], breaks = 20, main = NULL, xlab = NULL, ylab = NULL)
abline(v =1/Cole102PopTest[[1]][1,3], col = "Red", lwd = 2)
text(x = 1/Cole102PopTest[[1]][1,3] + 5E-11, y = 15, labels = paste("p = ", RankMat102[1,3]), col = "red", cex = 4)


mean((1/Cole102PopTest[[1]][1,3]) < (1/Cole102PopTest[[3]][[1]][,3]))
mean(Cole102PopTest[[1]][1,3] > Cole102PopTest[[3]][[1]][,3])



LCSExtract(CNFHChinDense[CNFHChinDense$FishID %in% ColemanPairs[[1]][2206,1],"Loc"],
           CNFHChinDense[CNFHChinDense$FishID %in% ColemanPairs[[1]][2206,2],"Loc"],
                         as.numeric(CNFHChinDense[CNFHChinDense$FishID %in% ColemanPairs[[1]][2206,1],"FirstDet"]),
                         as.numeric(CNFHChinDense[CNFHChinDense$FishID %in% ColemanPairs[[1]][2206,1],"LastDet"]),
                         as.numeric(CNFHChinDense[CNFHChinDense$FishID %in% ColemanPairs[[1]][2206,2],"FirstDet"]),
                         as.numeric(CNFHChinDense[CNFHChinDense$FishID %in% ColemanPairs[[1]][2206,2],"LastDet"]),
                         c("All"))



DebugCole<-function(SetKey,PairKey){
  cat("std::vector<std::string> Fish1Seq_ {", paste(shQuote(CNFHChinDense[which(CNFHChinDense$FishID %in% ColemanPairs[[SetKey]][PairKey,1]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ1")
  cat("std::vector<std::string> Fish2Seq_ {", paste(shQuote(CNFHChinDense[which(CNFHChinDense$FishID %in% ColemanPairs[[SetKey]][PairKey,2]),"Loc"], 
                                                            type = "cmd"), collapse =", "), "};\n")
  #print("END FSEQ2")
  cat("std::vector<double> TSeqArr1 {", paste(as.numeric(CNFHChinDense[which(CNFHChinDense$FishID %in% ColemanPairs[[SetKey]][PairKey,1]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ1")
  cat("std::vector<double> TSeqDep1 {", paste(as.numeric(CNFHChinDense[which(CNFHChinDense$FishID %in% ColemanPairs[[SetKey]][PairKey,1]),"LastDet"]), 
                                              collapse =", "), "};\n")
  #print("END DEPSEQ1")
  cat("std::vector<double> TSeqArr2 {", paste(as.numeric(CNFHChinDense[which(CNFHChinDense$FishID %in% ColemanPairs[[SetKey]][PairKey,2]),"FirstDet"]), 
                                              collapse =", "), "};\n")
  #print("END ARRSEQ2")
  cat("std::vector<double> TSeqDep2 {", paste(as.numeric(CNFHChinDense[which(CNFHChinDense$FishID %in% ColemanPairs[[SetKey]][PairKey,2]),"LastDet"]), 
                                              collapse =", "), "};\n")
}

DebugCole(2,2118)

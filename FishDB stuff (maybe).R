# Fish DB stuff

FishMeta69 <- read.csv("Data/UCD69kHzFMeta.csv")

colnames(FishMeta69)


nSpecies<-table(FishMeta69[,3])

SpecStageVec<-vector(mode = "character",length = nrow(FishMeta69))
SpecRelVec<-vector(mode = "character",length = nrow(FishMeta69))
for(i in 1:nrow(FishMeta69)){
  SpecStageVec[i] = paste(FishMeta69[i,3],FishMeta69[i,16],sep = ": ")
  SpecRelVec[i] = paste(FishMeta69[i,3],FishMeta69[i,10],sep = ": ")
}
nSpecStage<-table(SpecStageVec)
nSpecRel<-table(SpecRelVec)

table(FishMeta69$Release_date_time)

HatchMeta<-read.csv("Data/HatchLocTimeNum.csv")

Hatch<-na.omit(HatchMeta[order(HatchMeta$Total_tagged_released, decreasing = T),])

SacElkSubFish<-FishMeta69[which(FishMeta69$Release_location %in% c("SacElkLanding", "BattleCkNFHWeir","SacJellyRamp")),]

DBTestSub<-vector(mode = "character", length = 251)
Key <- 1
for(i in 1:nrow(SacElkSubFish)){
  if(SacElkSubFish$Release_date_time[i] %in% "2009-03-06 19:10:00-08"){
    DBTestSub[Key] = SacElkSubFish$Fish_ID[i]
    Key <- Key + 1
  }
}

sqlPrinter<-function(NameVec){
  cat("(",paste(shQuote(NameVec, type = "sh"), collapse = ", "), ") \n")
}

sqlPrinter(DBTestSub)



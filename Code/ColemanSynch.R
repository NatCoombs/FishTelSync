# This script is for going through and evaluating synchrony for the Coleman releases
ColemanDets<-read.csv("Data/ColemanRel.csv")
Rels<-read.csv("Data/Releases.csv")
FalseDets<-read.csv("Data/Cull.csv")

# Also make a table pairing location and general location for this

SHKLocs<-unique(SHK$DetectionLocation)

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

GGBvec<-c("Golden Gate West Line","Golden Gate East Line")

GGBdex<-which(GenLocTable[,1] %in% GGBvec)

GenLocTable[GGBdex,1]<-"Golden Gate and Ocean"

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

# Let's adjust ColemanSub to match the locations as indicated in the GenLocTable

for(i in 1:length(ColemanSub[,"Location_name"])){
  Key <- GenLocTable[which(ColemanSub[i, "Location_name"] %in% GenLocTable[,2]),1]
  if(length(Key) != 0){
  ColemanSub[i, "Location_name"] <- Key
  }
}


CNFHChinDense<-DetCondense(ColemanSub, ColemanIDsVec, "Location_name", "Detect_date_time", "Fish_ID")


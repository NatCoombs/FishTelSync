length(which(SHK$DetectionLocation == "NULL"))
F2Test<-unique(SHK$FishID[which(SHK$DetectionLocation == "NULL")])
unique(SHK$DetectionLocation[which(SHK$FishID %in% F2Test[6])])
# This looks super wonky. Why are 6k detections unlocated for 23 fish???? That's 4% of our sample fish set.!

# Let's try a waterfall plot based on LongSteelDense
LongSteelDense<-DetCondense(DetFrame = SHK, FishID = LongSteelSet, LocCol = "DetectionLocation", tCol = "DetectDate",
                            IDCol = "FishID")

RkmTable<-unique(SHK[,c("DetectionLocation","RiverKm")])
LocTable<-unique(SHK[,c("DetectionLocation","Lat","Lon")])



for(i in 1:length(LongSteelDense$Loc)){
  LongSteelDense$Rkm[i]<-RkmTable$RiverKm[which(RkmTable$DetectionLocation == LongSteelDense$Loc[i])]
  LongSteelDense$Lat[i]<-LocTable$Lat[which(LocTable$DetectionLocation == LongSteelDense$Loc[i])]
  LongSteelDense$Lon[i]<-LocTable$Lon[which(LocTable$DetectionLocation == LongSteelDense$Loc[i])]
}



WaterSplit<-function(DenseTable, FishCol, RkmCol, T1Col, T2Col){
  WaterFrame<-data.frame()
  for(i in 1:length(DenseTable[,FishCol])){
    WaterFrame[2*i-1,1:3]<-DenseTable[i,c(FishCol,RkmCol,T1Col)]
    WaterFrame[2*i,1:3]<-DenseTable[i,c(FishCol,RkmCol,T2Col)]
  }
  colnames(WaterFrame)<-c("Fish","Rkm","Times")
  return(WaterFrame)
}
LongSteelFall<-WaterSplit(DenseTable = LongSteelDense, FishCol = "FishID", RkmCol = "Rkm", T1Col = "FirstDet", T2Col = "LastDet")

# Setting up dates and times for Chron

LSFDt<-t(as.data.frame(strsplit(LongSteelFall$Times, split = " ")))

LongSteelFall$ChronTimes<-chron(dates. = LSFDt[,1], times. = LSFDt[,2], format = c("y-m-d", "h:m:s"))
  
LongSteelFallPlot<-plot(x = NA, y = NA, xlab = "Date", ylab = "Rkm", main = "Waterfall plot for some steelhead runs",
                       ylim = c(0, max(LongSteelFall$Rkm)+5), xlim  = c(range(LongSteelFall$ChronTimes, na.rm = TRUE)))
for(i in 1:length(LongSteelSet)){
  lines(x = LongSteelFall[which(LongSteelFall$Fish == LongSteelSet[i]), "ChronTimes"],
        y = LongSteelFall[which(LongSteelFall$Fish == LongSteelSet[i]), "Rkm"])
  
}

for(i in 1:length(LongSteelSet)){
  plot(x = LongSteelFall[which(LongSteelFall$Fish == LongSteelSet[i]), "ChronTimes"],
                         y = LongSteelFall[which(LongSteelFall$Fish == LongSteelSet[i]), "Rkm"],
       type = "l", xlab = "Date", ylab = "Rkm", 
       main = paste("Waterfall plot for", LongSteelSet[i], sep = " "))
}


AllSteelDense<-DetCondense(DetFrame = SHK, FishID = unique(SHK$FishID), LocCol = "DetectionLocation", tCol = "DetectDate",
                           IDCol = "FishID")

fishIDs<-na.omit(unique(AllSteelDense$FishID))
# Need to remove NAs from locations and times


wfplot<-function(WaterFrame,FishCol,tCol,RkmCol,FishIDs){
  IDstr<-paste(FishIDs,collapse = ", ")
  
  plot(x = NA, y = NA, xlab = "Date", ylab = "Rkm", main = 
         paste(strwrap(paste("Waterfall plot for", IDstr, sep = " "), width = 50), collapse = "\n"), 
       ylim = c(0,max(WaterFrame[which(WaterFrame[,FishCol] %in% FishIDs),RkmCol])), 
       xlim = chron::as.chron(range(WaterFrame[which(WaterFrame[,FishCol] %in% FishIDs),tCol], na.rm = TRUE)))
  for(i in 1:length(FishIDs)){
    Watersub<-WaterFrame[which(WaterFrame[,FishCol] %in% FishIDs[i]),]
    lines(x = chron::as.chron(Watersub[,tCol]), y = Watersub[,RkmCol])
  }
}
wfplot(WaterFrame = LongSteelFall,FishCol = "Fish",tCol = "ChronTimes",
       RkmCol = "Rkm",FishIDs = na.omit(unique(LongSteelFall$Fish)))
LocTable<-unique(SHK[,c("DetectionLocation","Lat","Lon")])
for(i in 1:length(AllSteelDense$Loc)){
  AllSteelDense$Rkm[i]<-RkmTable$RiverKm[which(RkmTable$DetectionLocation == AllSteelDense$Loc[i])]
  AllSteelDense$Lat[i]<-LocTable$Lat[which(LocTable$DetectionLocation == AllSteelDense$Loc[i])]
  AllSteelDense$Lon[i]<-LocTable$Lon[which(LocTable$DetectionLocation == AllSteelDense$Loc[i])]
}
AllSteelFall<-WaterSplit(AllSteelDense, "FishID","Rkm","FirstDet","LastDet")


compwfplot<-function(WaterFrame,FishCol,tCol,RkmCol,FishIDs,xLims,yLims){
  IDstr<-paste(FishIDs,collapse = ", ")
  
  plot(x = NA, y = NA, xlab = "Date", ylab = "Rkm", main = 
         paste(strwrap(paste("Waterfall plot for", IDstr, sep = " "), width = 50), collapse = "\n"), 
       ylim = xLims, 
       xlim = yLims)
  for(i in 1:length(FishIDs)){
    Watersub<-WaterFrame[which(WaterFrame[,FishCol] %in% FishIDs[i]),]
    lines(x = Watersub[,tCol], y = Watersub[,RkmCol])
  }
}

plot(CAHydroLL)
FishDetMap(LongSteelDense,IDCol = "FishID", LatCol = "Lat", LonCol = "Lon", WaterMaps = CAHydroLL, 
           FishID = LongSteelSet, LandMaps = CALandLL)

FishDetMap(LongSteelDense,IDCol = "FishID", LatCol = "Lat", LonCol = "Lon", WaterMaps = CAHydroLL, 
           FishID = "STH0411", LandMaps = CALandLL, textlab = TRUE, MonoCol = "Red", RkmCol = "Rkm")
#Let's worry about coloring later and fix the basemap stuff for now. 
#Maybe incorporate a way to annotate detection points?

map(database = "states", regions = "california",

     fill = TRUE,    col = "Dark Green", plot = TRUE,xlim = c(-124,-119), ylim = c(36,39))

Steven<-map(database = "state", regions = "california", fill = TRUE, col = "Dark Green", plot = TRUE,xlim = c(-124,-119), ylim = c(36,39))




USFWS0508<-read.csv("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Data/Kelt_releases_for_Nat_20210916.csv",
         stringsAsFactors = F,na.strings=c("NULL",""),)
FWS200Rel<-USFWS0508[which(USFWS0508$Fish_ID%in%USFWS200Rkm$FishID),]


RelAdd<-data.frame()
for(i in 1:length(FWS200Rel$Fish_ID)){

RelAdd[i,1]<-FWS200Rel$Fish_ID[i]
RelAdd[i,2]<-FWS200Rel$Release_location[i]
RelAdd[i,3]<-FWS200Rel$Release_date_time[i]
RelAdd[i,4]<-FWS200Rel$Release_date_time[i]
RelAdd[i,5]<-1
RelAdd[i,6]<-FWS200Rel$River_km[i]
RelAdd[i,7]<-FWS200Rel$Latitude[i]
RelAdd[i,8]<-FWS200Rel$Longitude[i]
}
colnames(RelAdd)<-colnames(SHK200FWSDense)
RelAdd$FirstDet <- substr(RelAdd$FirstDet,1,19)
RelAdd$LastDet <- substr(RelAdd$LastDet,1,19)


SHK200FWS<-SHK[which(SHK$FishID%in%USFWS200Rkm$FishID),]
SHK200FWSDense<-DetCondense(SHK200FWS,FishID = unique(SHK200FWS$FishID),
                            LocCol = "DetectionLocation", tCol = "DetectDate",
                            IDCol = "FishID")

for(i in 1:length(SHK200FWSDense$Loc)){
  SHK200FWSDense$Rkm[i]<-RkmTable$RiverKm[which(RkmTable$DetectionLocation %in% SHK200FWSDense$Loc[i])]
  SHK200FWSDense$Lat[i]<-LocTable$Lat[which(LocTable$DetectionLocation %in% SHK200FWSDense$Loc[i])]
  SHK200FWSDense$Lon[i]<-LocTable$Lon[which(LocTable$DetectionLocation %in% SHK200FWSDense$Loc[i])]
}

SHK200FWSDense<-rbind(SHK200FWSDense,RelAdd)

FWSSteelFall<-WaterSplit(SHK200FWSDense, "FishID","Rkm","FirstDet","LastDet")

wfplot(WaterFrame = FWSSteelFall, FishCol = "Fish", tCol = "Times", RkmCol = "Rkm",
       FishIDs = na.omit(unique(FWSSteelFall$Fish)))

wfplot(WaterFrame = FWSSteelFall, FishCol = "Fish", tCol = "Times", RkmCol = "Rkm",
       FishIDs = na.omit(unique(USFWS200Rkm05$FishID)))

wfplot(WaterFrame = FWSSteelFall, FishCol = "Fish", tCol = "Times", RkmCol = "Rkm",
       FishIDs = na.omit(unique(USFWS200Rkm06$FishID)))

wfplot(WaterFrame = FWSSteelFall, FishCol = "Fish", tCol = "Times", RkmCol = "Rkm",
       FishIDs = na.omit(unique(USFWS200Rkm07$FishID)))

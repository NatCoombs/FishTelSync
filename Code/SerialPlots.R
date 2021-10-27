library(chron)
library(rgdal)


FishRef<-unique(SHK[,c(2,4,10,12)])

USFWSsub<-FishRef[which(substr(FishRef$StudyID,1,5) == "USFWS"),]

USFWS200Rkm<-USFWSsub[which(USFWSsub$FishID %in% Fish200),]

USFWSReldex<-unique(USFWS200Rkm$Date_Released)

USFWS200Rkm05<-USFWS200Rkm[which(USFWS200Rkm$Date_Released %in% USFWSReldex[1]),]
USFWS200Rkm06<-USFWS200Rkm[which(USFWS200Rkm$Date_Released %in% USFWSReldex[2]),]
USFWS200Rkm07<-USFWS200Rkm[which(USFWS200Rkm$Date_Released %in% USFWSReldex[3]),]

Pairs05<-data.frame()
Pairs06<-data.frame()
Pairs07<-data.frame()

k <- 1
for(i in 1:length(USFWS200Rkm05$FishID)){
  for(j in (i+1):length(USFWS200Rkm05$FishID)){
    Pairs05[k,1]<-USFWS200Rkm05$FishID[i]
    Pairs05[k,2]<-USFWS200Rkm05$FishID[j]
    k <- k+1
  }
}
Pairs05<-na.omit(Pairs05)

k <- 1
for(i in 1:length(USFWS200Rkm06$FishID)){
  for(j in (i+1):length(USFWS200Rkm06$FishID)){
    Pairs06[k,1]<-USFWS200Rkm06$FishID[i]
    Pairs06[k,2]<-USFWS200Rkm06$FishID[j]
    k <- k+1
  }
}
Pairs06<-na.omit(Pairs06)

k <- 1
for(i in 1:length(USFWS200Rkm07$FishID)){
  for(j in (i+1):length(USFWS200Rkm07$FishID)){
    Pairs07[k,1]<-USFWS200Rkm07$FishID[i]
    Pairs07[k,2]<-USFWS200Rkm07$FishID[j]
    k <- k+1
  }
}
Pairs07<-na.omit(Pairs07)

# 265 pairs across the three USFWS study that exceed 200 rkm for each fish

USACEsub<-FishRef[which(substr(FishRef$StudyID,1,5) == "USACE"),]

USACE200Rkm<-USACEsub[which(USACEsub$FishID %in% Fish200),]

# Selecting for fish based on length of time in river.
TwoWeekFish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
  TwoWeekFish[i,1]<- fishIDs[i]
  TwoWeekFish[i,2]<- (range(fset)[2] - range(fset)[1])<= 14
}
TwoWeekSet<-TwoWeekFish[which(TwoWeekFish[,2]),1]

OneMonthFish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
  OneMonthFish[i,1]<- fishIDs[i]
  OneMonthFish[i,2]<- (range(fset)[2] - range(fset)[1])<= 31
}
OneMonthSet<-OneMonthFish[which(OneMonthFish[,2]),1]

TwoMonthFish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
  TwoMonthFish[i,1]<- fishIDs[i]
  TwoMonthFish[i,2]<- (range(fset)[2] - range(fset)[1])<= 62
}
TwoMonthSet<-TwoMonthFish[which(TwoMonthFish[,2]),1]

ThreeMonthFish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
  ThreeMonthFish[i,1]<- fishIDs[i]
  ThreeMonthFish[i,2]<- (range(fset)[2] - range(fset)[1])<= 93
}
ThreeMonthSet<-ThreeMonthFish[which(ThreeMonthFish[,2]),1]



SixMonthFish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
  SixMonthFish[i,1]<- fishIDs[i]
  SixMonthFish[i,2]<- (range(fset)[2] - range(fset)[1])<= 186
}
SixMonthSet<-SixMonthFish[which(SixMonthFish[,2]),1]

TwelveMonthFish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
  TwelveMonthFish[i,1]<- fishIDs[i]
  TwelveMonthFish[i,2]<- (range(fset)[2] - range(fset)[1])<= 365
}
TwelveMonthSet<-TwelveMonthFish[which(TwelveMonthFish[,2]),1]

# Set them up for some variety in rkm levels
Rkm100Fish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),2]
  Rkm100Fish[i,1]<- fishIDs[i]
  Rkm100Fish[i,2]<- (range(fset)[2] - range(fset)[1])>= 100
}
Rkm100Set<-TwelveMonthFish[which(TwelveMonthFish[,2]),1]

Rkm200Fish<-data.frame()
for(i in 1:length(fishIDs)){
  fset<-AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),2]
  Rkm200Fish[i,1]<- fishIDs[i]
  Rkm200Fish[i,2]<- (range(fset)[2] - range(fset)[1])>= 200
}
Rkm200Set<-TwelveMonthFish[which(TwelveMonthFish[,2]),1]

# Create an indexed set of fish pairs
OverlappingPairs<-data.frame()
k<-1
for(i in 1:length(fishIDs)){
  for(j in (i+1):length(fishIDs)){
    OverlappingPairs[k,1]<-fishIDs[i]
    OverlappingPairs[k,2]<-fishIDs[j]
    f1set<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[i]),4])
    f2set<-as.chron(AllSteelFall[which(AllSteelFall$Fish %in% fishIDs[j]),4])
    
    MxCont1<-max(f1set, na.rm = TRUE) <= max(f2set, na.rm = TRUE) & 
      max(f1set, na.rm = TRUE) >= min(f2set, na.rm = TRUE)
    MxCont2<-max(f2set, na.rm = TRUE) <= max(f1set, na.rm = TRUE) & 
      max(f2set, na.rm = TRUE) >= min(f1set, na.rm = TRUE)
    MnCont1<-min(f1set, na.rm = TRUE) <= max(f2set, na.rm = TRUE) & 
      min(f1set, na.rm = TRUE) >= min(f2set, na.rm = TRUE)
    MnCont2<-max(f2set, na.rm = TRUE) <= max(f1set, na.rm = TRUE) & 
      min(f2set, na.rm = TRUE) >= min(f1set, na.rm = TRUE)
    # MxCont1<-max(AllSteelDense[f1dex,"LastDet"], na.rm = TRUE) <= max(AllSteelDense[f2dex,"LastDet"], na.rm = TRUE) &&
    #   max(AllSteelDense[f1dex,"LastDet"], na.rm = TRUE) >= min(AllSteelDense[f2dex,"FirstDet"], na.rm = TRUE)
    # MxCont2<-max(AllSteelDense[f2dex,"LastDet"], na.rm = TRUE) <= max(AllSteelDense[f1dex,"LastDet"], na.rm = TRUE) &&
    #   max(AllSteelDense[f2dex,"LastDet"], na.rm = TRUE) >= min(AllSteelDense[f1dex,"FirstDet"], na.rm = TRUE)
    # MnCont1<-min(AllSteelDense[f1dex,"FirstDet"], na.rm = TRUE) <= max(AllSteelDense[f2dex,"LastDet"], na.rm = TRUE) &&
    #   min(AllSteelDense[f1dex,"FirstDet"], na.rm = TRUE) >= min(AllSteelDense[f2dex,"FirstDet"], na.rm = TRUE)
    # MnCont2<-min(AllSteelDense[f2dex,"FirstDet"], na.rm = TRUE) <= max(AllSteelDense[f1dex,"LastDet"], na.rm = TRUE) &&
    #   min(AllSteelDense[f2dex,"FirstDet"], na.rm = TRUE) >= min(AllSteelDense[f1dex,"FirstDet"], na.rm = TRUE)
    if(MxCont1||MxCont2||MnCont1||MnCont2){OverlappingPairs[k,3] <- TRUE}
    else {OverlappingPairs[k,3] <- FALSE}
    k <- k+1
  }
}



OverPlotPairs<-OverlappingPairs[which(OverlappingPairs[,3]),c(1,2)]

TwoWeekPairs<-OverPlotPairs[which(OverPlotPairs[,1] %in% TwoWeekSet 
                                   & OverPlotPairs[,2] %in% TwoWeekSet),]

OneMonthPairs<-OverPlotPairs[which(OverPlotPairs[,1] %in% OneMonthSet 
                                   & OverPlotPairs[,2] %in% OneMonthSet),]

TwoMonthPairs<-OverPlotPairs[which(OverPlotPairs[,1] %in% TwoMonthSet 
                                   & OverPlotPairs[,2] %in% TwoMonthSet),]

ThreeMonthPairs<-OverPlotPairs[which(OverPlotPairs[,1] %in% ThreeMonthSet 
                                     & OverPlotPairs[,2] %in% ThreeMonthSet),]

SixMonthPairs<-OverPlotPairs[which(OverPlotPairs[,1] %in% SixMonthSet 
                                     & OverPlotPairs[,2] %in% SixMonthSet),]

TwelveMonthPairs<-OverPlotPairs[which(OverPlotPairs[,1] %in% TwelveMonthSet 
                                   & OverPlotPairs[,2] %in% TwelveMonthSet),]


ThreeToSixMonthPairs<-SixMonthPairs[which(!(SixMonthPairs[,c(1,2)] %in% ThreeMonthPairs[,c(1,2)]))]

# Check to see which of the Rkm200 fish are in the overlapping pairs
Fish200<-Rkm200Fish[which(Rkm200Fish[,2]),1]
OverPlotdex1<-which(OverPlotPairs[,1] %in% Fish200)
OverPlotdex2<-which(OverPlotPairs[,2] %in% Fish200)
OverPlotdex200<-OverPlotdex1[which(OverPlotdex1 %in% OverPlotdex2)]
Fish200Pairs<-OverPlotPairs[OverPlotdex200,]
# We'll randomly select from these until we get 100 showing a reasonable range of results/similarities.
PlotDex<-runif(n = 50, min = 1, max = 1701)
PlotSet<-Fish200Pairs[PlotDex,]

# OverlapStatement: if(max of one is less than max of the other and more than min of the other OR)
plot.new()
par(mfrow = c(2,2))
wfplot(WaterFrame = LongSteelFall,FishCol = "Fish",tCol = "ChronTimes",
       RkmCol = "Rkm",FishIDs = "STH0411")
wfplot(WaterFrame = LongSteelFall,FishCol = "Fish",tCol = "ChronTimes",
       RkmCol = "Rkm",FishIDs = "STH0465")
FishDetMap(LongSteelDense,IDCol = "FishID", LatCol = "Lat", LonCol = "Lon", WaterMaps = CAHydroLL, 
           FishID = "STH0411", LandMaps = CALandLL, textlab = TRUE, MonoCol = "Red", RkmCol = "Rkm",
           nonNew = TRUE)
FishDetMap(LongSteelDense,IDCol = "FishID", LatCol = "Lat", LonCol = "Lon", WaterMaps = CAHydroLL, 
           FishID = "STH0465", LandMaps = CALandLL, textlab = TRUE, MonoCol = "Red", RkmCol = "Rkm",
           nonNew = TRUE)


RangeCompareXY<-function(f1,f2,RelFrame,xCol,yCol,fCol,DecLim = .1){
  f1set<-which(RelFrame[,fCol] %in% f1)
  f2set<-which(RelFrame[,fCol] %in% f2)
  
  x1Range<-range(RelFrame[f1set,xCol])
  x2Range<-range(RelFrame[f2set,xCol])
  xLower<-if(x1Range[1]<=x2Range[1]){x1Range[1]} else {x2Range[1]}
  xUpper<-if(x1Range[2]>=x2Range[2]){x1Range[2]} else {x2Range[2]}
  xRange<-c(xLower,xUpper)
  
  
  
  xOffset<-(xRange[2]-xRange[1])*DecLim
  xLimsinit<-xRange+c(-xOffset,xOffset)
  xCenter<-mean(xLimsinit)
  xReach<-xLimsinit[2]-xCenter
  
  
  y1Range<-range(RelFrame[f1set,yCol])
  y2Range<-range(RelFrame[f2set,yCol])
  yLower<-if(y1Range[1]<=y2Range[1]){y1Range[1]} else {y2Range[1]}
  yUpper<-if(y1Range[2]>=y2Range[2]){y1Range[2]} else {y2Range[2]}
  yRange<-c(yLower,yUpper)
  
  
  yOffset<-(yRange[2]-yRange[1])*DecLim
  yLimsinit<-yRange+c(-yOffset,yOffset)
  yCenter<-mean(yLimsinit)
  yReach<-yLimsinit[2]-yCenter
  
  
  if(yReach > xReach){
    xLims <- c(xCenter-yReach,xCenter+yReach)
    yLims <- c(yCenter-yReach,yCenter+yReach)
  }
  if(xReach > yReach){
    xLims <- c(xCenter-xReach,xCenter+xReach)
    yLims <- c(yCenter-xReach,yCenter+xReach)
  }
  return(c(xLims,yLims))
}

RangeCompareT<-function(f1,f2,RelFrame,tCol,fCol){
  f1set<-which(RelFrame[,fCol] %in% f1)
  f2set<-which(RelFrame[,fCol] %in% f2)
  tMin1<-min(f1set[,tCol], na.rm = TRUE)
  tMin2<-min(f2set[,tCol], na.rm = TRUE)
  tMax1<-max(f1set[,tCol], na.rm = TRUE)
  tMax2<-max(f2set[,tCol], na.rm = TRUE)
    
  tMin <- if(tMin1 <= tMin2){tMin1} else {tMin2}
  tMax <- if(tMax1 >= tMax2){tMax1} else {tMax2}
  return(c(tMin,tMax))
}


# Printing setup:
AllSteelFall$ChronTimes<-as.chron(AllSteelFall$Times)
pdf(file = "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Results/SampledPairwiseFishMovements.pdf")
par(mfrow = c(2,2))
for(i in 1:length(PlotSet[,1])){
  f1<-PlotSet[i,1]
  f2<-PlotSet[i,2]
  # MapLims<-RangeCompareXY(f1 = f1, f2 = f2, RelFrame = AllSteelDense, 
  #                         xCol = "Lon", yCol = "Lat", fCol = "FishID")
  # tLims<-RangeCompareT(f1 = f1, f2 = f2, RelFrame = AllSteelDense, 
  # tCol = "Times", fCol = "FishID")
  # plot.new()
  wfplot(WaterFrame = AllSteelFall,FishCol = "Fish",tCol = "ChronTimes",
         RkmCol = "Rkm",FishIDs = c(f1,f2))
  wfplot(WaterFrame = AllSteelFall,FishCol = "Fish",tCol = "ChronTimes",
         RkmCol = "Rkm",FishIDs = f2)
  FishDetMap(AllSteelDense,IDCol = "FishID", LatCol = "Lat", LonCol = "Lon", WaterMaps = CAHydroLL, 
             FishID = f1, LandMaps = CALandLL, textlab = TRUE, MonoCol = "Red", RkmCol = "Rkm",
             nonNew = TRUE)
  FishDetMap(AllSteelDense,IDCol = "FishID", LatCol = "Lat", LonCol = "Lon", WaterMaps = CAHydroLL, 
             FishID = f2, LandMaps = CALandLL, textlab = TRUE, MonoCol = "Red", RkmCol = "Rkm",
             nonNew = TRUE)
}

dev.off()








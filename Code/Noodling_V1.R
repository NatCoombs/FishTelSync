# Just a file for preliminary investigation of the data

#Set of all receiver/location sets in the data
library(maps)
library(chron)
library(viridisLite)
# Stations<-unique(SRC[,3:6])
# 
# RecConcernInd<-which(is.na(Stations$General_latitude))
# 
# 
# RecConcern<-as.vector(Stations$Receiver_ser_num[RecConcernInd])
# write.csv(RecConcern, "/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Intermediate stuff/Receivers missing locations.csv", 
#           row.names = FALSE)
# 
# Fish<-unique(SRC[,c(2,10)])
# 
# StationLocations<-Stations[which(is.na(Stations$General_latitude)==FALSE),4:3]
ArrayLocations<-unique(CleanSRC1[5:4])
ReleaseLocations<-unique(CleanSRC1[30:29])
HydroMap<-map(database = "state", regions = "california", fill = TRUE, col = "Dark Green", plot = TRUE,xlim = c(-124,-119), ylim = c(36,39))
HydroMap<-plot(CAHydroLL, col = "Dark Blue", lwd = 1.5, xlim = c(-123,-118), ylim = c(37,38), add = TRUE)
HydroCali<-recordPlot()

WriteOGR(HydroMap)

points(ArrayLocations, pch = 16, col = "red", cex = 1)
points(ReleaseLocations, pch = 13, col = "pink", cex = 2)
title(main = "Array & release locations for Spring Run 2017")
#Table of detections by tag ID & then receiver
NumDetsF<-as.data.frame(table(CleanSRC1$TagID))
NumDetsR<-as.data.frame(table(CleanSRC1$GPS.Names))
NumDetsLoc<-as.data.frame(table(CleanSRC1$General.Location))

NumDetsFxL<-as.data.frame(table(CleanSRC1$TagID,CleanSRC1$General.Location))
# Calculate number of locations a fish is detected in
TabFishLoc<-as.data.frame(table(NumDetsFxL$Var1,NumDetsFxL$Freq>0))
LocationsPerFish<-TabFishLoc[which(as.logical(TabFishLoc$Var2)),c(1,3)]
FishToPlot<-as.character(LocationsPerFish[which(LocationsPerFish[,2]>=5),1])

SRCWaterPlotSub<-CleanSRC1[which(CleanSRC1$TagID %in% FishToPlot & CleanSRC1$SigStr >= CleanSRC1$Thresh),
                           c(1,2,3,4,5,6,8,24,25,27,28,29,30)]
# Add "synthetic rows" to plot releases
FishReleases<-unique(SRCWaterPlotSub[c(1,8,9,10,11,12,13)])
AddableReleases<-matrix(data = NA, ncol = ncol(SRCWaterPlotSub),nrow = 335)
colnames(AddableReleases)<-colnames(SRCWaterPlotSub)
AddableReleases[,1]<-FishReleases$TagID
AddableReleases[,c(2,9)]<-FishReleases$ReleaseDate
AddableReleases[,c(3,7,10)]<-FishReleases$ReleaseLocationName
AddableReleases[,c(4,12)]<-FishReleases$ReleaseLatitude
AddableReleases[,c(5,13)]<-FishReleases$ReleaseLongitude
AddableReleases[,c(6,11)]<-FishReleases$ReleaseRKm
AddableReleases[,c(8)]<-FishReleases$ReleaseGroup

SRCWaterRelInc<-rbind(SRCWaterPlotSub,AddableReleases)

# Converting dtf to a more usable form
DTimeComps<-t(as.data.frame(strsplit(SRCWaterRelInc$dtf, " ")))
row.names(DTimeComps)<-NULL
# If I can get seconds later on, I will.  For the package, I'll want to check fields
DTimeComps[,2]<-paste(DTimeComps[,2], ":00", sep = "", collapse = NULL)
SRCWaterRelInc$DetectionTime<-chron(dates. = DTimeComps[,1] , times. = DTimeComps[,2],
                                   format = c(dates = "m/d/y",times = "h:m:s"), origin. = c(
                                     month = 1, day = 1, year = 2017
                                   ))
#Plotting all detections
StartEndDates<-chron(dates. = c("3/5/17","5/31/17"), times. = c("1:1:00","1:1:00"),
                     format = c(dates = "m/d/y",times = "h:m:s"), origin. = c(
                       month = 1, day = 1, year = 2017))
# plot(x = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[1]),"DetectionTime"],
#      y = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[1]),"rkm"],
#      type = "l", xlab = "Detection Time", ylab = "Rkm", ylim = c(0,310), xlim = c(StartEndDates[1:2]))
ReleaseGroupIndex<-unique(SRCWaterRelInc$ReleaseGroup)

library(viridisLite)
RunColors<-viridis(length(ReleaseGroupIndex), begin =.2, option = "H")
LaZPalette<-c("Red","Blue","Green","Yellow")
AllRunPlot<-plot(x = NA,
     y = NA,
     type = "l", xlab = "Detection Time", ylab = "Rkm", ylim = c(0,340), xlim = c(StartEndDates[1:2]))
#Incorporate variation to show each subgroup
for(i in 1:length(FishToPlot)){
  for(j in 1:length(ReleaseGroupIndex)){
    if(ReleaseGroupIndex[j]==SRCWaterRelInc$ReleaseGroup
       [which(SRCWaterRelInc$TagID == FishToPlot[i])]){
      lines(x = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i] ),"DetectionTime"],
        y = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i] ),"rkm"], 
        col = RunColors[j])}
    
  }
}     
title(main = "Preliminary waterfall plot showing all fish")
legend(x = "bottomleft", legend = ReleaseGroupIndex[1:length(ReleaseGroupIndex)], 
       col = RunColors[1:length(ReleaseGroupIndex)], lty = 1, bty = "n")



UpperRunPlot<-plot(x = NA,
                     y = NA,
                     type = "l", xlab = "Detection Time", ylab = "Rkm", ylim = c(0,340), xlim = c(StartEndDates[1:2]))
#Incorporate variation to show each subgroup
for(i in 1:length(FishToPlot)){
  for(j in 1:2){
    if(ReleaseGroupIndex[j]==SRCWaterRelInc$ReleaseGroup
       [which(SRCWaterRelInc$TagID == FishToPlot[i])]){
      lines(x = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i] ),"DetectionTime"],
            y = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i] ),"rkm"], 
            col = RunColors[j])}
    
  }
}     
title(main = "Preliminary waterfall plot showing Fresno releases")
legend(x = "topright", legend = ReleaseGroupIndex[1:2], 
       col = RunColors[1:2], lty = 1)

StartEndDates2<-chron(dates. = c("3/20/17","5/31/17"), times. = c("1:1:00","1:1:00"),
                     format = c(dates = "m/d/y",times = "h:m:s"), origin. = c(
                       month = 1, day = 1, year = 2017))
LowerRunPlot<-plot(x = NA,
                   y = NA,
                   type = "l", xlab = "Detection Time", ylab = "Rkm", ylim = c(0,200), xlim = c(StartEndDates2[1:2]))
#Incorporate variation to show each subgroup
for(i in 1:length(FishToPlot)){
  for(j in 3:4){
    if(ReleaseGroupIndex[j]==SRCWaterRelInc$ReleaseGroup
       [which(SRCWaterRelInc$TagID == FishToPlot[i])]){
      lines(x = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i] ),"DetectionTime"],
            y = SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i] ),"rkm"], 
            col = RunColors[i])}
    
  }
}     
title(main = "Preliminary waterfall plot showing Durham releases")
legend(x = "topright", legend = ReleaseGroupIndex[3:4], 
       col = RunColors[3:4], lty = 1)


#Add columns for downstream velocities to see if we can make sense of that
#1 time step
for(i in 1:length(FishToPlot)){
  DetTimeSet<-as.numeric(SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i]),"DetectionTime"])
  DetRkmSet<-as.numeric(SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i]),"rkm"])
  for(j in 1:(length(DetTimeSet)-1)){
    SRCWaterRelInc$VelStep1[which(SRCWaterRelInc$TagID == FishToPlot[i] && 
                                    as.numeric(SRCWaterRelInc$DetectionTime) == DetTimeSet[j])]<- 
      ((DetRkmSet[i+1]-DetRkmSet[i])/(DetTimeSet[i+1]-DetTimeSet[i]))
  }
}




#2 time step
SRCWaterRelInc$VelStep2<-NA

for(i in 1:length(FishToPlot)){
  DetTimeSet<-as.numeric(SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i]),"DetectionTime"])
  DetRkmSet<-as.numeric(SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i]),"rkm"])
  for(j in 1:(length(DetTimeSet)-2)){
    SRCWaterRelInc$VelStep2[which(SRCWaterRelInc$TagID == FishToPlot[i] && 
                                    as.numeric(SRCWaterRelInc$DetectionTime) == DetTimeSet[j])] <- 
      ((DetRkmSet[i+2]-DetRkmSet[i])/(DetTimeSet[i+2]-DetTimeSet[i]))
  }
}

#3 time step
SRCWaterRelInc$VelStep3<-NA

for(i in 1:length(FishToPlot)){
  DetTimeSet<-as.numeric(SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i]),"DetectionTime"])
  DetRkmSet<-as.numeric(SRCWaterRelInc[which(SRCWaterRelInc$TagID == FishToPlot[i]),"rkm"])
  for(j in 1:(length(DetTimeSet)-3)){
    SRCWaterRelInc$VelStep3[which(SRCWaterRelInc$TagID == FishToPlot[i] && 
                                    as.numeric(SRCWaterRelInc$DetectionTime) == DetTimeSet[j])]<- 
      ((DetRkmSet[i+3]-DetRkmSet[i])/(DetTimeSet[i+3]-DetTimeSet[i]))
  }
}

# You know, this is a chonky dataset.  
# I just wrote a short script that iterates and uses differences. 
# Why don't I just.... do more with that idea?

SRCBreaksInt<-unique(SRCWaterRelInc[,c("TagID","Lat","Lon","rkm","General.Location","DetectionTime")])
# Note: changed code for SRCBreaksInt. Need to runn SRCBReaks again even IF I have debugged it which...
# Unlikely.
SRCBreaksInt$KeepIndex<-NA
SRCBreaksInt<-SRCBreaksInt[order(SRCBreaksInt$TagID,
                                 SRCBreaksInt$DetectionTime),]


for(i in 1:length(FishToPlot)){
  DetTimeSet<-as.numeric(SRCBreaksInt[which(SRCBreaksInt$TagID == FishToPlot[i]),"DetectionTime"])
  DetRkmSet<-as.numeric(SRCBreaksInt[which(SRCBreaksInt$TagID == FishToPlot[i]),"rkm"])
  for(j in 1:length(DetTimeSet)){
    if(j == 1 || j == 2){
      SRCBreaksInt$KeepIndex[which(SRCBreaksInt$TagID == FishToPlot[i]) %in% 
              which(as.numeric(SRCBreaksInt$DetectionTime) == DetTimeSet[j])] <- 1
    }
    else {if(j == length(DetTimeSet) || j == length(DetTimeSet) - 1){
      SRCBreaksInt$KeepIndex[which(SRCBreaksInt$TagID == FishToPlot[i]) %in% 
              which(as.numeric(SRCBreaksInt$DetectionTime) == DetTimeSet[j])] <- 1
    }
    else { if(
      ((DetRkmSet[j] != DetRkmSet[j-2]) | (DetRkmSet[j] != DetRkmSet[j+2]))){
      SRCBreaksInt$KeepIndex[which(SRCBreaksInt$TagID == FishToPlot[i]) %in% 
            which(as.numeric(SRCBreaksInt$DetectionTime) == DetTimeSet[j])] <- 1
    }
    else{
      SRCBreaksInt$KeepIndex[which(SRCBreaksInt$TagID == FishToPlot[i]) %in% 
                  which(as.numeric(SRCBreaksInt$DetectionTime) == DetTimeSet[j])] <- 0
    }
    }
    }
   }
}

SRCBreaks<-subset(SRCBreaksInt, KeepIndex == 1)

SubDebSetInd<-which(!(unique(SRCBreaksInt$TagID) %in% unique(SRCBreaks$TagID)))
SubDebSet<-unique(SRCBreaksInt$TagID)[SubDebSetInd]
print(SubDebSet)

FofConcern<-subset(SRCBreaksInt, TagID %in% SubDebSet)

plot(x= FofConcern$DetectionTime, y = FofConcern$rkm)


plot(x= SRCBreaks$DetectionTime, y = SRCBreaks$rkm)

for(i in 1:length(FishToPlot)){
  DetTimeSet<-as.numeric(FofConcern[which(FofConcern$TagID == FishToPlot[i]),"DetectionTime"])
  DetRkmSet<-as.numeric(FofConcern[which(FofConcern$TagID == FishToPlot[i]),"rkm"])
  for(j in 1:length(DetTimeSet)){
    if(j == 1){
      FofConcern$KeepIndex[which(FofConcern$TagID == FishToPlot[i]) %in% 
                               which(as.numeric(FofConcern$DetectionTime) == DetTimeSet[j])] <- 1
    }
    else {if(j == length(DetTimeSet)){
      FofConcern$KeepIndex[which(FofConcern$TagID == FishToPlot[i]) %in% 
                               which(as.numeric(FofConcern$DetectionTime) == DetTimeSet[j])] <- 1
    }
      else { if(
        ((DetRkmSet[j] != DetRkmSet[j-1]) || (DetRkmSet[j] != DetRkmSet[j+1]))){
        FofConcern$KeepIndex[which(FofConcern$TagID == FishToPlot[i]) %in% 
                                 which(as.numeric(FofConcern$DetectionTime) == DetTimeSet[j])] <- 1
      }
        else{
          FofConcern$KeepIndex[which(FofConcern$TagID == FishToPlot[i]) %in% 
                                   which(as.numeric(FofConcern$DetectionTime) == DetTimeSet[j])] <- 0
        }
      }
    }
  }
}







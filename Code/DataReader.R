# Read in table(s)
SRC<-read.csv("Data/Chinook_Juvenile_SpringRun_SanJoaquin_20210802.csv",stringsAsFactors = F)

CleanSRC1<-read.csv("Data/SalmonData/sj_reboot2.csv",stringsAsFactors = F)

SHK<-read.csv("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Data/sac_river_steelhead.csv",
              stringsAsFactors = F,na.strings=c("NULL",""),nrows=1782042)
#Read in shapefiles
CAHydro<-readOGR("Data/CA_Hydro")
CAHydroLL = spTransform(CAHydro, "+init=epsg:4326")

TestingSubset<-SRCWaterRelInc[which(SRCWaterRelInc$TagID %in% FishToPlot[1:10]),c(1,2,4,5,6,7,8)]

write.csv(TestingSubset, file = "Code/Functions/TestingData/SRCTestSub1.csv")

CAland<-rgdal::readOGR("/Users/nathanielcoombs/Downloads/ca-state-boundary")

CALandLL = spTransform(CAland, "+init=epsg:4326")
plot(CALandLL, col = "Dark Green", xlim = c(-123,-121), ylim = c(37,41))
plot(CAHydroLL, col = "Blue", add = T, lwd = 2)
points(x = Locs$Lon, y = Locs$Lat, col = "Yellow", pch = 16, cex = 2)
title(main = "Primary detector sites in PATH")


# Read in all locations

JSATSLocs<-read.csv("Data/JSATSLL.csv")
kHzLocs<-read.csv("Data/69kHzLL.csv")
colnames(kHzLocs)<-c("Lat","Lon")
Locs<-rbind(unique(JSATSLocs), unique(kHzLocs))



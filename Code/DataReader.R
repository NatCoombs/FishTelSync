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
plot(CALandLL, col = "Dark Green")

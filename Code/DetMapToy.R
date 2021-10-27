# Testing doc for making a quick and dirty "Plot that fish function"
library(plotfunctions)

MapThatFish<-function(FishID, BaseMap, MapType, DataSource, LatCol, LongCol, IDCol, TimeCol, xlims, ylims){
  FishPoints<-DataSource[which(FishID==DataSource[,IDCol]),c(LatCol,LongCol,TimeCol)]
  colnames(FishPoints)<-c("Lat","Long","Time")
  
  FishCol<-viridis(length(FishPoints$Time))
  
  if(MapType == "Shapefile"){plot(BaseMap, xlim = xlims, ylim = ylims)}
  if(MapType == "RecordedPlot"){BaseMap}
  for(i in 1:length(FishPoints$Time)){
    points(x = FishPoints$Long[i], y = FishPoints$Lat[i], col = FishCol[i], cex = 2, pch = 16)
    title(main = paste("Movement map of fish #", substr(FishID, 1, nchar(FishID)), sep=  " ", collapse = NULL))
    #gradientLegend(valRange = FishPoints$Time, color = FishCol, side = 2, length = .8, Depth = .1,
    #            inside = TRUE, n.seg = 4, border.col = "black", tick.col = "Light gray", 
    #            fit.margin = TRUE )
    dev.off()
  }
}
# Need to figure out how to make the plots play nice.
MapThatFish(FishToPlot[27],HydroMap,"Shapefile",SRCWaterRelInc,"Lat","Lon","TagID","dtf",xlims = c(-123,-118), ylims = c(37,38))

MapThatFish(FishToPlot[126],HydroCali,"RecordedPlot",SRCWaterRelInc,"Lat","Lon","TagID","dtf")

MapThatFish(FishToPlot[324],HydroCali,"RecordedPlot",SRCWaterRelInc,"Lat","Lon","TagID","dtf")




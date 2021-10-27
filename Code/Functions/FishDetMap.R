# Function FishDetMap

# # # Description

# Generates a plot of all fish ID's passed to it from the detection data frame passed to it overlaid on any 
# underlying shapefiles passed to it. It is recommended to pass a local geography and a hydrologic network shapefile, but not required.
# Each detection is indicated as a point colored based on a gradient colormap (uses Viridis for accessibility) bound to whatever value
# is passed to it.
# User can input limits as a percentage overlap beyond the range of points included.
# All position data MUST be in lat long, and you should check to be sure projections match up (WGS 86 is preferred)
# Currently, releases must be converted into pseudodetection points

# # # Args

# RelFrame, the release data frame input for plotting
# IDCol, fish ID column name
# LatCol, latitude column name
# lonCol, longitude column name
# LandMapDb, map function database name
# LandMapReg, map function region name
# LandColor, color chosen for land, defaults to "Dark Green"
# WaterMaps, any shapefiles to plot hydrologic networks
# WaterColor, color chosen for water, defaults to "Dark Blue"
# FishID, any fish to be used for a single plot
# ColorFactor, the column name by which to determine colorings, defaults to none used
# MonoCol, optional argument used for when ColorFactor is not given, defaults to black
# Palette, a list of two or more colors to base a palette on, defaults to "Yellow4" to "Red"
# cex, the cex scaling for the points used, defaults to 1
# DecLim, the percentage "excess" framing for the plot, expressed as a decimal, defaults to .1
# pch, the icon selected to indicate detection locations, defaults to 16

# # # FishDetMap

FishDetMap<-function(RelFrame,IDCol,LatCol,LonCol,LandMaps = "Unlisted", LandColor = "Dark Green", WaterMaps = "Unlisted", WaterColor = "Dark Blue",
                     FishID, ColorFactor = NA, MonoCol = "Black", Palette = c("Yellow","Red"), cex = 1, DecLim = .1, pch = 16, textlab = FALSE,
                     SetLims = "Unlisted", RkmCol, nonNew = FALSE, StepDex = 1){
  if(!IDCol %in% colnames(RelFrame)){
    stop("Error in FishDetMap: Fish ID column selected not found in release data frame")
  }
  if(!LatCol %in% colnames(RelFrame)){
    stop("Error in FishDetMap: Latitude column selected not found in release data frame")
  }
  if(!LonCol %in% colnames(RelFrame)){
    stop("Error in FishDetMap: Longitude column selected not found in release data frame")
  }
  if(!is.na(ColorFactor)){
    if(length(Palette) < 2){
      stop("Error in FishDetMap: Palette must include two or more colors")
    }
  if(!ColorFactor %in% colnames(RelFrame)){
    stop("Error in FishDetMap: ColorFactor column selected not found in release data frame")}
    if(!is.na(MonoCol)){
      warning("Warning in FishDetMap: MonoCol ignored; only incorporated when ColorFactor is left blank")
    }}
  if(any(!FishID %in% RelFrame[,IDCol])){
    stop("Error in FishDetMap: FishIDs selected not found in release data frame")
  }
  if(!pch %in% c(0:25)){
    stop("Error in FishDetMap: Invalid pch selected")
  }
  if(is.na(as.numeric(cex))){
    stop("Error in FishDetMap: cex must be numeric")
  }
  if(cex < 0){
    stop("Error in FishDetMap: cex must be positive")
  }
  if(is.na(as.numeric(DecLim))){
    stop("Error in FishDetMap: DecLim must be numeric")
  }
  if(DecLim < 0){
    stop("Error in FishDetMap: DecLim must be positive")
  }

  if(any(is.na(as.numeric(RelFrame[,LatCol])))){
    stop("Error in FishDetMap: Latitude must be numeric")
  }
  if(any(is.na(as.numeric(RelFrame[,LonCol])))){
    stop("Error in FishDetMap: Longitude must be numeric")
  }
  if(!nonNew){plot.new()}
 
  # Initial input error checking done
  FishInd<-which(RelFrame[,IDCol] %in% FishID)
  if(StepDex != 1){
    LabFrame<-RelFrame[FishInd,]
    LabFrame$N <- 1:length(LabFrame[,1])
    LabFrame$StepMod <- LabFrame$N %% 5
    LabFrame <- LabFrame[which(LabFrame$StepMod == 0),]
  } else {
    LabFrame<-RelFrame[FishInd,]
  }
  
  par(pty = "s")
  if(typeof(SetLims) == "character"){
  xRange<-range(RelFrame[,LonCol])
  xOffset<-(xRange[2]-xRange[1])*DecLim
  xLimsinit<-xRange+c(-xOffset,xOffset)
  xCenter<-mean(xLimsinit)
  xReach<-xLimsinit[2]-xCenter
  
  yRange<-range(RelFrame[,LatCol])
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
  }
  
  else {
    xLims <- SetLims[c(1,2)]
    yLims <- SetLims[c(3,4)]
  }
  
  if(is.na(ColorFactor == TRUE)){
   if(typeof(LandMaps) != "character"){plot(LandMaps, col = LandColor, 
                                            xlim = xLims, ylim = yLims, ann = FALSE)}
    if(typeof(WaterMaps) != "character"){plot(WaterMaps, col = WaterColor, ann = FALSE, add = TRUE,
                                              xlim = xLims, ylim = yLims, lwd = 1)}
    points(x = RelFrame[FishInd,LonCol],y = RelFrame[FishInd,LatCol],type = "p", col = MonoCol, pch = pch, cex = cex,
           xlim = xLims, ylim = yLims)
    if(textlab == TRUE) {text(x = LabFrame[,LonCol],
                              y = LabFrame[,LatCol], adj = c(0,1),
                              labels = LabFrame[,RkmCol], cex = .75, col = "White")}
}
  if(!is.na(ColorFactor == TRUE)){
    if(typeof(LandMaps) != "character"){plot(LandMaps, col = LandColor, 
                                             xlim = xLims, ylim = yLims, ann = FALSE)}
    if(typeof(WaterMaps) != "character"){plot(WaterMaps, col = WaterColor, add = TRUE, ann = FALSE,
                                              xlim = xLims, ylim = yLims, lwd = 1)}
  ValSet<-RelFrame[which(RelFrame[,IDCol] %in% FishID),]
  FishPalette<-rgb((1:100)/100,(1:100)/100,(1:100)/100)
  ValSet[,"Coloring"]<-NA
  if(is.numeric(ValSet[,ColorFactor])){
    ValSet[,"Coloring"]<-FishPalette[as.numeric(cut(ValSet[,ColorFactor], breaks = 100))]
  } else {
      for(i in 1:length(unique(ValSet[,ColorFactor]))){
        ValSet[which(ValSet[,ColorFactor] %in% unique(ValSet[,ColorFactor])[i]),
               "Coloring"] <- as.character(i)
      }
    }
  
  points(x = RelFrame[FishInd,LonCol],y = RelFrame[FishInd,LatCol],type = "p", 
         col = ValSet$Coloring, pch = pch, cex = cex,

         add = TRUE,
         xlim = xLims, ylim = yLims)
  if(textlab == TRUE) {text(x = LabFrame[,LonCol],
                            y = LabFrame[,LatCol], adj = c(0,1),
                            labels = LabFrame[,RkmCol], cex = .75, col = "White")}
  
}

  # par(xlims = xLims, ylims = yLims)
  # if(length(FishID)>5){for(i in 2:length(FishID))
  #   if(((i-1) %% 5) == 0){
  #   FishID[i]<-paste0(FishID[i],"/n", collapse = "")
  #     }}
  IDstr<-paste(FishID,collapse = ", ")
# add an index for where IDstr ==
  titlestr<-paste(strwrap(paste("Movement map of Fish", IDstr, sep = " ", collapse = NULL), width = 50), 
                   collapse = "\n")
  
    title(main = titlestr, xlab = "Longitude",
          ylab = "Latitude")

        #Add in a legend?
}

# R G B ing time: Just writing as a comment for now, will actually pull later.

# W2B<-rgb((1:100)/100,(1:100)/100,(1:100)/100)
# 

# # # Possible future features

# Currently only allows for splitting based on coloring index factor and which fish you're using, could 
# add a way to distinguish pch between releases, detections, and maybe endpoints or critical route detections? Possibly add an arg
# to split based on a character vector?
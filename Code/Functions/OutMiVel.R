# Function OutMiVel

# # # Description

# OutMiVel takes a salmonid release tracking data frame and calculates stepwise displacement for each detection.
# From this, it generates downstream velocity using a timing window based on steps (e.g., if arg step  = 1, it will use just the 
# immediate time step, if step = 3, it will add the timing and displacement of the steps on either side).
# Additionally, it sorts based on individual fish and time of detection
# Releases must be converted into pseudodetection points


# # # Args

# RelFrame, the release data frame input for processing
# TStep, the time step chosen. MUST BE ODD, defaults to 1
# Weight, uses a decimal to weight the incrmental influence of each time step included on mean velocity at point, defaults to 1
#     (e.g., if TStep = 3 and Weighting = .5, then Vel[i]<-(Dis[i]+((Dis[i-1]+Dis[i+1])/2)/(Time[i]+((Time[i-1]+Time[i-2])/2))
#     Helps attenuate the buffering and prevent movements from being attributed to the wrong time step.
# Fish, the column name with reciever ID's or however else fish are individually marked
# Times, the column name with timing data. Highly recommend including both seconds and fractional seconds, if that's important to your data.
# Rkm, the column name with rkm data

# # # Output

# RefFrame, a version of RelFrame that has been sorted by fish and detection timing and has had displacement and velocity calculated.

OutMiVel<-function(RelFrame, TStep = 1, Weight = 1, Fish, Times, Rkm){
  # Input error checks
  RefFrame <- RelFrame
  
  if(any(is.na(as.numeric(RefFrame[,Times])))){
    stop("Error in OutMiVel: Times must have numeric equivalent")
  }
  if(any(is.na(as.numeric(RefFrame[,Rkm])))){
    stop("Error in OutMiVel: Rkm must have numeric equivalent")
  }
  if(any(table(RefFrame[,c(Fish,Times)]) > 1)){
    stop("Error in OutMiVel: Repeated fish/time pairs")
  }
  if(any(is.na(RefFrame[,Fish]))){
    stop("Error in OutMiVel: Detections must be associated with a fish ID")
  }
  if(!is.numeric(TStep)){
    stop("Error in OutMiVel: Time step must be numeric")
  } 
  if(!((TStep %% 1) == 0)){
      stop("Error in OutMiVel: Time step must be an integer")
  }
  if((TStep %% 2) == 0){
    stop("Error in OutMiVel: Time step must be odd")
  }
  if(!is.numeric(Weight)){
    stop("Error in OutMiVel: Weight must be numeric")
  }
  if(Weight > 1){
    stop("Error in OutMiVel: Weight must be <= 1")
  }
  if("Vel" %in% colnames(RefFrame) | "Disp" %in% colnames(RefFrame)){
    stop("Error in OutMiVel: Displacement and/or Velocity columns being overwritten")
  }
  #Sorting the data frame by fish and time
  OrdFrame<-RefFrame[order(RefFrame[,Times]),]
  RefFrame<-OrdFrame[order(OrdFrame[,Fish]),]
  
  RefFrame[,"Disp"]<-NA
  RefFrame[,"Vel"]<-NA
  
  # Set up weighting and stepwise indices
  if(TStep != 1){
  Tdex<-matrix(data = 1:TStep, nrow = 1, ncol = TStep)
  Tdex<-Tdex-mean(Tdex) }
  
  # Weighting Index
  if(Weight != 1){
  Wdex<-matrix(data = NA, nrow = 1, ncol = TStep)
  for(k in 1:TStep){
    Wdex[k] <- Weight^(abs(Tdex[k]))
  }}

  # Begin the for loops
  FishSet<-unique(RefFrame[,Fish])

  
    for(i in 1:length(FishSet)){
    
        TimeRkmPairs<-RefFrame[which(RefFrame[,Fish] == FishSet[i]),c(Times,Rkm)]
        TimeSet<-TimeRkmPairs[,Times]
        RkmSet<-TimeRkmPairs[,Rkm]
        Fishdex<-which(RefFrame[,Fish] == FishSet[i])
        # Displacement
        for(j in 1:length(TimeSet)){
          Timedex<-which(RefFrame[,Times] == TimeSet[j])
          # NEED TO FIX REPIND
          
          Repind <- Fishdex[which(Fishdex %in% Timedex)]
          
          if(j == length(TimeSet)){
            RefFrame[Repind, "Disp"] <- 0} 
          
          else {RefFrame[Repind, "Disp"] <- (RkmSet[j+1]-RkmSet[j])}
          }
        
        DispSet<-RefFrame[Fishdex,"Disp"]
        
        # Velocity
        for(j in 1:length(TimeSet)){
          
          Timedex<-which(RefFrame[,Times] == TimeSet[j])
          # NEED TO FIX REPIND
          Repind <- Fishdex[which(Fishdex %in% Timedex)]
          
          if(TStep == 1){
            if(j == length(TimeSet)){
              RefFrame[Repind, "Vel"] <- 0
            } else{
            RefFrame[Repind, "Vel"] <- DispSet[j]/(TimeSet[j+1]-TimeSet[j]) 
          }} else{

              # For cases where step would include values before time series start
              if(any(j + Tdex == 1)){

                TdexMin <- which((j + Tdex) == 1)
                Veldex <- matrix(data = NA, nrow = 1, ncol = length(TdexMin:TStep))
                Tdiff<-TimeSet[j+Tdex[TStep]+1]-TimeSet[j+Tdex[TdexMin]]
                
                for(k in TdexMin:TStep){
                  Veldex[k] <- (DispSet[j+Tdex[k]]*Wdex[k])
                }
                VelW<-sum(Veldex[TdexMin:TStep])/Tdiff
                RefFrame[Repind, "Vel"] <- VelW
              }
          # For cases where step would include values after time series end
          else{ if(any(j + Tdex == length(TimeSet))){
                # It seems to be an issue w/ the if statement wherein it runs unacceptable values, i.e., j +
                TdexMax <- which((Tdex + j) == length(TimeSet))
                Veldex <- matrix(data = NA, nrow = 1, ncol = TdexMax)
                Tdiff<-TimeSet[j+Tdex[TdexMax]+1]-TimeSet[j+Tdex[1]]
                
                for(k in 1:TdexMax){
                  Veldex[k] <- (DispSet[j+Tdex[k]])*Wdex[k]
                }
                VelW<-sum(Veldex[1:TdexMax])/Tdiff

                RefFrame[Repind, "Vel"] <- VelW
             } 
            else {
              # All other cases
              Veldex <- matrix(data = NA, nrow = 1, ncol = TStep)
              Tdiff<-TimeSet[j+Tdex[TStep]+1]-TimeSet[j+Tdex[1]]
              for(k in 1:TStep){
                Veldex[k] <- (DispSet[j+Tdex[k]])*Wdex[k]
              }
              VelW<-sum(Veldex[1:TStep])/Tdiff
              RefFrame[Repind, "Vel"] <- VelW
            
            }}}
          }
  }
  return(RefFrame)
}



# # # Potential future additions

# Currently relies on Rkm to calc displacement, could instead use a paired path distance table


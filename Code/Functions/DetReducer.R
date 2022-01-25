# functionname: DetReducer

####################################

##### Function Description:

# DetReducer takes a condensed dataframe of detection (as output by DetCondenser) and creates a reduced dataframe of steps 
# between sites based on the given criteria with the reduction process being determined by the "Method" and "SiteSet" args.
# Four reduction processes are given based on the "Method" arg, with three being based on a predetermined set of steps and the 
# last being based on the steps present in the chosen fish subsets of the dataframe. The "Max", "Min" and 
# "Shortest" methods all take a "SiteSet" argument in the form of a set of paired sites in the form of an [X,2] matrix where X is the number 
# of steps given and each column gives departure and arrival sites, respectively. 
# "Max" returns the version where each unique step is represented exactly once, with each step containing every instance of 
# its endpoints found in the subset. 
# "Min" returns the version with every equivalent step between the departure and arrival sites found in the dataset.
# "Shortest" returns the version with each step between the departure and arrival sites that do NOT contain any instances of 
# its endpoints.
# By contrast "Similar" does not take a "SiteSet" argument and will ignore it. The "Similar" method instead takes a set of fish 
# and returns the most similar set of steps for each fish. It does so by comparing the sites represented in each and then 
# generating a set of arrivals and departures which recreates the shortest of the paths in the form of only steps which can be 
# found within all fishes' detection history

##### Function Arguments

# DenseFrame: the condensed dataframe of detections to be used
# FishSet: the IDs of each fish to be included
# SiteCol: the column of sites to be used 
# t1Col: the column of arrival times
# t2Col: the column of arrival times
# fCol: the column of fish IDs
# Method: the method to be used for reduction, defaults to "LSTime"


DetReducer<-function(DenseFrame,FishSet,SiteCol,t1Col,t2Col,fCol, Method = "LSTime"){
  
  if(any(!(FishSet %in% DenseFrame[,fCol]))){
    stop("Error in DetReducer: Fish not found in the condensed dataframe")
  }
  # 
  # if(Method == "Similar"){
  #   if(length(FishSet) < 2){
  #   stop("Error in DetReducer: Method 'Similar' requires more than one fish")
  # }}
  # 
  # if(Method == "Max"){
  #   if(SiteSet == "Ungiven"){
  #     stop("Error in DetReducer: Method given requires arg 'SiteSet'")
  # }}
  # 
  # if(Method == "Min"){
  #   if(SiteSet == "Ungiven"){
  #     stop("Error in DetReducer: Method given requires arg 'SiteSet'")
  # }}
  # 
  # if(Method == "Shortest"){
  #   if(SiteSet == "Ungiven"){
  #     stop("Error in DetReducer: Method given requires arg 'SiteSet'")
  # }}
  # 
  # if(Method != "Similar"){
  #   if(dim(SiteSet)[2] != 2){
  #     stop("Arg 'SiteSet' inappropriately formatted")
  # }}
  
  # Temporary error message until I update the LCS utility
  if(length(FishSet) != 2){
    stop("Error in DetReducer: functionality currently limited to only two fish.")
  }
  
  
  #Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")
  
  
  SubF<-list()
  SeqList<-list()
  LookDims<-vector(mode = )
  for(i in 1:length(FishSet)){
    SubF[[i]]<-DenseFrame[which(DenseFrame[,fCol] %in% FishSet[i]),]
    SeqList[[i]]<-SubF[[i]][,SiteCol]
    LookDims[i]<-length(SeqList[[i]])
  }
  
  LookTable<-array(data = NA, dim = LookDims)
  CLookTable<-array(data = NA, dim = LookDims)
  # if(any(!(c(SiteSet[,1],SiteSet[,2]) %in% SeqMat[,]))){
  #   stop("Error in DetReducer: Arg 'SiteSet' contains sites not found in the condensed dataframe")
  # }
  # 
  # if(! Method %in% c("Max","Min","Shortest","Similar")){
  #   stop("Error in DetReducer: Value given for arg 'Method' not found")
  # }
  
  
  # TEMPORARY SECTION FOR LIMITED LCS UTILITY
  LCSSet<-LCSExtract(SeqList[[1]],SeqList[[2]])
  
  
  # Start of actual processing
  if(Method == "LSTime"){
    # Section that gives us the version from an LCS constructor set that gives the least squares time offset
    MLSMulti<-function(ValSet){
      ValMean<-mean(ValSet, na.rm = TRUE)
      ValSquares<-vector(mode = "double", length = 0)
      for(i in 1:length(ValSet)){
        ValSquares[i]<-(ValSet[i]-ValMean)^2
      }
      VSNet<-sum(ValSquares, na.rm = TRUE)
      return(c(VSNet,ValMean))
    }
    
    # Core logic structure: Pull constructors with LCSExtract, then start using the 
    # lookup table to test to calculate a vector of LS time displacement scores,
    # then extract a data frame for the pair of fish based on that.
    LSTVect<-vector(mode = "double",length = 0)
    #print(LSTVect)
    nConstr<-length(LCSSet)
    nDims<-length(FishSet)
    LCSLen<-length(LCSSet[[1]][[1]])
    ConstrVec<-vector(mode = "integer", length = 0)
    LSTVals<-vector(mode = "double")
    CenTVals<-vector(mode = "double")
    LSTSet<-list()
    CenTSet<-list()
    OutList<-list()
    LSTFrame<-data.frame()
    OutFrame<-matrix()
    for(i in 1:nConstr){
      for(j in 1:LCSLen){
        # For each LCS, at each position, check to see if sets of constructor positions
        # have already had their squared time differences measured, and add
        # them to a vector for each position, which will be added together to get 
        # a measure for the least squares.
      for(k in 1:nDims){
        ConstrVec[k]<-as.numeric(LCSSet[[i]][[k+1]][j])
      }
      #print(ConstrVec)
      
        # Here we check lookup table
      if(is.na(LookTable[deparse(ConstrVec)])){
        ArrSet<-vector(mode = "double",length = 0)
        DepSet<-vector(mode = "double",length = 0)
        for(k in 1:nDims){
          ArrSet[k] <- as.numeric(SubF[[k]][ConstrVec[k],t1Col])
          DepSet[k] <- as.numeric(SubF[[k]][ConstrVec[k],t2Col])
        }
        # print(ArrSet)
        # print(DepSet)
        MLSVal<-MLSMulti(c(ArrSet,DepSet))[1]
        CenTVal<-MLSMulti(c(ArrSet,DepSet))[2]
        #DepLS<-MLSMulti(DepSet)
        # print(ArrLS)
        # print(DepLS)
        #SLSVal<-mean(c(ArrLS,DepLS), na.rm = TRUE)
        #print(SLSVal)
        LookTable[deparse(ConstrVec)]<-MLSVal
        LSTVals[j]<-MLSVal
        CenTVals[j]<-CenTVal
        CLookTable[deparse(ConstrVec)]<-CenTVal
        #print(LSTVals)
        
      } else {
        LSTVals[j]<-LookTable[deparse(ConstrVec)]
        CenTVals[j]<-CLookTable[deparse(ConstrVec)]
      }
      }
      # print(LSTVals)
      LSTSet[[i]] <- LSTVals
      CenTSet[[i]] <- CenTVals
      LSTVect[i]<-mean(LSTVals, na.rm = TRUE)
    }
    # print(LSTVect)
    ConstrIndex<-which(LSTVect %in% min(LSTVect))
    # print(ConstrIndex)
    if(length(ConstrIndex) == 0){
      stop("No LCS found.")
    }
    
    if(length(ConstrIndex) > 1){
    print("Warning: Multiple equally good constructor sets available. Please choose from the printed list, or stop running.")
    set.seed(ConstrIndex[2])
    ConstrIndex <- ConstrIndex[sample(1:length(ConstrIndex),1)]
    }
    
    
    RedFrame<-SubF[[1]][as.numeric(LCSSet[[ConstrIndex]][[2]]),]
    #print(SubF[[2]][as.numeric(LCSSet[[ConstrIndex]][[3]]),])
    for(i in 2:nDims){
      RedFrame<-rbind(RedFrame,SubF[[i]][as.numeric(LCSSet[[ConstrIndex]][[i+1]]),])
    }
    #print(SubF[[1]][as.numeric(LCSSet[[ConstrIndex]][[2]]),SiteCol])
    OutFrame<-data.frame(SubF[[1]][as.numeric(LCSSet[[ConstrIndex]][[2]]),SiteCol])
    
    # Add a variant on LSTSet for the actual mean times
    # print(LSTSet[[ConstrIndex]])
    # print(OutFrame)
    # print(RedFrame)
    # It's probably right here
    OutFrame$LST <- LSTSet[[ConstrIndex]]
    OutFrame$tCenter <- CenTSet[[ConstrIndex]]

    for(i in 0:(nDims-1)){
      OutFrame[,3 + 2*i + 1] <- SubF[[i+1]][as.numeric(LCSSet[[ConstrIndex]][[i+2]]),t1Col]
      OutFrame[,3 + 2*i + 2] <- SubF[[i+1]][as.numeric(LCSSet[[ConstrIndex]][[i+2]]),t2Col]
    }
    colnames(OutFrame) <- c("Site","MLS Disp","Center Time",paste("T",seq(1:(nDims*2))))
    OutList[[1]] <- RedFrame
   OutList[[2]] <- OutFrame
    return(OutList)
  }
  
  
  # if(Method == "Max"){
  #   # Section that runs the process for maximum reduction based on the site pair sequence given
  #   
  #   # Process: for each fish, find the first instance of SiteSet[i,1] and the last instance of SiteSet[i,2] and pull the resp.
  #   # departure and arrival times. 
  #   return(RedFrame)
  # }
  # 
  # if(Method == "Min"){
  #   # Section that runs the process for minimum reduction based on the site pair sequence given
  #   
  #   # Process: for each fish, for each SiteSet[i,1], find every postsequent instance of SiteSet[i,2] and pull times
  #   return(RedFrame)
  # }
  # 
  # if(Method == "Shortest"){
  #   # Section that runs the process for reduction to shortest steps based on SPS given
  #   
  #   # Process: for each fish, find each pair of incidences of SiteSet[i,1] and SiteSet[i,2] that contain no instances of themselves
  #   # between them. Pull times.
  #   return(RedFrame)
  # }
  # 
  # if(Method == "Similar"){
  #   # Section that runs the process for reduction to most similar steps based on SPS given. This will use at least one C++ call.
  #   # and it will take some doing.
  #   
  #   # Process pt 1: find all possible Longest Common Subsequences for each pair of fish (we may add a method for doing this for 
  #   # more than two fish concurrently later on, but the base method is to give pairs)
  #   
  #   # Process pt 2: Select an LCS based on the following criteria: Must include the greatest # of unique sites,
  #   # Must have the earliest possible start and latest possible end
  #   # I'm sure we can come up with more, but we'll start with this. After these two criterai are met, select the first in the set.
  #   
  #   # Process pt 3: Find every possible subsequence of each fish's detections that could produce the LCS. Select within these to 
  #   # prioritize the smallest median step sizez (in # of locations) and avoid steps that start with backtracking to the start of 
  #   # the prior step. If there are still multiple options after these criteria are met, select the first in the set for each.
  #   
  #   # Process pt 4: Use these sequences to pull paired departure arrival data in these reduced detection sets for each fish.
  # 
  # 
  #   return(RedFrame)
  # }
  # 
  
  
  
  
}








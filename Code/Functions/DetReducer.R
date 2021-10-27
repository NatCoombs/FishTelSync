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
# Method: the method to be used for reduction, defaults to "Similar"
# SiteSet: the arbitrary, predetermined set of sites to be used for analysis

DetReducer()<-function(DenseFrame,FishSet,SiteCol,t1Col,t2Col,Method = "Similar",SiteSet = "Ungiven"){
  
  if(any(!(FishSet %in% DenseFrame[,fCol]))){
    stop("Error in DetReducer: Fish not found in the condensed dataframe")
  }
  
  if(Method == "Similar"){
    if(length(FishSet) < 2){
    stop("Error in DetReducer: Method 'Similar' requires more than one fish")
  }}
  
  if(Method == "Max"){
    if(SiteSet == "Ungiven"){
      stop("Error in DetReducer: Method given requires arg 'SiteSet'")
  }}
  
  if(Method == "Min"){
    if(SiteSet == "Ungiven"){
      stop("Error in DetReducer: Method given requires arg 'SiteSet'")
  }}
  
  if(Method == "Shortest"){
    if(SiteSet == "Ungiven"){
      stop("Error in DetReducer: Method given requires arg 'SiteSet'")
  }}
  
  if(Method != "Similar"){
    if(dim(SiteSet)[2] != 2){
      stop("Arg 'SiteSet' inappropriately formatted")
  }}
  

  SubF<-list()
  SeqMat<-matrix()
  for(i in 1:length(FishSet)){
    SubF<-c(SubF,DenseFrame[which(DenseFrame[,fCol] %in% FishSet[i]),])
    SeqMat[,i]<-DenseFrame[which(DenseFrame[,fCol] %in% FishSet[i]),SiteCol]
  }
  
  if(any(!(c(SiteSet[,1],SiteSet[,2]) %in% SeqMat[,]))){
    stop("Error in DetReducer: Arg 'SiteSet' contains sites not found in the condensed dataframe")
  }
  
  if(! Method %in% c("Max","Min","Shortest","Similar")){
    stop("Error in DetReducer: Value given for arg 'Method' not found")
  }
  
  # Start of actual processing
  
  
  if(Method == "Max"){
    # Section that runs the process for maximum reduction based on the site pair sequence given
    
    # Process: for each fish, find the first instance of SiteSet[i,1] and the last instance of SiteSet[i,2] and pull the resp.
    # departure and arrival times. 
    return(RedFrame)
  }
  
  if(Method == "Min"){
    # Section that runs the process for minimum reduction based on the site pair sequence given
    
    # Process: for each fish, for each SiteSet[i,1], find every postsequent instance of SiteSet[i,2] and pull times
    return(RedFrame)
  }
  
  if(Method == "Shortest"){
    # Section that runs the process for reduction to shortest steps based on SPS given
    
    # Process: for each fish, find each pair of incidences of SiteSet[i,1] and SiteSet[i,2] that contain no instances of themselves
    # between them. Pull times.
    return(RedFrame)
  }
  
  if(Method == "Similar"){
    # Section that runs the process for reduction to most similar steps based on SPS given. This will use at least one C++ call.
    # and it will take some doing.
    
    # Process pt 1: find all possible Longest Common Subsequences for each pair of fish (we may add a method for doing this for 
    # more than two fish concurrently later on, but the base method is to give pairs)
    
    # Process pt 2: Select an LCS based on the following criteria: Must include the greatest # of unique sites,
    # Must have the earliest possible start and latest possible end
    # I'm sure we can come up with more, but we'll start with this. After these two criterai are met, select the first in the set.
    
    # Process pt 3: Find every possible subsequence of each fish's detections that could produce the LCS. Select within these to 
    # prioritize the smallest median step sizez (in # of locations) and avoid steps that start with backtracking to the start of 
    # the prior step. If there are still multiple options after these criteria are met, select the first in the set for each.
    
    # Process pt 4: Use these sequences to pull paired departure arrival data in these reduced detection sets for each fish.
  
  
    return(RedFrame)
  }
  
  
  
  
  
}








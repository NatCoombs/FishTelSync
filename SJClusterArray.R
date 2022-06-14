# File to be sourced by the shell script for SLURM to be run on the cluster.

# Package check
if(! "Rcpp" %in% installed.packages()[,"Package"]){
  install.packages("Rcpp", repos = "http://cran.us.r-project.org")
}
library("Rcpp")

# Source appropriate functions
source(file = "./Functions/LCSCalc.R")
source(file = "./Functions/MFPop.R")
source(file = "./Functions/MarkovFishTable.R")
source(file = "./Functions/MarkovFishSurrogates.R")
source(file = "./Functions/MFSig.R")
Rcpp::sourceCpp(file = "./Functions/FullLCSExtractor.cpp")
# One arg: the key we're on at this point and then the working directory.
Index <- commandArgs(trailingOnly = TRUE)[1]

Pairs <- read.csv(file = paste0("./Data/SJReduced/Pairs", Index, ".csv"))
Dets <- read.csv(file = paste0("./Data/SJReduced/Dets", Index, ".csv"))

# Evaluate synchrony of the pairs
Synch <- LCSCalc(Dets,Pairs)

# Create a Markov state table & generate surrogate fish
MTable <- MarkovFishTable(Dets, "FishID","Loc","FirstDet","LastDet")
Surrogates <- MarkovFishSurrogates(MTable, "Rel",0,100000,Reps = 1)
  
# Significance test synchrony of releases
PopSig <- MFPop(Synch, Pairs, Surrogates, "Rel", 10000)
# Significance test synchrony of pairs
SplitKeys <- which(Surrogates[,"IDCol"] <= 50000)
SurrogSplit1 <- Surrogates[SplitKeys,]
SurrogSplit1 <- Surrogates[-SplitKeys,]

PairSig <- MFSig(Pairs, Dets, Synch, SurrogSplit1, SurrogSplit2, 10000)
# Write outputs (e.g., synchrony vals & p-values as an Nx8 matrix of F1, F2, val, p-val, val, p-val, val, p-val) to csv
SynchMat<-matrix(data = NA, nrow = dim(Pairs)[1], ncol = 3)
for(i in 1:dim(Pairs)[1]){
  SynchMat[i,1] <- Synch[[i]][[7]]
  SynchMat[i,2] <- Synch[[i]][[14]]
  SynchMat[i,3] <- Synch[[i]][[21]]
}
colnames(SynchMat) <- c("LDSynch","PSSynch","LSSynch")

PairSynch<-cbind(Synchmat,PairSig)

if(!dir.exists("./Outputs")){
  dir.create("./Outputs")
}
if(!dir.exists("./Outputs/Pops")){
  dir.create("./Outputs/Pops")
}
if(!dir.exists("./Outputs/Pairs")){
  dir.create("./Outputs/Pairs")
}

write.csv(PairSynch, file = paste0("./Outputs/Pairs/", Index, ".csv"), row.names = FALSE)
saveRDS(PairSynch, file = paste0("./Outputs/Pairs/", Index, ".rds"))
cat("Run #", Index, "has been completed.")
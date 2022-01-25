# Linearized script for the cluster

SHK<-read.csv("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Data/sac_river_steelhead.csv",
              stringsAsFactors = F,na.strings=c("NULL",""),nrows=1782042)


source("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Code/Functions/DetCondense.R")


# Just a file for preliminary investigation of the data
StationSerials<-unique(SRC$Receiver_ser_num)
StationSet<-matrix(data = NA, nrow = 164, ncol = 3)
colnames(StationSet)<-c("Station","Lat","Long")

for(i in 1:length(StationSerials)){
  StationSet[i,1]<-as.character(StationSerials[i])
}

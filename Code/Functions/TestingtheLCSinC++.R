# Testing the LCS function

install.packages("Rcpp", repos="https://rcppcore.github.io/drat")

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

TestLCS<-LCSExtract(c('A','B','C','D'),c('A','C','B','D'),c(0,2,4,6),c(1,3,5,7),c(0,4,5,8),c(1,4.5,6,12),c("All"))

typeof(TestLCS[[1]])

TestSeq1<-sample(1:40,size = 200, replace = TRUE, prob = NULL)
TestSeq2<-sample(1:40,size = 200, replace = TRUE, prob = NULL)

RandTestLCS<-LCSExtract(TestSeq1,TestSeq2)


Rcpp::sourceCpp("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Code/HWTestFromCoatless.cpp")

Rcpp::cppFunction(code = 'void test() { Rcpp::CharacterVector cv(1); }')
Rcpp::evalCpp("2 + 2")


# Run some test blocks
# Update tests to work for new format
LCSExtract(c('A','B','C','D'),c('A','C','B','D'),c(0,2,4,6),c(1,3,5,7),c(0,4,5,8),c(1,4.5,6,12),c("All")) #

LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D'),c(0,2,4,4.5,6,8),c(1,3,4.3,5,7,9),c(0,2,4,6),c(1,3,5,7),c("All")) #

LCSExtract(c('A','B','A','B'),c('A','B'),c(0,2,4,6),c(1,3,5,7),c(0,4),c(1,4.5),c("All")) #

LCSExtract(c('ALPHA','BETA','CHARLIE','DELTA'),c('ALPHA','CHARLIE','BETA','DELTA'),c(0,2,4,6),c(1,3,5,7),c(0,4,5,8),c(1,4.5,6,12),c("All")) #

LCSExtract(c('A','B','C','D'),c('a','C','B','D'),c(0,2,4,6),c(1,3,5,7),c(0,4,5,8),c(1,4.5,6,12),c("All")) #

LCSExtract(c('1','2','3','4'),c('1','3','2','4'),c(0,2,4,6),c(1,3,5,7),c(0,4,5,8),c(1,4.5,6,12),c("All")) #

LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D','B','C','D'),c(0,2,4,4.5,6,8),c(1,3,4.3,5,7,9),c(0,2,4,6,8,10,12),c(1,3,5,7,9,11,13),c("All")) #

LCSExtract(c('A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'),c(0,2,4,4.5,6,8,10),c(1,3,4.3,5,7,9,11),c(0,2,4,6,8,10,12,14),c(1,3,5,7,9,11,13,15),c("All")) #

#PhaSim bugged here
LCSExtract(c('A','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'),c(0,2,4,4.5,6,8,10,12),c(1,3,4.3,5,7,9,11,13),c(0,2,4,6,8,10,12,14),c(1,3,5,7,9,11,13,15),c("All")) #

LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'),c(0,2,4,4.5,6,8,10,12,14),c(1,3,4.3,5,7,9,11,13,15),c(0,2,4,6,8,10,12,14),c(1,3,5,7,9,11,13,15),c("All")) #

#PhaSim is still bugged here and I do not know why
LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','B','D','B','C','D','E'),c(0,2,4,4.5,6,8,10,12,14),c(1,3,4.3,5,7,9,11,13,15),c(0,2,4,6,8,10,12,14,16),c(1,3,5,7,9,11,13,15,17),c("All")) #



set.seed(11520)
Seq1<-sample(1:30,size = 30, replace = TRUE, prob = NULL)
Seq2<-sample(1:30,size = 30, replace = TRUE, prob = NULL)
RandTest1<-LCSExtract(Seq1,Seq2)
# Passes manual check
Seq1<-sample(1:30,size = 30, replace = TRUE, prob = NULL)
Seq2<-sample(1:30,size = 30, replace = TRUE, prob = NULL)
RandTest2<-LCSExtract(Seq1,Seq2)
# First three solutions pass manual check
Seq1<-sample(1:30,size = 30, replace = TRUE, prob = NULL)
Seq2<-sample(1:30,size = 30, replace = TRUE, prob = NULL)
RandTest3<-LCSExtract(Seq1,Seq2)
# First three solutions pass manual check

# Let's rip a pair of fish sequences and test it and then call this good for now.

TestFish1<-USFWS200Rkm07[1,1]
TestFish2<-USFWS200Rkm07[3,1]
TestFishSeq1<-FWS07_200Dense[which(FWS07_200Dense$FishID %in% TestFish1),"Loc"]
TestFishSeq2<-FWS07_200Dense[which(FWS07_200Dense$FishID %in% TestFish2),"Loc"]
write.csv(TestFishSeq1, "/Users/nathanielcoombs/Downloads/TFSeq1.csv", row.names = FALSE)
write.csv(TestFishSeq2, "/Users/nathanielcoombs/Downloads/TFSeq2.csv", row.names = FALSE)

TestLCSFish<-LCSExtract(TestFishSeq1,TestFishSeq2)


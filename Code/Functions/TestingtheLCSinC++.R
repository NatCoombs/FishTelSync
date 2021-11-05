# Testing the LCS function

install.packages("Rcpp", repos="https://rcppcore.github.io/drat")

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

TestLCS<-LCSExtract(c('A','B','C','D'),c('A','C','B','D'))

TestSeq1<-sample(1:10,size = 100, replace = TRUE, prob = NULL)
TestSeq2<-sample(1:10,size = 100, replace = TRUE, prob = NULL)

RandTestLCS<-LCSExtract(TestSeq1,TestSeq2)


Rcpp::sourceCpp("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Code/HWTestFromCoatless.cpp")

Rcpp::cppFunction(code = 'void test() { Rcpp::CharacterVector cv(1); }')
Rcpp::evalCpp("2 + 2")

# Testing the LCS function

install.packages("Rcpp", repos="https://rcppcore.github.io/drat")

Rcpp::sourceCpp("Code/Functions/FullLCSExtractor.cpp")

LCSExtract(c('A','B','C','D'),c('A','C','B','D'))



Rcpp::sourceCpp("/Users/nathanielcoombs/Documents/Git/Repos/FishTelPrac/Code/HWTestFromCoatless.cpp")

Rcpp::cppFunction(code = 'void test() { Rcpp::CharacterVector cv(1); }')
Rcpp::evalCpp("2 + 2")

context("FullLCSExtractor.R")

source("FullLCSExtractor.R")

testthat::test_that("The LCS utility is running properly",{
  # Basic ABD
  testthat::expect_equal(LCSExtract(c('A','B','C','D'),c('A','C','B','D'))[[1]][[1]],c('A','B','D'))
  testthat::expect_equal(LCSExtract(c('A','B','C','D'),c('A','C','B','D'))[[1]][[2]],c('1','2','4'))
  testthat::expect_equal(LCSExtract(c('A','B','C','D'),c('A','C','B','D'))[[1]][[3]],c('1','3','4'))
  
  # Basic ALPHA BETA DELTA
  testthat::expect_equal(LCSExtract(c('ALPHA','BETA','CHARLIE','DELTA'),c('ALPHA','CHARLIE','BETA','DELTA'))
[[1]][[1]],c('ALPHA','BETA','DELTA'))
  testthat::expect_equal(LCSExtract(c('ALPHA','BETA','CHARLIE','DELTA'),c('ALPHA','CHARLIE','BETA','DELTA'))
[[1]][[2]],c('1','2','4'))
  testthat::expect_equal(LCSExtract(c('ALPHA','BETA','CHARLIE','DELTA'),c('ALPHA','CHARLIE','BETA','DELTA'))
[[1]][[3]],c('1','3','4'))
  
  # Basic case sensitive BD
  testthat::expect_equal(LCSExtract(c('A','B','C','D'),c('a','C','B','D'))[[1]][[1]],c('B','D'))
  testthat::expect_equal(LCSExtract(c('A','B','C','D'),c('a','C','B','D'))[[1]][[2]],c('2','4'))
  testthat::expect_equal(LCSExtract(c('A','B','C','D'),c('a','C','B','D'))[[1]][[3]],c('3','4'))
  
  # Basic 124
  testthat::expect_equal(LCSExtract(c('1','2','3','4'),c('1','3','2','4'))[[1]][[1]],c('1','2','4'))
  testthat::expect_equal(LCSExtract(c('1','2','3','4'),c('1','3','2','4'))[[1]][[2]],c('1','2','4'))
  testthat::expect_equal(LCSExtract(c('1','2','3','4'),c('1','3','2','4'))[[1]][[3]],c('1','3','4'))
  
  # Basic ABAB
  testthat::expect_equal(LCSExtract(c('A','B','A','B'),c('A','B'))[[1]][[1]],c('A','B'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B'),c('A','B'))[[1]][[2]],c('1','2'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B'),c('A','B'))[[1]][[3]],c('1','2'))
  
  # ABLong1
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D'))[[1]][[1]],c('A','B','C','D'))
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D'))[[1]][[2]],c('1','2','3','6'))
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D'))[[1]][[3]],c('1','2','3','4'))
  
  # ABLong2
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D','B','C','D'))[[1]][[1]],c('A','B','C','B','C','D'))
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D','B','C','D'))[[1]][[2]],c('1','2','3','4','5','6'))
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','D'),c('A','B','C','D','B','C','D'))[[1]][[3]],c('1','2','3','5','6','7'))
  
  # ABLong3
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[1]],c('A','B','C','B','C','E'))
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[2]],c('1','2','3','4','5','6'))
  testthat::expect_equal(LCSExtract(c('A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[3]],c('1','2','3','5','6','8'))
  
  # ABLong4
  testthat::expect_equal(LCSExtract(c('A','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[1]],c('A','B','C','B','C','E'))
  testthat::expect_equal(LCSExtract(c('A','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[2]],c('1','3','4','5','6','7'))
  testthat::expect_equal(LCSExtract(c('A','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[3]],c('1','2','3','5','6','8'))
  
  # ABLong5
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[1]],c('A','B','C','B','C','E'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[2]],c('1','2','5','6','7','8'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[3]],c('1','2','3','5','6','8'))
  
  # ABLong6
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[1]],c('A','B','C','B','C','E'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[2]],c('1','2','5','6','7','8'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','D','B','C','D','E'))[[1]][[3]],c('1','2','3','5','6','8'))
  
  # ABLong7
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','B','D','B','C','D','E'))[[1]][[1]],c('A','B','B','B','C','E'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','B','D','B','C','D','E'))[[1]][[2]],c('1','2','4','6','7','8'))
  testthat::expect_equal(LCSExtract(c('A','B','A','B','C','B','C','E','D'),c('A','B','C','B','D','B','C','D','E'))[[1]][[3]],c('1','2','4','6','7','9'))
  
  # Random tests
  set.seed(11520)
  
  # RTest1
  RTest1<-LCSExtract(sample(1:30,size = 30, replace = TRUE, prob = NULL),sample(1:30,size = 30, replace = TRUE, prob = NULL))
  testthat::expect_equal(RTest1[[1]][[1]],c("25", "27", "27", "30", "2",  "3"))
  testthat::expect_equal(RTest1[[1]][[2]],c("1",  "14", "15", "20", "27", "28"))
  testthat::expect_equal(RTest1[[1]][[3]],c("4",  "8",  "11", "14", "17", "18"))
  
  # RTest2
  RTest2<-LCSExtract(sample(1:30,size = 30, replace = TRUE, prob = NULL),sample(1:30,size = 30, replace = TRUE, prob = NULL))
  testthat::expect_equal(RTest2[[1]][[1]],c("7",  "3",  "28", "24", "14", "15", "25", "20"))
  testthat::expect_equal(RTest2[[1]][[2]],c("5",  "6" , "7",  "9",  "10", "12", "16", "20"))
  testthat::expect_equal(RTest2[[1]][[3]],c( "3",  "9",  "10", "19", "21", "24", "29", "30"))
  
  # RTest3
  RTest3<-LCSExtract(sample(1:30,size = 30, replace = TRUE, prob = NULL),sample(1:30,size = 30, replace = TRUE, prob = NULL))
  testthat::expect_equal(RTest3[[1]][[1]],c("26", "12", "5",  "3",  "23", "14", "27", "18", "8"))
  testthat::expect_equal(RTest3[[1]][[2]],c("3",  "5",  "9",  "11", "12", "21", "24", "26", "29"))
  testthat::expect_equal(RTest3[[1]][[3]],c("2",  "6",  "9",  "13", "16", "19", "20", "21", "23"))
  
  # Add a test or three for real fish sequences
  FTest<-LCSExtract(c("BattleCk10",
                      "SR_JellysE",
                      "SR_JellysW",
                      "SR_JellysE",
                      "SR_JellysW",
                      "SR_BendBrE",
                      "SR_BendBrW",
                      "SR_BendBrE",
                      "SR_BendBrW",
                      "SR_BendBrE",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_ChinaW",
                      "SR_ChinaE",
                      "SR_AbvRBDD_E",
                      "SR_BlwRBDD_W",
                      "SR_BlwRBDD_E",
                      "SR_BlwMillCk",
                      "SR_AbvThomes",
                      "SR_BlwGCID",
                      "SR_BlwIrvineFinch",
                      "SR_AbvChicoCk",
                      "SR_OrdBend",
                      "SR_ButteBr",
                      "SR_AbvColusaBr1",
                      "SR_MeridianBr",
                      "AR_Mouth",
                      "BtlCkCNFHWeir"),
                    c("BattleCk10","SR_JellysE","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_AbvRBDD_E","SR_BlwRBDD_E","SR_AbvThomes","SR_BlwDryCk","SR_BlwGCID","SR_BlwIrvineFinch","SR_AbvChicoCk","SR_OrdBend","SR_ButteBr","SR_AbvColusaBr1","SR_MeridianBr","SR_CCFBr","SR_Freeport","SutSlough","SR_DCCNorth","Georg_SloughN","SR_Ryde","SR_Mouth","RioVistaBr01","RioVistaBr02","RioVistaBr03","RioVistaBr01","RioVistaBr02","RioVistaBr03","RioVistaBr01","RioVistaBr03","RioVistaBr01","RioVistaBr03","RioVistaBr02","Decker_IsSW","Chipp11","Chipp01","BenBr05","BenBr03","BenBr04","BenBr03","BenBr05","BenBr04","BenBr02","BenBr06","BenBr03","BenBr05","BenBr04","BenBr02","BenBr06","BenBr05","BenBr04","BenBr03","BenBr05","BenBr04","BenBr03","BenBr05","BenBr04","BenBr03","BenBr05","BenBr04","BenBr03","BenBr04","CarBr01","CarBr04","CarBr05","CarBr04","CarBr01","CarBr02","CarBr05","CarBr04","CarBr01","CarBr02","CarBr05","CarBr04","CarBr05","CarBr02","CarBr05","CarBr04","CarBr01","CarBr02","CarBr04","CarBr01","CarBr04","CarBr03","CarBr05","CarBr03","CarBr01","CarBr05","CarBr03","CarBr05","CarBr01","CarBr02","CarBr04","CarBr02","CarBr04","CarBr01","CarBr02","CarBr04","CarBr01","CarBr02","CarBr04","CarBr01","CarBr02","CarBr04","CarBr01","CarBr02","CarBr04","CarBr02","CarBr04","CarBr01","CarBr02","CarBr01","CarBr02","CarBr01","CarBr02","CarBr01","CarBr02","CarBr01","CarBr02","CarBr01","CarBr02","CarBr01","CarBr02","CarBr01","RichBr06","RichBr05","RichBr07","RichBr06","RichBr05","RichBr06","RichBr07","RichBr05","RichBr06","RichBr07","RichBr05","RichBr07","RichBr06","RichBr05","RichBr06","RichBr07","RichBr05","RichBr06","RichBr05","RichBr06","GG6.5","GG6","GG6.5","GG6","GG6.5","GG6","GG6.5","GG6","GG6.5","GG7","GG6.5","GG7","GG7.2","GG1.5","GG7.2","GG1.5","GG2.5","GG3","GG2.5","GG3.5","GG3","GG2.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG3","GG3.5","GG2.5","GG3","GG3.5","GG2.5","GG3","GG2.5","GG3.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG3.5","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG2.5","GG3","GG8.4","GG7.7","GG7.5","GG7.7","GG7.5","GG7.7","GG7.5","GG7.7","GG7.5","GG7.7","GG7.5","GG7.7","GG9","GG9.5","GG9","GG9.5","GG9","GG9.5","GG9","GG9.5","GG9","GG9.5","GG7.7","GG8.4","GG9","GG7.7","GG8.4","GG9","GG9.5","GG7.7","GG8.4","GG9","GG9.5","GG7.7","GG8","GG8.4","GG9","GG8","GG7.7","GG8.4","GG9","GG3.5","GG8","GG7.7","GG8.4","GG9","GG8","GG7.7","GG8.4","GG3.5","GG7.7","GG4.5","GG9","GG8.4","GG7.7","GG8","GG8.4","GG9","GG4","GG3.5","GG7.5","GG7.7","GG4.5","GG4","GG3.5","GG8.4","GG4.5","GG9","GG4","GG3.5","GG7.7","GG4.5","GG3","GG4","GG8.4","GG3.5","GG4.5","GG3","GG4","GG3.5","GG4.5","GG3","GG8.4","GG4","GG3","GG8.4","GG4.5","GG4","GG4.5","GG4","GG3.5","GG4.5","GG4","GG3.5","GG4.5","GG3","GG4","GG3.5","GG4.5","GG3","GG4","GG3.5","GG3","GG4.5","GG4","GG3.5","GG4.5","GG4","GG3.5","GG4.5","GG4","GG3.5","GG4.5","GG4","GG4.5","SR_AbvRBDD_E","BtlCkCNFHWeir"))
  testthat::expect_equal(FTest[[1]][[1]],c("BattleCk10","SR_JellysE","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_ChinaW","SR_ChinaE","SR_AbvRBDD_E","SR_BlwRBDD_E","SR_AbvThomes","SR_BlwGCID","SR_BlwIrvineFinch","SR_AbvChicoCk","SR_OrdBend","SR_ButteBr","SR_AbvColusaBr1","SR_MeridianBr","BtlCkCNFHWeir"))
  testthat::expect_equal(FTest[[1]][[2]],c("1","2","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","30","32","33","34","35","36","37","38","39","41"))
  testthat::expect_equal(FTest[[1]][[3]],c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","24","25","26","28","29","30","31","32","33","34","365"))
  
})

context("DetReducer.R")

source("DetCondenser.R")

testthat::test_that("Inputs are checked for format",{
  # Generating testing data
  F1Frame<-data.frame()
  F1Frame[1:5,"Fish"]<-"Alpha"
  F1Frame[1:5,"Site"]<-c("A","B","C",'D','E')
  F1Frame[1:5,"t1"]<-c(0,2,10,25,31)
  F1Frame[1:5,"t2"]<-c(1,5,15,30,32)
  
  F2Frame<-data.frame()
  F2Frame[1:8,"Fish"]<-"Beta"
  F2Frame[1:8,"Site"]<-c("A","B","A",'B','C','D','C','E')
  F2Frame[1:8,"t1"]<-c(0,2,4,6,8,10,12,14)
  F2Frame[1:8,"t2"]<-c(1,3,5,7,9,11,13,15)
  
  F3Frame<-data.frame()
  F3Frame[1:4,"Fish"]<-"Charlie"
  F3Frame[1:4,"Site"]<-c("A","B","A",'E')
  F3Frame[1:4,"t1"]<-c(0,2,4,6)
  F3Frame[1:4,"t2"]<-c(1,3,5,7)
  
  TestFrame<-rbind(F1Frame,F2Frame,F3Frame)
  
  # Testing input errors of varying types
  testthat::expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Stuart", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish"), 
               "Fish not found in the condensed dataframe")
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish"), 
  #              "Method 'Similar' requires more than one fish")
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max"), 
  #              "Method given requires arg 'SiteSet'")
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
  #                         SiteSet = c("A","B","E")), 
  #              "Arg 'SiteSet' inappropriately formatted")
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
  #                         SiteSet = cbind(c("A","B","E"),c("B","E","F"))), 
  #              "Arg 'SiteSet' contains sites not found in the condensed dataframe")
  # 
  # Restore<-TestFrame
  # 
  # TestFrame[3,3]<-"Stuart"
  # 
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
  #                         SiteSet = cbind(c("A","B","C"),c("B","C","E"))), 
  #              "All times must be numeric or have an appropiate numeric conversion")
  # 
  # TestFrame<-Restore
  # 
  # TestFrame[3,4]<-"Stuart"
  # 
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
  #                         SiteSet = cbind(c("A","B","C"),c("B","C","E"))), 
  #              "All times must be numeric or have an appropiate numeric conversion")
  # 
  # TestFrame<-Restore
  
  # expect_error(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Stuart",
  #                         SiteSet = cbind(c("A","B","C"),c("B","C","E"))), 
  #              "Value given for arg 'Method' not found")
  
})

# testthat::test_that("The function works as expected",{
#   # Generating testing data
#   F1Frame<-data.frame()
#   F1Frame[1:5,"Fish"]<-"Alpha"
#   F1Frame[1:5,"Site"]<-c("A","B","C",'D','E')
#   F1Frame[1:5,"t1"]<-c(0,2,10,25,31)
#   F1Frame[1:5,"t2"]<-c(1,5,15,30,32)
#   
#   F2Frame<-data.frame()
#   F2Frame[1:8,"Fish"]<-"Beta"
#   F2Frame[1:8,"Site"]<-c("A","B","A",'B','C','D','C','E')
#   F2Frame[1:8,"t1"]<-c(0,2,4,6,8,10,12,14)
#   F2Frame[1:8,"t2"]<-c(1,3,5,7,9,11,13,15)
#   
#   F3Frame<-data.frame()
#   F3Frame[1:4,"Fish"]<-"Charlie"
#   F3Frame[1:4,"Site"]<-c("A","B","A",'E')
#   F3Frame[1:4,"t1"]<-c(0,2,4,6)
#   F3Frame[1:4,"t2"]<-c(1,3,5,7)
#   
#   TestFrame<-rbind(F1Frame,F2Frame,F3Frame)
#   
#   ######## Testing Outputs
#   
#   ##########
#   
#   # # # Simple initial testing of Method = "Max"
# 
#   # Alpha start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Alpha end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,4], 
#                15)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,5], 
#                31)
#   
#   #Beta start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Beta end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,4], 
#                9)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,5], 
#                14)
#   
#   # Alpha & Beta start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Alpha & Beta end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[6,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[6,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[6,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[6,4], 
#                9)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Max",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[6,5], 
#                14)
#   ##########
#   
#   # # # Testing Method = "Min"
#   
#   # Alpha start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Alpha end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,4], 
#                15)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,5], 
#                31)
#   
#   #Beta start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Beta end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[9,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[9,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[9,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[9,4], 
#                13)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[9,5], 
#                14)
#   
#   # Alpha & Beta start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Alpha & Beta end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[12,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[12,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[12,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[12,4], 
#                13)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Min",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[12,5], 
#                14)
#   
#   # Maybe add a test using Beta & Charlie to test how well min & shortest play with multidirectionality?
#   
#   ##########
#   
#   # # # Testing Method = "Shortest"
#   
#   # Alpha start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Alpha end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,4], 
#                15)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Alpha", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,5], 
#                31)
#   
#   #Beta start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Beta later
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,2], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,3], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,4], 
#                7)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = "Beta", SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[3,5], 
#                8)
#   
#   # Alpha & Beta start
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[1,5], 
#                2)
#   
#   # Alpha & Beta end
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[7,1], 
#                "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[7,2], 
#                "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[7,3], 
#                "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[7,4], 
#                13)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Shortest",
#                           SiteSet = cbind(c("A","B","C"),c("B","C","E")))[7,5], 
#                14)
#   
#   # Maybe add a test using Beta & Charlie to test how well min & shortest play with multidirectionality?
#   
#   
#   ##########
#   
#   # # # Testing Method = "Similar"
#   
#   # # Let's start with Alpha & Beta
#   
#   # Start of Alpha
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#                           )[1,1], 
#                "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#                           )[1,2], 
#                "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#                           )[1,3], 
#                "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#                           )[1,4], 
#                1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#                           )[1,5], 
#                2)
#   
#   # End of Alpha
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,1], 
#   "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,2], 
#   "D")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,4], 
#   30)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,5], 
#   31)
#   
#   # Start of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,5], 
#   6)
#   
#   # End of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[8,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[8,2], 
#   "D")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[8,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[8,4], 
#   11)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[8,5], 
#   14)
#   
#   # Middle of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,3], 
#   "C")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,4], 
#   7)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,5], 
#   8)
#   
#   # # Let's test Alpha & Charlie
#   
#   # Start of Alpha
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,1], 
#   "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,5], 
#   2)
#   
#   # End of Alpha
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,1], 
#   "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,4], 
#   5)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,5], 
#   31)
#   
#   # Start of Charlie
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,1], 
#   "Charlie")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,5], 
#   2)
#   
#   # End of Charlie
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,1], 
#   "Charlie")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,4], 
#   3)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,5], 
#   6)
#   
#   # # Testing Beta & Charlie
#   
#   # Start of Beta
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,5], 
#   2)
#   
#   # Second of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,3], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,4], 
#   3)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,5], 
#   4)
#   
#   # End of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,4], 
#   5)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,5], 
#   14)
#   
#   # Start of Charlie
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,1], 
#   "Charlie")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,5], 
#   2)
#   
#   # End of Charlie
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,1], 
#   "Charlie")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,4], 
#   5)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,5], 
#   6)
#   
#   # # Let's test all 3 at once
#   
#   # Start of Alpha
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,1], 
#   "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[1,5], 
#   2)
#   
#   # End of Alpha
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,1], 
#   "Alpha")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,4], 
#   5)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[2,5], 
#   31)
#   
#   # Start of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[3,5], 
#   6)
#   
#   # End of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,1], 
#   "Beta")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,4], 
#   7)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,5], 
#   14)
#   
#   # Start of Charlie
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,1], 
#   "Charlie")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,2], 
#   "A")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,3], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,4], 
#   1)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[5,5], 
#   2)
#   
#   # End of Beta
#   
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,1], 
#   "Charlie")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[6,2], 
#   "B")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,3], 
#   "E")
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,4], 
#   3)
#   expect_equal(DetReducer(DenseFrame = TestFrame, FishSet = c("Alpha","Beta","Charlie"), SiteCol = "Site", t1Col = "t1", t2Col = "t2", fCol = "Fish", Method = "Similar",
#   )[4,5], 
#   6)
# })











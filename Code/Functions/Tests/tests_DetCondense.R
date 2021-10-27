context("DetCondense.R")

source("DetCondense.R")

test_that("Inputs are checked for format",{
  # Generating testing data
  FDets<-data.frame()
  FDets[1:10,"Receiver"]<-c("A","A","A","A","B","B","C","D","D","D")
  FDets[1:10,"FishID"]<-"A24"
  FDets[1:10,"Time"]<-c(1,2,3,4,5,6,7,8,9,10)
  expect_error(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Stuart", tCol = "Time", IDCol = "FishID")
               , "Location column selected not found in detection data frame")
  expect_error(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Stuart", IDCol = "FishID")
               , "Time column selected not found in detection data frame")
  expect_error(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "Stuart")
               , "Fish ID column selected not found in detection data frame")
  expect_error(DetCondense(DetFrame = FDets, FishID = "A27", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")
               , "FishID selected not found in detection data frame")
  Restore<-FDets
  # FDets[4,"Time"]<-"Stuart"
  # expect_error(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")
  #              , "Times must be numeric")
  # FDets<-Restore
})
test_that("Outputs end up looking right",{
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[1,1],
               "A24")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[1,2],
               "A")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[1,3],
               1)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[1,4],
               4)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[1,5],
               4)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,1],
               "A24")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,2],
               "D")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,3],
               8)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,4],
               10)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,5],
               3)
  FDets[1:10,"Receiver"]<-c("A","A","B","A","B","C","C","C","D","D")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[3,2],
              "A")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[3,3],
               4)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[3,4],
               4)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[3,5],
               1)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,2],
               "B")
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[4,5],
               1)
  expect_equal(DetCondense(DetFrame = FDets, FishID = "A24", LocCol = "Receiver", tCol = "Time", IDCol = "FishID")[6,2],
               "D")
})
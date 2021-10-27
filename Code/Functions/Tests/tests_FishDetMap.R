context("FishDetMap.R")

source("FishDetMap.R")

test_that("Inputs are checked for format",{
  # Generating testing data
  TestDets<-read.csv("~/Documents/Git/Repos/FishTelPrac/Code/Functions/TestingData/SRCTestSub1.csv", stringsAsFactors = F)
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",  LatCol = "Lat", LonCol = "Lon", FishID = "Stuart"),
                         "FishIDs selected not found in release data frame")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",  LatCol = "Stuart", LonCol = "Lon", FishID = "2F54"),
                         "Latitude column selected not found in release data frame")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "Stuart", LatCol = "Lat", LonCol = "Lon", FishID = "2F54"),
                         "Fish ID column selected not found in release data frame")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",  LatCol = "Lat", LonCol = "Stuart", FishID = "2F54"),
                         "Longitude column selected not found in release data frame")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                    pch = "Stuart"),
                         "Invalid pch selected")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                    cex = "Stuart"),
                         "cex must be numeric")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                    cex = -1),
                         "cex must be positive")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                    DecLim = "Stuart"),
                         "DecLim must be numeric")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                    DecLim = -.2),
                         "DecLim must be positive")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                    ColorFactor = "Stuart"),
                         "ColorFactor column selected not found in release data frame")
  testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54",
                                      ColorFactor = "dtf", Palette = "Stuart"),
                           "Palette must include two or more colors")
    Restore<-TestDets[3,]
    TestDets[3,"Lat"]<-"Stuart"
    testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54"),
                           "Latitude must be numeric")
    TestDets[3,]<-Restore
    TestDets[3,"Lon"]<-"Stuart"
    testthat::expect_error(FishDetMap(RelFrame = TestDets, IDCol = "TagID",   LatCol = "Lat", LonCol = "Lon", FishID = "2F54"),
                           "Longitude must be numeric")
    TestDets[3,]<-Restore
    })








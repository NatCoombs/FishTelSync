context("OutMiVel.R")

source("OutMiVel.R")

test_that("Inputs are checked for format",{
  # Generating an initial testing data frame
  TestFrame <- data.frame("Fish" = NA, "Times" = NA, "Rkm" = NA) 
  for(i in 0:11){
    for(j in 0:25){
      TestFrame[1+i*26+j,"Fish"] <- LETTERS[i+1]
      TestFrame[1+i*26+j,"Times"] <- as.numeric(c(1:26)[j+1])
      TestFrame[1+i*26+j,"Rkm"] <- as.numeric(c(100:73)[j+1])
    }
  }
  # Restore is used as a variable for any value(s) being replaced to produce an error or warning
  Restore<-TestFrame[14,]
  TestFrame[14,]<-TestFrame[1,]
  testthat::expect_error(OutMiVel(RelFrame = TestFrame, Fish = "Fish", Times = "Times", Rkm = "Rkm"),"Repeated fish/time pairs")
  TestFrame[14,]<-Restore
  
  TestFrame[14,"Times"]<-"Apples"
  testthat::expect_error(OutMiVel(RelFrame = TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm"),"Times must have numeric equivalent")
  TestFrame[14,]<-Restore
  
  TestFrame[14,"Rkm"]<-"Apples"
  
  testthat::expect_error(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm"),"Rkm must have numeric equivalent")
  TestFrame[14,]<-Restore
  
  TestFrame[14,"Fish"]<-NA
  testthat::expect_error(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm"),"Detections must be associated with a fish ID")
  TestFrame[14,]<-Restore
  testthat::expect_error(OutMiVel(TestFrame,TStep = 2, Fish = "Fish", Times = "Times", Rkm = "Rkm"),
                         "Time step must be odd")
  testthat::expect_error(OutMiVel(TestFrame,TStep = "Apis apis", Fish = "Fish", Times = "Times", Rkm = "Rkm"),
                         "Time step must be numeric")
  testthat::expect_error(OutMiVel(TestFrame,TStep = 3.5, Fish = "Fish", Times = "Times", Rkm = "Rkm"),
                         "Time step must be an integer")
  testthat::expect_error(OutMiVel(TestFrame,TStep = 3, Weight = 1.2, Fish = "Fish", Times = "Times", Rkm = "Rkm"),
                         "Weight must be <= 1")
  testthat::expect_error(OutMiVel(TestFrame,TStep = 3, Weight = "Apis apis", Fish = "Fish", Times = "Times", Rkm = "Rkm"),
                         "Weight must be numeric")
  Restore<-TestFrame
  TestFrame$Vel<-NA
  testthat::expect_error(OutMiVel(TestFrame, Fish = "Fish", Times = "Times", Rkm = "Rkm"), 
                           "Displacement and/or Velocity columns being overwritten")
  TestFrame<-Restore
  TestFrame$Disp<-NA
  testthat::expect_error(OutMiVel(TestFrame, Fish = "Fish", Times = "Times", Rkm = "Rkm"), 
                           "Displacement and/or Velocity columns being overwritten")
})

test_that("Outputs are correctly processed",{
  TestFrame <- data.frame(Fish = NA, Times = NA, Rkm = NA) 
  for(i in 0:11){
    for(j in 0:25){
      TestFrame[1+i*26+j,"Fish"] <- LETTERS[i+1]
      TestFrame[1+i*26+j,"Times"] <- c(1:26)[j+1]
      TestFrame[1+i*26+j,"Rkm"] <- (2 * c(100:73)[j+1])
    }
  }
  # Testing edge cases w/ Step 1
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Disp"],-2)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],-2)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[26,"Disp"],0)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[26,"Vel"],0)
  
  # Let's implement some variation to the displacement and timestep see how that goes
  TestFrame[1:12,"Rkm"]<-c(220,210,208,208,208,200,150,145,140,130,120,110)
  
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Disp"],-10)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],-10)
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],(-12/2))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],(-12/3))
  
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Disp"],0)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],0)
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],(-2/3))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],(-20/5))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],(-1/3))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],(-5.5/5))
  
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[2,"Disp"],-2)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[2,"Vel"],-2)
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Fish = "Fish", Times = "Times", Rkm = "Rkm")[2,"Vel"],(-4))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[2,"Vel"],(-3))
  
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[2,"Vel"],(-7/3))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[2,"Vel"],(-7/4))
  #And testing some simple elements with time variation added in
  TestFrame[1:26,"Times"]<-c(1,3,7,8,10,14,15,18,19,20,22,23:37)
  
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Disp"],-10)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],-5)
  
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],-12/6)
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],-12/7)
  
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],(-11/6))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[1,"Vel"],(-11/7))
  
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Disp"],0)
  testthat::expect_equal(OutMiVel(TestFrame,Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],0)
  
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],-2/7)
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],-20/13)
  
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 3, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],(-1/7))
  testthat::expect_equal(OutMiVel(TestFrame,TStep = 5, Weight = .5, Fish = "Fish", Times = "Times", Rkm = "Rkm")[3,"Vel"],(-5.5/13))
})
  
  
  
  
  
  
  
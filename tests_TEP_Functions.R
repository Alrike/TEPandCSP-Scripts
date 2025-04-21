#!/usr/bin/R
#18.04.2025  Alrike
#Unit-Tests for TEP-Functions.R und TEP-graphics-plugin.R


#requires library:testthat

#tests the functions:
#individual analysis (indan)
#observed area (obsarea)  [per photograph!!!], can now do both microscopes
#size distribution (size_distribution)
#add to masterhist (addto_masterhist)
#calculate carboncontent (calc_Ccontent)
library(testthat)
source("TEP-FUnctions.R")


################ indan ################
test_that("indan, happy path", {
  testdata <- data.frame(Number=1:5, Area=(1:5/2)^2*pi)
  results  <- indan(testdata)
  expect_length(results[2,], 4)
  expect_equal(results$`ESD[um]`, 1:5)
  expect_equal(results$`ESV[um^3]`[3],14.1371669)
  expect_vector(results$Number, 1:5)
})

test_that("indan, invalid arguments", {
  testdata <- data.frame(Number=1:5, area=1:5)
 expect_error(indan(testdata), "Input has no column named Area!")
  
})


################ obsarea ################
test_that("obsarea, happy path", {
  expect_equal(obsarea("Axioscope",400),218*163)
  expect_equal(obsarea("Axiolab",200),715*402)
})

test_that("obsarea, invalid arguments", {
  expect_error(obsarea("Microscope",400), "Unknown Microscope!")
  expect_error(obsarea(50,400), "Unknown Microscope!")
  expect_error(obsarea("Axioscope",630), "Magnification not known!")
  expect_error(obsarea("Axiolab", 1000), "Magnification not known!")
  
})

################ size distribution ################
test_that("size_distribution, happy path", {
  testhist<-hist(c(rep(1,10),rep(2,20),rep(3,15),rep(4,12),rep(5,5), rep(6,0)), 
                 breaks=0:6)
  result<-size_distribution(testhist, "Excel-Mastersheet")
  expect_equal(result$nreg_points, 6)
  expect_equal(round(result$slope,2), -0.99)
  expect_equal(round(result$intercept,2), 1.35)
  
  result<-size_distribution(testhist, "Mari&Kiorboe(1996)")
  expect_equal(result$nreg_points, 3)
  expect_equal(round(result$slope,2), -5,41)
  expect_equal(round(result$intercept,2), 4.09)
  
  result<-size_distribution(testhist, "zerotail_omit")
  expect_equal(result$nreg_points, 3)
  expect_equal(round(result$slope,2), -5,41)
  expect_equal(round(result$intercept,2), 4.09)
  
  testhist<-hist(c(rep(1,10),rep(2,20),rep(3,15),rep(4,12),rep(5,5), rep(6,0)), 
                 breaks=0:10)
  result<-size_distribution(testhist, "zerotail_omit")
  expect_equal(result$nreg_points, 6)
  expect_equal(round(result$slope,2), -2,97)
  expect_equal(round(result$intercept,2), 2.55)
  
  result<-size_distribution(testhist, "AG_Engel_Standard")
  expect_equal(result$nreg_points, 3)
  expect_equal(round(result$slope,2), -0.60)
  expect_equal(round(result$intercept,2), 1.41)
})

test_that("size_distribution, no calculation sensible", {
  testhist<-hist(c(rep(1,10),rep(2,20)))
  expect_warning(size_distribution(testhist, "zerotail_omit"),
                 "Not enough suitable sizeclasses for size distribution")
  expect_warning(size_distribution(testhist, "AG_Engel_Standard"),
                 "Not enough suitable sizeclasses for size distribution")
  expect_warning(size_distribution(testhist, "some_method"))
  
})

test_that("size_distribution, invalid arguments", {
  testhist<-list(content=c(1,1), mids=c(12,3))
  expect_error(size_distribution(testhist, "AG_Engel_Standard"),
                 "function size_distribution requires a histogram with elements mids and count")
  testhist<-list(content=c(1,1), mids=c(12,3))
  expect_error(size_distribution(c(1,2), "Excel-Mastersheet"),
               "input histogram is not a list")
  
})

############add to masterhist (addto_masterhist)############
test_that("addtomasterhist, happy path", {
  starthist<-hist(c(rep(1,10),rep(2,20),rep(3,15),rep(4,12),rep(5,5), rep(6,0)), 
                 breaks=0:10)
  masterhist<-data.frame(mids=starthist$mids, 
                         firstsample=starthist$counts)
  testhist<-hist(c(rep(1,5),rep(2,12),rep(3,20),rep(4,10),rep(5,4), rep(6,2)), 
                 breaks=0:10)
  resulthist<-addto_masterhist("secondsample", testhist, masterhist, FALSE)
  expect_equal(resulthist$secondsample, c(5,12,20,10,4,2,0,0,0,0))
  expect_equal(resulthist$firstsample, c(10,20,15,12,5,0,0,0,0,0))
  expect_equal(resulthist[3,3], 20)
  expect_equal(resulthist[3,2], 15)
})

test_that("addtomasterhist, no input path", {
  starthist<-hist(c(rep(1,10),rep(2,20),rep(3,15),rep(4,12),rep(5,5), rep(6,0)), 
                  breaks=0:10)
  masterhist<-data.frame(mids=starthist$mids, 
                         firstsample=starthist$counts)
  testhist<-hist(c(rep(1,5),rep(2,12),rep(3,20),rep(4,10),rep(5,4), rep(6,2)), 
                 breaks=0:10)
  expect_warning(addto_masterhist("secondsample", testhist, masterhist=masterhist, isEmpty=TRUE),
                 "input-histogram is being ignored with isEmpty=TRUE")
  suppressWarnings(resulthist<-addto_masterhist("secondsample", testhist, masterhist=masterhist, isEmpty=TRUE))
  expect_equal(resulthist$secondsample, rep(0,10))
  expect_equal(resulthist$firstsample, c(10,20,15,12,5,0,0,0,0,0))
  expect_equal(resulthist[3,3], 0)
  expect_equal(resulthist[3,2], 15)
  
  resulthist<-addto_masterhist("secondsample", masterhist=masterhist, isEmpty=TRUE)
  expect_equal(resulthist$secondsample, rep(0,10))
  expect_equal(resulthist$firstsample, c(10,20,15,12,5,0,0,0,0,0))
  expect_equal(resulthist[3,3], 0)
  expect_equal(resulthist[3,2], 15)
})

test_that("addtomasterhist, invalid input", {
  starthist<-hist(c(rep(1,10),rep(2,20),rep(3,15),rep(4,12),rep(5,5), rep(6,0)), 
                  breaks=0:10)
  masterhist<-data.frame(mids=starthist$mids, 
                         firstsample=starthist$counts)
  testhist<-hist(c(rep(1,5),rep(2,12),rep(3,20),rep(4,10),rep(5,4), rep(6,2)), 
                 breaks=0:10)
  expect_error(addto_masterhist("secondsample", testhist, masterhist=masterhist),
               "isEmpty needs to be either TRUE or FALSE")
  testhist<-hist(c(rep(1,5),rep(2,12),rep(3,20),rep(4,10),rep(5,4), rep(6,2)))
  expect_error(addto_masterhist("secondsample", masterhist=masterhist, isEmpty=FALSE),
               "No data to add to histogram")
  expect_error(addto_masterhist("secondsample", testhist, isEmpty=FALSE),
               "No histogram to add data to")
  expect_error(addto_masterhist("secondsample", testhist, masterhist=masterhist, isEmpty = FALSE),
               "histogram doesn't match structure of masterhist")
 
})

########################Calc_Ccontent################
test_that("calc_Ccontent, Happy path", {
  testdata<-data.frame(mids=seq(0.5,9.5, by=1), 
                       firstsample=c(10,20,15,12,5,rep(0,5)),
                       secondsample=c(5,12,20,10,4,2, rep(0,4)),
                       zerosample=rep(0,10))
  testresult<-calc_Ccontent(testdata)
  expect_length(testresult$mids, 11)
  expect_equal(testresult$firstsample[11], "3.14820558704278e-05")
  expect_equal(testresult$secondsample[11], "3.5229090640763e-05")
  expect_equal(testresult$zerosample[11], "0")
})

test_that("calc_Ccontent, invalid arguments", {
  testdata<-12
  expect_error(calc_Ccontent(testdata),
               "input is not a dataframe")
  testdata<-data.frame(firstsample=c(10,20,15,12,5,rep(0,5)),
                       secondsample=c(5,12,20,10,4,2, rep(0,4)),
                       zerosample=rep(0,10))
  expect_error(calc_Ccontent(testdata),
               "input is missing mids column in first position")
  testdata<-data.frame(mids=seq(0.5,9.5, by=1))
  expect_error(calc_Ccontent(testdata),
               "input has no data")
  
})

#!/usr/bin/R
#18.04.2025  Alrike
#End-to-End Tests for Particle-Analysis.R

#for running on your PC, please ensure, that the folder "Testfiles" is in the same place as your script
#also please comment out the following parameters:
#magnification
#microscope
#histvec
#method_sizedis
#filterdir
#tabledir
#particletype
#input

#Unfortunately these tests are not suposed to use this way, so testing needs to be semiautomated:
# make sure the variable scriptloc is in your global environment
# first run all codelines in the block up to the first "expect"
# then scroll back to the test_that line and run this one
# then all expectations will be checked and you'll be informed of pass or fail and issues

library(testthat)
library(readxl)
scriptloc<-"C:/Users/linds/Documents/GitHub/TEPandCSP-Scripts/"


test_that("TEP-Calculation, Happy Path", {
  #setup parameters for all tests
  magnification                   <- 200          
  microscope                      <- "Axioscope"     
  method_sizedis                  <- "AG_Engel_Standard" 
  filterdir<-paste(scriptloc, "Testfiles/", sep="") #if scriptloc does not end on / please add      
  tabledir<-filterdir 
  
  #individual test
  setwd(tabledir)
  input<-read_excel("Test-input.xls", sheet="Testdata", col_names=TRUE)
  particletype                    <- "TEP"
  histvec                         <- seq(0,70, by=0.5) 
  setwd(scriptloc)
  suppressWarnings(source("Particle-Analysis.R"))
  expect_length(results$Filter_ID, 6)
  expect_length(names(results), 158)
  expect_equal(sum(as.integer(results$`0.25`)), 0)
  expect_equal(round(results$maxESD,2), c(9.69, 8.27, 50.75, 5.29, 3.54, 6.32))
  expect_equal(results$picnum, rep(34,6))
  expect_equal(results$Filter_ID, 
               c("A-Sample", "B-Sample", "C-Sample", "A-Blank", "B-Blank", "C-Blank"))
  expect_equal(results$Number_Particles, c(4958, 1481, 2860, 234, 72, 67))
  expect_contains(names(results), c("Carboncontent [ug] (observed)", "Carboncontent [ug/l]"))
})

test_that("CSP-Calculation, Happy Path", {
  #setup parameters for all tests
  magnification                   <- 200          
  microscope                      <- "Axioscope"     
  method_sizedis                  <- "AG_Engel_Standard" 
  filterdir<-paste(scriptloc, "Testfiles/", sep="") #if scriptloc does not end on / please add      
  tabledir<-filterdir 
  
  #individual test
  setwd(tabledir)
  input<-read_excel("Test-input.xls", sheet="Testdata", col_names=TRUE)
  histvec                         <- seq(0,70, by=0.5) 
  particletype                    <- "CSP"
  setwd(scriptloc)
  suppressWarnings(source("Particle-Analysis.R"))
  expect_length(results$Filter_ID, 6)
  expect_length(names(results), 156)
  expect_equal(sum(as.integer(results$`0.25`)), 0)
  expect_equal(round(results$maxESD,2), c(9.69, 8.27, 50.75, 5.29, 3.54, 6.32))
  expect_equal(results$picnum, rep(34,6))
  expect_equal(results$Filter_ID, 
               c("A-Sample", "B-Sample", "C-Sample", "A-Blank", "B-Blank", "C-Blank"))
  expect_equal(results$Number_Particles, c(4958, 1481, 2860, 234, 72, 67))
  expect(!all(c("Carboncontent [ug] (observed)", "Carboncontent [ug/l]") %in% names(results)),
         "Carbon-Content has been calculated for CSP")
})

test_that("CSP with zero filter and different histvec", {
  #setup parameters for all tests
  magnification                   <- 200          
  microscope                      <- "Axioscope"     
  method_sizedis                  <- "AG_Engel_Standard" 
  filterdir<-paste(scriptloc, "Testfiles/", sep="") #if scriptloc does not end on / please add      
  tabledir<-filterdir 
  
  #individual test
  setwd(tabledir)
  input<-read_excel("Test-input.xls", sheet="Testdata_blank", col_names=TRUE)
  particletype                    <- "CSP"
  histvec                         <- seq(0,20, by=0.5)
  setwd(scriptloc)
  source("Particle-Analysis.R")
  expect_length(results$Filter_ID, 2)
  expect_equal(results$Number.l[2], 0)
  expect_length(names(results),56)
  expect_output(source("Particle-Analysis.R"), 
                "1 \\t A-Sample \\n2 \\t Empty_filter \\nNo particles on filter?")
  
})


test_that("TEP, unexpected file-ending", {
  #setup parameters for all tests
  magnification                   <- 200          
  microscope                      <- "Axioscope"     
  method_sizedis                  <- "AG_Engel_Standard" 
  filterdir<-paste(scriptloc, "Testfiles/", sep="") #if scriptloc does not end on / please add      
  tabledir<-filterdir 
  
  #individual test
  setwd(tabledir)
  input<-read_excel("Test-input.xls", sheet="Testdata_wrongending", col_names=TRUE)
  particletype                    <- "TEP"
  histvec                         <- seq(0,20, by=0.5)
  setwd(scriptloc)
  expect_error(source("Particle-Analysis.R"), "Format not known!! Please teach me how to read this one!")
  
})

test_that("CSP, non-unique sample names", {
  #setup parameters for all tests
  magnification                   <- 200          
  microscope                      <- "Axioscope"     
  method_sizedis                  <- "AG_Engel_Standard" 
  filterdir<-paste(scriptloc, "Testfiles/", sep="") #if scriptloc does not end on / please add      
  tabledir<-filterdir 
  
  #individual test
  setwd(tabledir)
  input<-read_excel("Test-input.xls", sheet="Testdata_doublename", col_names=TRUE)
  particletype                    <- "CSP"
  histvec                         <- seq(0,20, by=0.5)
  setwd(scriptloc)
  expect_error(source("Particle-Analysis.R"), "Sample names are not unique! Please ensure no name occurs twice")
  
})

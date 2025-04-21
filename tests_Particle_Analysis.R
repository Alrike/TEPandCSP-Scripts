#!/usr/bin/R
#18.04.2025  Alrike
#End-to-End Tests for Particle-Analysis.R

#for running on your PC, please ensure, that the folder "Testfiles" is in the same place as your script
#also please set the following parameters:
#

#requires library:testthat
library(testthat)

magnification                   <- 200          
microscope                      <- "Axioscope"     
histvec                         <- seq(0,70, by=0.5) 
method_sizedis                  <- "AG_Engel_Standard" 
particletype                    <- "TEP"        
filterdir<-paste(scriptloc, "Testfiles/", sep="") #if scriptloc does not end on / please add      
tabledir<-filterdir 

input<-read_excel("Test-input.xls", sheet="Testdata", col_names=TRUE)
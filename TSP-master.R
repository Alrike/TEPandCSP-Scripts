#/usr/bin/R
#09.12.2022, Alrike
#analysing combistain samples

library(readxl)
tabledir<-"C:/Users/lscheidemann/Desktop/Labplas-Elbe2023/Alkor586/" 
setwd(tabledir)
input_all<-read_excel("Alkor586-input.xlsx", col_names=TRUE)
scriptloc<-"C:/Users/lscheidemann/Desktop/Dokumrnte/imageJ-stuff"
setwd(scriptloc)
#three calls to the script
print("TEP")
filterdir<-"C:/Users/lscheidemann/Desktop/Labplas-Elbe2023/Alkor586/" 
input<-input_all
names(input)[names(input) == 'TEPfile'] <- 'filename'
filename    <-"test-TEP.csv"
setwd(scriptloc)
source("Particle-Analysis-TSP.R")
TEP        <-results

print("CSP")
filterdir<-"C:/Users/lscheidemann/Desktop/Labplas-Elbe2023/Alkor586/" 
input<-input_all
names(input)[names(input) == 'CSPfile'] <- 'filename'
filename    <-"test-CSP.csv"
setwd(scriptloc)
source("Particle-Analysis-TSP.R")
CSP        <-results

print("TSP")
filterdir<-"C:/Users/lscheidemann/Desktop/Labplas-Elbe2023/Alkor586/"
input<-input_all
names(input)[names(input) == 'TSPfile'] <- 'filename'
filename    <-"test.csv"
setwd(scriptloc)
source("Particle-Analysis-TSP.R")
TSP        <-results

setwd(filterdir)
#####################################special part#########################
TSP_results<-data.frame(filter_ID=input_all$filter_ID, 
                        check1=rep(NA, length(input_all$filter_ID)),
                        check2=length(input_all$filter_ID),
                        TEPinCSP=length(input_all$filter_ID),
                        CSPinTEP=length(input_all$filter_ID))
for(n in 1:length(input_all$filter_ID)){
  particles_all<-read.csv(file=input_all$TSPfile[n], header=TRUE)
  particles_CSP<-read.csv(file=input_all$CSPfile[n], header=TRUE)
  particles_TEP<-read.csv(file=input_all$TEPfile[n], header=TRUE)
  #quality check
  check<-sum(particles_TEP$Area)+sum(particles_CSP$Area)-sum(particles_all$Area) #pretty good (0.01%)
  
  #calculate ESD
  particles_all<-indan(particles_all)
  #prepare for sorting
  particles_CSP$particle.total<-rep(NA, length(particles_CSP$Area))
  particles_TEP$particle.total<-rep(NA, length(particles_TEP$Area))
  particles_all$CSP.Area<-rep(NA, length(particles_all$Area))
  particles_all$TEP.Area<-rep(NA, length(particles_all$Area))
  
  #loop over total particles (large aggregates from combined image)
  for(i in 1:length(particles_all$Area)){
    if(i %% 1000 == 0){
      print(i)
    }
    
    #X-coordinates
    CSP.X<-particles_CSP$X < particles_all$X[i] + particles_all$`ESD[um]`[i] &
      particles_CSP$X > particles_all$X[i] - particles_all$`ESD[um]`[i]
    TEP.X<-particles_TEP$X < particles_all$X[i] + particles_all$`ESD[um]`[i] &
      particles_TEP$X > particles_all$X[i] - particles_all$`ESD[um]`[i]
    
    #y-coordinates
    CSP.Y<-particles_CSP$Y < particles_all$Y[i] + particles_all$`ESD[um]`[i] &
      particles_CSP$Y > particles_all$Y[i] - particles_all$`ESD[um]`[i]
    TEP.Y<-particles_TEP$Y < particles_all$X[i] + particles_all$`ESD[um]`[i] &
      particles_TEP$Y > particles_all$Y[i] - particles_all$`ESD[um]`[i]
    
    #stack position
    CSP.Z<-particles_CSP$Slice==particles_all$Slice[i]
    TEP.Z<-particles_TEP$Slice==particles_all$Slice[i]
    
    #particles that overlap in X, Y, and Z dimension (step could be jumped)
    particles_CSP$particle.total[which(CSP.X&CSP.Y&CSP.Z)]=i
    particles_TEP$particle.total[which(TEP.X&TEP.Y&TEP.Z)]=i
    
    #sum particles and place in "master-dataframe"
    particles_all$CSP.Area[i]<-sum(particles_CSP$Area[which(particles_CSP$particle.total==i)])
    particles_all$TEP.Area[i]<-sum(particles_TEP$Area[which(particles_TEP$particle.total==i)])
  }
  
  #calculate percent TEP and CSP and quality check
  particles_all$sum<-particles_all$CSP.Area+particles_all$TEP.Area
  particles_all$percent.TEP <- (particles_all$TEP.Area/particles_all$sum)*100
  particles_all$percent.CSP <- (particles_all$CSP.Area/particles_all$sum)*100
  particles_all$check<-particles_all$sum-particles_all$Area
  
  #calculate the amount of TEP bound with CSP and CSP bound with TEP
  percentTEPinCSP<-
    (sum(particles_all$TEP.Area[which(!particles_all$percent.TEP %in% c(0, 100, NaN))])/
       sum(particles_all$TEP.Area))*100
  print(sum(particles_all$TEP.Area))
  
  percentCSPinTEP<-
    (sum(particles_all$CSP.Area[which(!particles_all$percent.CSP %in% c(0, 100, NaN))])/
       sum(particles_all$CSP.Area))*100
  
  ##add numbers to df
  TSP_results$check1[n]<-check
  TSP_results$check2[n]<-sum(particles_all$sum)-sum(particles_all$Area)
  TSP_results$CSPinTEP[n]<-percentCSPinTEP
  TSP_results$TEPinCSP[n]<-percentTEPinCSP
  
  
}
setwd(scriptloc)
write.csv(TSP_results, "Elbe2023.csv")

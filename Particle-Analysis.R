#!/usr/bin/R
#09.08.2018  Lindsay Scheidemann, slightly optimized 25.10, further optimised to be more user friendly 1.12.
#adjusted to allow for empty blanks 25.02.2019, automated format recognition 04.03.2019
#TEP-carbon content, more flexible histogram-data and optimization for increased user-friendliness 07-11.08.2019,
#changes in size_distribution methods until 23.08., 12.11.2019, 21.07.2020 minor bug fixes ,15.08.2020 add stats to output,
#26.10.2023 adding the new microscope

#reading the TEP- and CSP-analysis output from imageJ and analysing the data, may be used for other particles as well

#getting started
#required: 
#library: readxl
#additional scripts: TEP-Functions.R (functions: indan, obsarea, size_distribution, addto_masterhist, Calc_Ccontent, savexls)
#formula for calculation from Excel-mastersheet, Carolina, Mari (1999)
#thanks for debugging help and formula checking: Carolina(v0.3), Makcim

#reading all the necessary information, preparing for work/script setup
scriptloc<-"C:/Users/lscheidemann/Desktop/Dokumrnte/imageJ-stuff"   #script location
setwd(scriptloc)
source("TEP-Functions.R")                        #script defining some extra-functions necessary to run
                                                 #this contains the functions: individual analysis (indan)
                                                 #observed area (obsarea)  [per photograph!!!] size distribution (size_distribution) 
                                                 #add to masterhist (addto_masterhist), save_xls, calculate carboncontent (calc_Ccontent) [TEP_only!!!]
library(readxl)                                  #for reading the input-Excel
plugin_use <- FALSE
if(plugin_use){source("TEP_graphics_plugin.R")}
#####################################first questions##########################
#information about data and prior analysis
magnification                   <- 200          # magnification used to take the pictures
microscope                      <- "Axioscope"    # either Axioscope (Zeiss-old) or Axiolab (Zeiss-new) 
histvec                         <- seq(0,70, by=0.5) # vector defining the borders of the histogram bins, standard: seq(0,50, by=0.5)
method_sizedis                  <- "AG_Engel_Standard" #either "Excel-Mastersheet" or "Mari&Kiorboe(1996)" or "zerotail_omit" or "AG_Engel_Standard"
                                                #for information on methodological differences see pdf (or the function size_distribution)

###############################script input data###############################
#preparing to read and find all the necessary information 
#about the filters for analysis
particletype                    <- "TEP"        #"TEP", "CSP", or "OTHER", so far only specialised output for TEP,
                                                # feel free to add other types and/or analyses
filterdir<-"C:/Users/lscheidemann/Desktop/Labplas-Elbe2023/Alkor586/CSPs-586/"       #place where the "filters" are stored/directory with imageJ results
tabledir<-"C:/Users/lscheidemann/Desktop/Labplas-Elbe2023/Alkor586/"              #directory with input-table
setwd(tabledir)
#input<-read_excel("Input_TEP_2.xlsx", col_names=TRUE, n_max=12) #files listed in csv, tsv or "fake"-xls
input<-read_excel("Alkor586-input.xlsx", sheet="CSPs", col_names=TRUE)
##################################script output questions###########################
#preparing for output of information into a file
plotresults              <-TRUE                     #plots boxplots for area/l and particles/l and slope, 
                                                 #if it is TRUE, only useful for small number of treatments
saveresults              <-TRUE                    #saves results in a tsv formatted excel, if it is TRUE
decimal_seperator        <-","                   # depending on your Excel 2,1 (",") might be more useful than 2.1("."), or the other way round
filename                 <-"TSP-tests-CSP.xls"        #output filename
savedir                  <-"/Users/lscheidemann/Desktop/TSP-tests3-5/"  #directory to save output in

####################################last intern preparations###########################
#calculating/requesting input-consequences
magniffactor<-obsarea(microscope, magnification)
filters<-input$filter_ID

#making the script more robust with regard to input format (no problem with treatments 1-3 etc)
input$filename      <-   as.character(input$filename)
input$treatment     <-   as.character(input$treatment)

######################################### Analysis #########################################################
#writing the overall dataframe
results<-data.frame(Filter_ID=filters, 
                    "Number_Particles"= rep(NA, length(filters)), "Volume"= rep(NA, length(filters)),
                    "picnum"= rep(NA, length(filters)), "Number/l"= rep(NA, length(filters)),
                    "Area"= rep(NA, length(filters)) ,"Area/l"= rep(NA, length(filters)), 
                    "treatment"= rep(NA, length(filters)), "slope"=rep(NA, length(filters)),
                    "intercept"=rep(NA, length(filters)), "nreg_points"=rep(NA, length(filters)),
                    "pvalueslope"=rep(NA, length(filters)),"r2_slope"=rep(NA, length(filters)),
                    "adj_r2"=rep(NA, length(filters)),"minESD"= rep(NA, length(filters)),
                     "maxESD"= rep(NA, length(filters)))

#getting to the filters
setwd(filterdir)
count=1
for(i in filters)      #for loop across the filters
{
  #reading the information from the input-file
  cat(count, "\t", i, "\n")

  V              = input$`volume[ml]`[input$filter_ID==i]
  picnum         = as.integer(input$`images[nr]`[input$filter_ID==i])
  treatment      = input$treatment[input$filter_ID==i]
  
if (is.na(input$filename[input$filter_ID==i])) #set particles to 0 manually, if no file, good blanks(!!)
  {
  print("No particles on filter?")     #reminder, in case the user forgot to enter a filename
  #set observed parameters to 0
    results$Number_Particles[results$Filter_ID==i]   <- 0
    results$Area[results$Filter_ID==i]               <- 0
    results$minESD[results$Filter_ID==i]             <- 0
    results$maxESD[results$Filter_ID==i]             <- 0
    
    #create the entry for the histogram-table

    masterhist<-addto_masterhist(filter_ID=i, hists=NA,
                                 masterhist=masterhist,
                                 isEmpty=TRUE)
    
    #set the remaining calculations, that will cause trouble, NA
    results$slope[results$Filter_ID==i]              <- NA
    results$intercept[results$Filter_ID==i]          <- NA
    results$nreg_points[results$Filter_ID==i]        <- NA
    results$pvalueslope[results$Filter_ID==i]        <- NA
    results$r2_slope[results$Filter_ID==i]           <- NA
    results$adj_r2[results$Filter_ID==i]             <- NA
    

  }  else  #all other filters, read the observed parameters from imageJ-output
  {
    #extract fileending, to recognize the format of the output-file automatically
    lengthnum<-nchar(input$filename[input$filter_ID==i])
    fileending<-substr(input$filename[input$filter_ID==i], lengthnum-2, lengthnum)
    
    #reading the file based on the ending (or give a more informative output)
    if(!fileending %in% c("csv", "tsv", "xls"))
    {
      print("Format not known!! Please teach me how to read this one!")
      }
    if(fileending=="csv")
    {
      infos<-read.table(input$filename[input$filter_ID==i], header = TRUE, sep=",")
      }   else
      {
        infos<-read.table(input$filename[input$filter_ID==i], header = TRUE, sep="\t")
      }
    #get the first information from the files
    data<-indan(infos)
    data<-subset(data, data$Area>0.2)
    
    ###############calculating the results
    results$Number_Particles[results$Filter_ID==i]  <- length(data$Area)
    results$Area[results$Filter_ID==i]              <- sum(data$Area)
    results$minESD[results$Filter_ID==i]            <- min(data$`ESD[um]`)
    results$maxESD[results$Filter_ID==i]            <- max(data$`ESD[um]`)
    
    ########################### creating the histogram overview ######################
    data   <- subset(data, data$`ESD[um]`<= max(histvec))     # in case ESDmax>max(histvec) the script would stop otherwise
    hists  <- hist(data$`ESD[um]`, breaks = histvec)
   
    #saving the histogram-output to a table (and initialize on the first filter)
    #this allows more flexibility in terms of the span of the histogram
    if(count==1)
    {
      assign(paste(as.name(i)),hists$counts)
      masterhist<-data.frame(mids=hists$mids, 
                             eval(as.name(i)))
      names(masterhist)<-c("mids", paste(i))
    } else
    {
      masterhist<-addto_masterhist(filter_ID=i, hists=hists,
                                   masterhist=masterhist,
                                   isEmpty=FALSE)
    }

    ############################ size distribution ##################
    sizedis<-size_distribution(hists, method_sizedis, i)
    results$slope[results$Filter_ID==i]<-sizedis$slope
    results$intercept[results$Filter_ID==i]<-sizedis$intercept
    results$nreg_points[results$Filter_ID==i]<-sizedis$nreg_points
    results$pvalueslope[results$Filter_ID==i]<- sizedis$pvalue
    results$r2_slope[results$Filter_ID==i]  <- sizedis$rsquared
    results$adj_r2[results$Filter_ID==i] <-sizedis$adj_rsquared
  }  #close specific analysis for non-empty filters  
  
#############################calculating the results ####################
    results$Volume[results$Filter_ID==i]<-V
    results$picnum[results$Filter_ID==i]<-picnum
    results$Number.l[results$Filter_ID==i]<-((pi/4)*(23000^2)*results$Number_Particles[results$Filter_ID==i])/
      (picnum*magniffactor*(V/1000))*10^(-6) #conversion for having more beautiful numbers
    results$Area.l[results$Filter_ID==i]<-((pi/4)*(23000^2)*results$Area[results$Filter_ID==i])/
      (picnum*magniffactor*(V/1000))*10^(-8) #conversion um^2 to cm^2
    results$treatment[results$Filter_ID==i]<-input$treatment[input$filter_ID==i]

  #delete "old memories", just to be sure
  #rm(infos); rm(data); rm(V); rm(picnum); rm(treatment)
  #rm(hists); rm(sizedis)
    
    count=count+1
}                  #end loop "filters"
################################Particletype-specific Analyses########
if (particletype=="TEP")
{
  masterhist<-calc_Ccontent(masterhist = masterhist)
  Ccontent_obs<-as.numeric(masterhist[length(histvec), 2:ncol(masterhist)])
  picvec<-results$picnum
  Volume_ml_vec<-results$Volume
  Ccontent_per_l<- ((pi/4)*(23000^2)*Ccontent_obs)/
    (picvec*magniffactor*(Volume_ml_vec/1000))
  masterhist<-rbind(masterhist, Ccontent=c("Carboncontent [ug/l]", Ccontent_per_l))
}
testdf<-masterhist #for debugging
#########################combining all results#########################
#take the dataframe with the histogram results and add them to the results
masterhist<-t(masterhist)                                            #transpone masterhist, so the orientations match
colnames(masterhist)<-masterhist[1,]                                 #make the mid-counts the column names (so these are meaningful)
masterhist=masterhist[!row.names(masterhist) %in% c("mids"),]        #remove the mid-counts row, since it is no longer necessary and will interfere with the merge function
masterhist<-cbind(Filter_ID=rownames(masterhist), masterhist)        #create a column with the filternames (which are now rownames), because merge won't work otherwise
results<-merge(results, masterhist, by="Filter_ID")                  #finally merge the tables by the Filter_IDs, the positions should be the same in both tables, but I still prefer merge to cbind, 
                                                                     #because it will correct and adjust, if they don't, while cbind doesn't even give a warning
if(particletype == "TEP") {
  results$`Carboncontent [ug/l]`<-Ccontent_per_l
}

rm(count)
##############################save results ##########
if (saveresults)
{
  setwd(savedir)
  save_xls(results=results, filename=filename,  
            decimal_seperator=decimal_seperator)
  print("Thanks for your patience! Your Excel is now available")
}

setwd(scriptloc)                             #set working directory back to location

####################################### plot results ##############
if (plotresults)        #create simple boxplots for a first overview, only useful, if low nr of treatments
{
  par(mfrow=c(2,2))
  boxplot(results$Area.l~results$treatment, 
          main= paste(particletype,"Concentration"), ylab="Area [cm^2/l]", xlab="Treatment")
  boxplot(results$Number.l~results$treatment,
        ylab=paste(particletype,"abundance (10^6 Nr/l)"), main=paste(particletype,"Abundance"), xlab="Treatment")
  if(length(which(!is.na(results$slope)))>0.4*length(results$slope)) #more than 40% of filters have slope data
    {boxplot(results$slope~results$treatment,
          main="slope of size distribution", ylab="slope", xlab="Treatment")
  }
  dotchart(results$Area.l, groups=factor(results$treatment), 
           xlab="Area [cm^2/l]", ylab="Treatment", main="Data Points")
  par(mfrow=c(1,1))
}

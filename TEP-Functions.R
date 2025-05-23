#!/usr/bin/R
#19.08.2018  Lindsay Scheidemann
#functions for analysing the TEP-data
#for use in Particle-Analysis.R (Version 2.0.0) or independently

#contains the functions:
#individual analysis (indan)
#observed area (obsarea)  [per photograph!!!], can now do both microscopes
#size distribution (size_distribution) 
#add to masterhist (addto_masterhist)
#calculate carboncontent (calc_Ccontent)
#save Excel(output)-file (savexls) 

indan<-function(TEP_Data)  #19.08.2018
{
  if(!"Area" %in% names(TEP_Data)) {stop("Input has no column named Area!")}
  TEP_Data$`ESD[um]`<-sqrt(TEP_Data$Area/pi)*2
  TEP_Data$`ESV[um^3]`<-(4/3)*pi*((TEP_Data$`ESD[um]`/2)^3)
  return(TEP_Data)
}

obsarea<-function(microscope, magnification)   #25.10.2018
{
  if(!microscope %in%c("Axioscope", "Axiolab")) {stop("Unknown Microscope!")} #26.10.23
  if(microscope == "Axioscope")
     {
        if (!(magnification %in% c(100, 200, 400, 1000))) {stop("Magnification not known!")}
        if (magnification==100){magniffactor=865*648}  #image in um
        if (magnification==200){magniffactor=436*327}
        if (magnification==400){magniffactor=218*163}
        if (magnification==1000){magniffactor=87.2*65.3}         #14.8.19
       }
     if (microscope == "Axiolab") #26.10.23
       {
       if (!magnification %in% c(100, 200, 400, 630)) {stop("Magnification not known!")}
       if (magnification==100){magniffactor=1421*799}
       if (magnification==200){magniffactor=715*402}
       if (magnification==400){magniffactor=355*200}
       if (magnification==630){magniffactor=226*127} 
       }
  return(magniffactor)
}

size_distribution<-function(hists, method_sizedis, name)  #15.8.19
{
  if(!is.list(hists)){stop("input histogram is not a list")}
  if(!("counts"%in%names(hists)&"mids"%in%names(hists))){stop("function size_distribution requires a histogram with elements mids and counts")}
  histdata<-data.frame(mids=hists$mids, counts=hists$counts)
  if (!method_sizedis %in% c("Excel-Mastersheet", "Mari&Kiorboe(1996)", "zerotail_omit", "AG_Engel_Standard"))
  {
    method_sizedis="AG_Engel_Standard"
    warning("Method not known!!, Check spelling, use a different one or add to the function, please",
        "\n", "Size distribution parameters are calculated by method: 'AG_Engel_Standard'")
  }
  if (method_sizedis=="Excel-Mastersheet")
  {
    histdata<-subset(histdata, histdata$mids<=33) #data are cut at 33um ESD
      for (n in 1:length(histdata$mids))
      {
        if(histdata$counts[n]==0)           #exclusion of 0 counts for log 10
        {histdata$counts[n]=1}              #they are manually set to one (!!equal to single counts)
      }
      halfbin<-(histdata$mids[2]-histdata$mids[1])*0.5               #calculate the the way from classmid to upper border
      intdata<-data.frame(countslog=log10(histdata$counts), 
                          classmidslog=log10(histdata$mids+halfbin)) #upper borders are used
  }
  
  if (method_sizedis=="Mari&Kiorboe(1996)")
  {
    histdata<-subset(x=histdata, subset=histdata$mids > 3)        # may be underrepresented due to flexible character
                                                                  # and are difficult to enumerate at 250x
    histdata<-subset(x=histdata, subset=histdata$mids < 40)       # bad counting statistics
    intdata<-data.frame(countslog=log10(histdata$counts + 0.001), # because log10(0)=>ERROR!!!
                        classmidslog=log10(histdata$mids))
     for (n in 1:length(intdata$classmidslog))
      {
        if(intdata$countslog[n]<0)    #if and only if counts were 0=> log10(0.001)=-3
        {intdata$countslog[n]=0}      #if counts have been one=>log10(1.001)= 0.0004
                                      #so the loop assign the value 0 back to 0 counts
      }
  }
  if (method_sizedis=="zerotail_omit")
  {
    histdata<-subset(x=histdata, subset=histdata$mids > 3) #I trust reasoning and experience of the paper authors
    zerocount<-0
    rowcount<-1
    while(zerocount<3 & rowcount<=length(histdata$mids))              # this spots the point in the data, 
    {                               # where the last three counts in a row were 0
      #cat(rowcount, "\t", zerocount, "\n") #to check wether this works, debugging only
      if(histdata$counts[rowcount]==0)
      {zerocount=zerocount+1}else
      {zerocount=0}
      rowcount=rowcount+1
      if(rowcount==length(histdata$mids)){break}
    }
    histdata<-histdata[1:(rowcount),] #the three zeros in a row are included in the data (!!)
    intdata<-data.frame(countslog=log10(histdata$counts + 0.001), # because log10(0)=>ERROR!!!
                        classmidslog=log10(histdata$mids))        #actually useless, because the prevous loop takes at least the three zeros
    if (length(intdata$countslog)<3)                              #regression with less than three data points would be random
    {
      warning("Not enough suitable sizeclasses for size distribution for filter: ", name)
      slope        <-NA                                           #so NAs are assigned (similar to empty blanks)
      intercept    <-NA
      nreg_points  <-length(intdata$countslog)
      pvalue       <-NA
      rsquared     <-NA
      adj_rsquared <-NA
    } else
    {
      for (n in 1:length(intdata$classmidslog))
      {
        if(intdata$countslog[n]<0)    #if and only if counts were 0=> log10(0.001)=-3
        {intdata$countslog[n]=0}      #if counts have been one=>log10(1.001)= 0.0004
                                      #so the loop assign the value 0 back to 0 counts
      }
      begin<-which.max((intdata$countslog))
      intdata<-intdata[begin:length(intdata$countslog),]
    }
  }
  
  if (method_sizedis=="AG_Engel_Standard")
  {
    histdata<-subset(histdata, histdata$counts>10) #anything with less than ten counts is excluded (bad stats)
    intdata<-data.frame(countslog=log10(histdata$counts), 
                        classmidslog=log10(histdata$mids)) 
    if (length(intdata$countslog)<3)                                #regression with less than three data points would be random
    {
      warning("Not enough suitable sizeclasses for size distribution for filter: ", name)
      slope        <-NA                                             #so NAs are assigned (similar to empty blanks)
      intercept    <-NA
      nreg_points  <-length(intdata$countslog)
      pvalue       <-NA
      rsquared     <-NA
      adj_rsquared <-NA
    }
  }

  if(!exists("slope"))                #only calculate the regression, if it makes sense 
  {
    #regression and reading
    regr<-lm(intdata$countslog~intdata$classmidslog)
    slope<-unname(regr$coefficients[2])
    intercept<-unname(regr$coefficients[1])
    nreg_points<-length(intdata$countslog)
    regr_details<-summary(regr)
    pvalue<-regr_details$coefficients[2,4]
    rsquared<-regr_details$r.squared
    adj_rsquared<-regr_details$adj.r.squared
  }
  return(list(slope=slope, intercept=intercept, nreg_points=nreg_points,
              pvalue=pvalue, rsquared=rsquared, adj_rsquared=adj_rsquared))
}

addto_masterhist<-function(filter_ID, hists=NA, masterhist=NA, isEmpty=NA) #10.08.2019
{
  if(is.na(isEmpty)){stop("isEmpty needs to be either TRUE or FALSE")}
  if (isEmpty)
  {
    if(is.list(hists)){warning("input-histogram is being ignored with isEmpty=TRUE")}
    assign(paste(as.name(filter_ID)),rep(0, length(masterhist$mids)))
    histdata<-data.frame(mids=masterhist$mids, eval(as.name(filter_ID)))
    names(histdata)<-c("mids", paste(filter_ID))
    masterhist<-merge(masterhist, histdata, by="mids")
  }else
  {
    if(!is.list(hists)){stop("No data to add to histogram")}
    if(!is.list(masterhist)){stop("No histogram to add data to")}
    if(!all(hists$mids==masterhist$mids)){stop("histogram doesn't match structure of masterhist")}
    
    assign(paste(as.name(filter_ID)),hists$counts)
    histdata<-data.frame(mids=hists$mids, eval(as.name(filter_ID)))
    names(histdata)<-c("mids", paste(filter_ID))
    masterhist<-merge(masterhist, histdata, by="mids")
  }
  return(masterhist)
}

calc_Ccontent<-function(masterhist) #10.08.2019, formula: Mari (1999), title see header
{
  if(!is.data.frame(masterhist)){stop("input is not a dataframe")}
  if(!names(masterhist)[1]=="mids"){stop("input is missing mids column in first position")}
  if(length(names(masterhist))==1){stop("input has no data")}
     
  carboncontentvec=c("Carboncontent [ug] (observed)", rep(NA, ncol(masterhist)-1))
  for (k in 2:ncol(masterhist)) #first column is mids => no carboncontent of mids!
  {
    carboncontent<-0
    for (n in 1:length(masterhist$mids))
    {
      carboncontent<-carboncontent+    #based on formula by Mari (1999)
        0.25*10^(-6)*                  #some constant
        (masterhist$mids[n]/2)^2.55*   #(diameter to radius) multiplied with fractal dimension
        masterhist[n,k]                #multiplied with frequency
    }
    carboncontentvec[k]<-carboncontent
  }
  masterhist<-rbind(masterhist, Ccontent=carboncontentvec) #can't say why it needs a name, 
                                                           #that isn't used, spooky codeline!!!
  return(masterhist)
}

#current version does not need decimal separator, left in to allow for downwards compatability
save_xls<-function(results, filename, decimal_seperator) #no tests
{
  library(openxlsx2)
  if("Area.l" %in%names(results)){
    #reset the names to be more informative/beautiful, 
    #but less practical to work with, therefore only done for saving the file 
    #to work with other programs and human inspection
    names(results)[names(results) == "Number_Particles"] <- "Number of Particles (observed)"
    names(results)[names(results) == "Volume"]           <- "filtered Volume [ml]"
    names(results)[names(results) == "picnum"]           <- "Number of Pictures taken [per filter]"
    names(results)[names(results) == "Number.l"]         <- "Number of particles [*10^6/Liter]"
    names(results)[names(results) == "Area"]             <- "Area of Particles (observed)"
    names(results)[names(results) == "Area.l"]           <- "Area of Particles [cm^2/Liter]"
    names(results)[names(results) == "nreg_points"]      <- "Number of count data points included into the regression for size distribution"
    names(results)[names(results) == "pvalueslope"]      <- "p-value (linear regression)"
    names(results)[names(results) == "adj_r2"]           <- "adjusted r^2 (linear regression)"
    names(results)[names(results) == "r2_slope"]         <- "r^2 (linear regression)"  
  }
  
  #saving file as true-Excel
  write_xlsx(results, filename, asTable = TRUE)
  
  #old code, in case the result is not supposed to be an Excel
  #saving the file
  # write.table(results, file=filename, 
  #             sep="\t", dec=decimal_seperator,
  #             row.names = F, col.names = T)
}

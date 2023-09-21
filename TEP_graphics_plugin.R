#!/usr/bin/R
#21.12.2020  Lindsay Scheidemann
#functions for analysing the TEP-data,
#more visual versions
#for use in Analysis.R

#contains the functions:
#size distribution (size_distribution) #extended version with graphs

size_distribution<-function(hists, method_sizedis, filter_ID)  #15.8.19
{
  histdata<-data.frame(mids=hists$mids, counts=hists$counts)
  
  histdataint<-subset(histdata, histdata$counts>0)    
  intdataplot<-data.frame(countslog=log10(histdataint$counts), 
                          classmidslog=log10(histdataint$mids))
  
  if (!method_sizedis %in% c("Excel-Mastersheet", "Mari&Kiorboe(1996)", "zerotail_omit", "AG_Engel_Standard"))
  {
    method_sizedis="AG_Engel_Standard"
    cat("Method not known!!, Check spelling, use a different one or add to the function, please",
        "\n", "Size distribution parameters are calculated by method: 'AG_Engel_Standard'", "\n")
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
    histdata<-subset(x=histdata, subset=histdata$mids > 3)      # may be underrepresented due to flexible character
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
    while(zerocount<3)              # this spots the point in the data, 
    {                               # where the last three counts in a row were 0
      #cat(rowcount, "\t", zerocount, "\n") #to check wether this works, debugging only
      if(histdata$counts[rowcount]==0)
      {zerocount=zerocount+1}else
      {zerocount=0}
      rowcount=rowcount+1
    }
    histdata<-histdata[1:(rowcount),] #the three zeros in a row are included in the data (!!)
    intdata<-data.frame(countslog=log10(histdata$counts + 0.001), # because log10(0)=>ERROR!!!
                        classmidslog=log10(histdata$mids))        #actually useless, because the prevous loop takes at least the three zeros
    if (length(intdata$countslog)<3)                              #regression with less than three data points would be random
    {
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
  
  plot(intdataplot$countslog~intdataplot$classmidslog,
    pch=4, main=filter_ID, xlab="Particle size (ESD in um)",
     ylab="Particle counts",
    xaxt="none", yaxt="none") #plot transformed data in coordinate system
                              # without axis marks
  points(intdata$countslog~intdata$classmidslog, pch=20, col="red")
  axis(1, at=c(0, 0.3, 0.7,1, 1.7, 2), #add the tickmarks and labels manually
       labels=c("1", "2", "5", "10", "50", "100"))
  axis(2, at=c(0,0.3,1,2, 2.3, 2.7, 3 ), 
       labels=c("1", "2", "10", "100", "200", "500", "1000"))
  if(!is.na(slope)){ #add regression line
   abline(a=intercept, b=slope, col="darkblue")}
  
  return(list(slope=slope, intercept=intercept, nreg_points=nreg_points,
              pvalue=pvalue, rsquared=rsquared, adj_rsquared=adj_rsquared))
}

calc_Ccontent_size <- function(Filter_ID, TEP_data, Ccontent_classvec, 
                               Ccontent_master, count, isEmpty)
{
  if(isEmpty){#only possible like this, because the first filter is never empty
    # Ccontent_master <- addto_masterhist(filter_ID = Filter_ID, hists = NA,
    #                                     masterhist = Ccontent_master,
    #                                     isEmpty = TRUE) #add line with zeros
    assign(paste(as.name(Filter_ID)),rep(0, length(Ccontent_master$mids)))
    histdata<-data.frame(mids=Ccontent_master$mids, eval(as.name(Filter_ID)))
    names(histdata)<-c("mids", paste(Filter_ID))
    Ccontent_master<-merge(Ccontent_master, histdata, by="mids")
  }else{
    #calculate particle based carboncontent
    TEP_data$Ccontent <- 0.25*10^(-6)*(TEP_data$`ESD[um]`/2)^2.55
    Ccontent_vec <- rep(NA, (length(Ccontent_classvec)-1)) #prepare an empty vec
    #loop over size classes
    for(i_func in 1:length(Ccontent_vec))
    { #include particles smaller than the upper and larger than the lower border
      #as I just learned, square brackets are safer, than subset
      intdata<-TEP_data[TEP_data$`ESD[um]` < Ccontent_classvec[i_func+1],]
      intdata<-intdata[intdata$`ESD[um]` > Ccontent_classvec[i_func], ]
      if(nrow(intdata)<1){
        #if no particles in the size class -> Ccontent = 0
        Ccontent_vec[i_func] <-0
      }else{
        #Ccontent of size class sum Ccontent of individual particles
        Ccontent_vec[i_func] <- sum(intdata$Ccontent)
      }
    }
    Ccontent_classvec <- c(Ccontent_classvec, "sum")
    Ccontent_vec <- c(Ccontent_vec, sum(Ccontent_vec))
    if(count==1) #in line with the masterhist part of the script
    { #the upper borders are called mids for compatibility with addto_masterhist
      assign(paste(as.name(Filter_ID)),Ccontent_vec)
      Ccontent_master<-data.frame(mids=Ccontent_classvec[-1], #upper borders!!!
                             eval(as.name(Filter_ID)))
      names(Ccontent_master)<-c("mids", paste(Filter_ID))
    } else
    {#the carboncontent is called counts for compatibility with addro_masterhist
      Ccontent_int <- data.frame(mids=Ccontent_classvec[-1],
                                 counts=Ccontent_vec)
      Ccontent_master<-addto_masterhist(filter_ID=Filter_ID, hists=Ccontent_int,
                                   masterhist=Ccontent_master,
                                   isEmpty=FALSE)
    }
  }
    
  return(Ccontent_master)
}




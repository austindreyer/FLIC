source("ExpDesignFunctions.R")
require(ggplot2)



#####Treatment based functions######
## Treatment based functions
## Divisions will create a separate graph for the cumulative PI (starting at range[0]) up
## to each of the division points in the experiment.  This is distinct from the time-dependent
## PI because it will always start from the first point in the data set (satisfying the first
## range parameter).
FeedingLicks.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingLicks.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingLicks.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
FeedingEvents.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingEvents.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    Feeding.Events.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
FeedingMeanDurations.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingMeanDuration.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingMeanDuration.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
FeedingMeanTimeBtw.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(parameters$Chamber.Size==1)
    FeedingMeanTimeBtw.OneWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else if(parameters$Chamber.Size==2)
    FeedingMeanTimeBtw.TwoWell.Trt(monitors,parameters,expDesign,range,divisions,SaveToFile)
  else
    stop("Feeding lick plots not implemented for this DFM type.")    
}
################################

###################################
## Will output Feeding.Summary data for each chamber in each monitor
## over the specified range to a .csv file.
Feeding.Summary.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0),file.output=TRUE){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    ##print(paste("Summarizing feeding data for DFM ",monitors[j],".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)  
    parameter.vector<-matrix(GetParameterVector(p),nrow=1)
    pnames<-Get.Parameter.Names(p)
    tmp<-Feeding.Summary(dfm,range)      
    tmp2<-data.frame(tmp,parameter.vector)
    names(tmp2)<-c(names(tmp),pnames)
    if(j==1){
        results<-tmp2
      }
      else {
        results<-rbind(results,tmp2)  
      }
      
  }  
  
  if(is.data.frame(expDesign)) {
    results<-AppendTreatmentonResultsFrame(results,expDesign)
    trt.summary<-suppressWarnings(AggregateTreatments(results))
    if(file.output==TRUE){
      filename<-paste("FeedingSummary_TRT_Stats",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(trt.summary,file=filename,row.names=FALSE)
      filename<-paste("FeedingSummary_TRT_Data",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
      write.csv(results,file=filename,row.names=FALSE)
    }
  }
  else if(file.output==TRUE){
    filename<-paste("FeedingSummary_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="")
    write.csv(results,file=filename,row.names=FALSE)
  }
  
  if(is.data.frame(expDesign)) {
    return(list(Results=results,Stats=trt.summary))
  }
  else {
    return(list(Results=results))
  }
}

Feeding.Summary<-function(dfm,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size==1)
    Feeding.Summary.OneWell(dfm,range)
  else if(dfm$Parameters$Chamber.Size==2)
    Feeding.Summary.TwoWell(dfm,range)
  else
    stop("Feeding Summary not implemented for this DFM type.")    
}

Feeding.Summary<-function(dfm,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size==1)
    Feeding.Summary.OneWell(dfm,range)
  else if(dfm$Parameters$Chamber.Size==2)
    Feeding.Summary.TwoWell(dfm,range)
  else
    stop("Feeding Summary not implemented for this DFM type.")    
}

## This function will output the baselined (and cleaned) analog
## values (along with minutes, parameter values, etc) to a 
## .csv file.
OutputBaselinedData.DFM<-function(dfm, range=c(0,0)){
  
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  
  tmp.all<-BaselineData(dfm,range)
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmp.all)),tmp.all))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  final.names<-c("DFM",names(tmp.all),pnames)
  names(tmp3)<-final.names
  
  filename<-paste("BaselinedData_DFM",dfm$ID,".csv",sep="") 
  write.csv(tmp3,file=filename,row.names=FALSE)  
}

## These functions will output the intervals
GetIntervalData.Well<-function(dfm,well, range=c(0,0)){
  nameA<-paste("W",well,sep="")
  parameter.vector<-GetDFMParameterVector(dfm)
  pnames<-Get.Parameter.Names(dfm$Parameters)
  
  theData<-dfm$Intervals[[nameA]]
  
  tmpA<-data.frame(rep(well,nrow(theData)),theData)
  names(tmpA)<-c("Well","Minutes","Sample","IntervalSec")
  
  tmp<-data.frame(cbind(rep(dfm$ID,nrow(tmpA)),tmpA))
  tmp2<-matrix(rep(parameter.vector,nrow(tmp)),ncol=length(parameter.vector),byrow=TRUE)
  
  tmp3<-cbind(tmp,tmp2)
  names(tmp3)<-c("DFM","Well","Minutes","Sample","IntervalSec",pnames)
  
  tmp3
}
GetIntervalData.DFM<-function(dfm,range){
  for(i in 1:12){
    tmp<-GetIntervalData.Well(dfm,i)
    if(i==1)
      result<-tmp
    else
      result<-rbind(result,tmp)  
  }
  result
}

## This function will output the baselined (and cleaned) analog
## values (along with minutes, parameter values, etc) to a 
## separate .csv file for each chamber in each of the specified monitors.
OutputBaselinedData.Monitors<-function(monitors,parameters,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    print(paste("Outputting Baselined Data for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    OutputBaselinedData.DFM(dfm,range)
    }
  }

OutputIntervalData.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  for(j in 1:length(monitors)){
    ##print(paste("Outputting Interval Data for DFM ",monitors[j],".",sep=""))
    ##flush.console()
    monitor<-monitors[j]
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    tmp2<-GetIntervalData.DFM(dfm,range)
    if(is.data.frame(expDesign))
      tmp2<-AppendTreatmentonResultsFrame(tmp2,expDesign)
    if(j==1){
      result<-tmp2
    }
    else {
      result<-rbind(result,tmp2)
    }
  }
  filename<-paste("IntervalData_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}
## This fucntion will output for each well in each chamber for each monitor
## the total amount of time spend drinking over the perscribed range.
OutputTotalFeeding.Monitors<-function(monitors,parameters,expDesign=NA,range=c(0,0)){
  individ.params<-FALSE
  ## Check to determine whether parameters is a signle parameter object
  ## or a list of them.  If it is a single one, then we use the same one for all
  ## if it is a list, then we use a different one for each.
  if(is.list(parameters[[1]])==TRUE){
    if(length(parameters)!=length(monitors))
      stop("If individuals parameter objects are specified, there must be one for each DFM.")
    individ.params<-TRUE
  }
  
  for(j in 1:length(monitors)){
    print(paste("Outputting TotalFeeding Data for DFM ",monitors[j],".",sep=""))
    flush.console()
    monitor<-monitors[j]
    x<-1:12
    if(individ.params==TRUE)
      p<-parameters[[j]]
    else
      p<-parameters
    dfm<-DFMClass(monitor,p)
    parameter.vector<-GetParameterVector(p)
    pnames<-Get.Parameter.Names(p)
    tmp<-Feeding.Summary(dfm,range)
    if(p$Chamber.Size==1){
      atotal<-tmp$Events*tmp$MeanDuration
      d<-tmp$DFM
      c<-tmp$Chamber
      tmp2<-matrix(rep(parameter.vector,length(d)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-data.frame(d,c,atotal,tmp2)
      names(tmp3)<-c("DFM","Chamber","TotalSec",pnames)
      if(j==1){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }
    }
    else if(p$Chamber.Size==2){
      atotal<-tmp$EventsA*tmp$MeanDurationA
      btotal<-tmp$EventsB*tmp$MeanDurationB
      d<-tmp$DFM
      c<-tmp$Chamber
      tmp2<-matrix(rep(parameter.vector,length(d)),ncol=length(parameter.vector),byrow=TRUE)
      tmp3<-data.frame(d,c,atotal,btotal,tmp2)
      names(tmp3)<-c("DFM","Chamber","ATotalSec","BTotalSec",pnames)
      if(j==1){
        result<-tmp3
      }
      else {
        result<-rbind(result,tmp3)  
      }      
    }
    else 
      stop("Feeding Summary not implemented for this DFM type.")    
  }
  
  if(is.data.frame(expDesign))
    result<-AppendTreatmentonResultsFrame(result,expDesign)
  
  filename<-paste("TotalFeedingTime_DFM",monitors[1],"_",monitors[length(monitors)],".csv",sep="") 
  write.csv(result,file=filename,row.names=FALSE)  
}

RawDataPlot.DFM<-function(dfm,range=c(0,0)) {
  ##windows(record=FALSE,width=8,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,2))
  if(dfm$Parameters$Chamber.Size==1) {
    for(i in c(1,3,5,7,9,11)){
      RawDataPlot.SingleWell(dfm,i,range)    
    }
  }
  else if(dfm$Parameters$Chamber.Size==2) {
    for(i in 1:6){
      RawDataPlot.TwoWell(dfm,i,range)    
      }
  }
  ##windows.options(record=FALSE) #stops recording.
}
BaselineDataPlot.DFM<-function(dfm,range=c(0,0)) {
  ##windows(record=FALSE,width=8,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,2))
  if(dfm$Parameters$Chamber.Size==1) {
    for(i in c(1,3,5,7,9,11)){
      BaselineDataPlot.SingleWell(dfm,i,range)    
    }
  }
  else if(dfm$Parameters$Chamber.Size==2) {
    for(i in 1:6){
      BaselineDataPlot.TwoWell(dfm,i,range)
    }
  }
  ##windows.options(record=FALSE) #stops recording.
}
Feeding.ThresholdPlots.DFM<-function(dfm,range=c(0,0)){
  ##windows(record=FALSE,width=16,height=12) # opens a window and starts recording
  op <- par(ask=FALSE)
  on.exit(par(op))
  par(mfrow=c(3,4))
  if(dfm$Parameters$Chamber.Size==1) {
    for(i in c(1,3,5,7,9,11)){
      Feeding.ThresholdPlots.SingleWell(dfm,i,range)     
    }
  }
  else if(dfm$Parameters$Chamber.Size==2) {
    for(i in 1:6){
      Feeding.ThresholdPlots.TwoWell(dfm,i,range)    
    }
  }
  ##windows.options(record=FALSE) #stops recording.  
}


#####Functions for case-specific calls########
Feeding.Summary.OneWell<-function(dfm,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=1)
    stop("This function is for single chambers only")
  
  for(i in 1:12 ){
    interval<-Feeding.IntervalSummary.Well(dfm,i,range)
    intensity<-Feeding.IntensitySummary.Well(dfm,i,range)
    dur<-Feeding.DurationSummary.Well(dfm,i,range)  
    FLicks<-Feeding.TotalLicks.Well(dfm,i,range)
    FEvents<-Feeding.TotalEvents.Well(dfm,i,range)    
    if(i==1)
      result<-data.frame(matrix(c(dfm$ID,i,FLicks,FEvents,unlist(dur),unlist(interval),unlist(intensity),range[1],range[2]),nrow=1))  
    else {
      tmp<-data.frame(matrix(c(dfm$ID,i,FLicks,FEvents,unlist(dur),unlist(interval),unlist(intensity),range[1],range[2]),nrow=1))  
      result<-rbind(result,tmp)
    }      
  }
  names(result)<-c("DFM","Chamber","Licks","Events","MeanDuration","MedDuration",
                   "MeanTimeBtw","MedTimeBtw","MeanInt","MedianInt","StartMin","EndMin")
  result    
  
}
Feeding.Summary.TwoWell<-function(dfm,range=c(0,0)){
  if(dfm$Parameters$Chamber.Size!=2)
    stop("This function is for two-chamber DFM only")
  
  cnames<-paste("C",1:nrow(dfm$Parameters$Chamber.Sets),sep="")
  for(i in 1:nrow(dfm$Parameters$Chamber.Sets)) {
    wellA<-dfm$Parameters$Chamber.Sets[i,1]
    wellB<-dfm$Parameters$Chamber.Sets[i,2] 
    
    interval.a<-Feeding.IntervalSummary.Well(dfm,wellA,range)
    intensity.a<-Feeding.IntensitySummary.Well(dfm,wellA,range)
    dur.a<-Feeding.DurationSummary.Well(dfm,wellA,range)  
    FLicks.a<-Feeding.TotalLicks.Well(dfm,wellA,range)
    FEvents.a<-Feeding.TotalEvents.Well(dfm,wellA,range)    
    
    interval.b<-Feeding.IntervalSummary.Well(dfm,wellB,range)
    intensity.b<-Feeding.IntensitySummary.Well(dfm,wellB,range)
    dur.b<-Feeding.DurationSummary.Well(dfm,wellB,range)  
    FLicks.b<-Feeding.TotalLicks.Well(dfm,wellB,range)
    FEvents.b<-Feeding.TotalEvents.Well(dfm,wellB,range) 
    
    FPIs<-c(Feeding.FinalPI.Chamber(dfm,i,range),Feeding.FinalEventPI.Chamber(dfm,i,range))
    
    if(i==1){
      result<-data.frame(matrix(c(dfm$ID,i,FPIs,FLicks.a,FLicks.b,FEvents.a,FEvents.b,unlist(dur.a),unlist(dur.b),unlist(interval.a),
                                  unlist(interval.b),unlist(intensity.a),unlist(intensity.b),range[1],range[2]),nrow=1))    
      
    }
    else {
      tmp<-data.frame(matrix(c(dfm$ID,i,FPIs,FLicks.a,FLicks.b,FEvents.a,FEvents.b,unlist(dur.a),unlist(dur.b),unlist(interval.a),
                               unlist(interval.b),unlist(intensity.a),unlist(intensity.b),range[1],range[2]),nrow=1))
      result<-rbind(result,tmp)      
    }
  }
  names(result)<-c("DFM","Chamber","PI","EventPI","LicksA","LicksB","EventsA","EventsB","MeanDurationA","MedDurationA",
                   "MeanDurationB","MedDurationB","MeanTimeBtwA","MedTimeBtwA",
                   "MeanTimeBtwB","MedTimeBtwB","MeanIntA","MedianIntA",
                   "MeanIntB","MedianIntB","StartMin","EndMin")
  result    
  
}
FeedingLicks.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingLicksBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
    print(summary(aov(Licks~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        print(summary(aov(Licks~Treatment,data=results)))
        r<-paste("Licks -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
 
} 
FeedingEvents.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingEventsBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Events -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
    print(summary(aov(Events~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Events -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
        print(summary(aov(Events~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
} 
FeedingLicks.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingLicksBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    Licks<-results$LicksA+results$LicksB
    results<-data.frame(results,Licks)
    r<-paste("Licks -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        Licks<-results$LicksA+results$LicksB
        results<-data.frame(results,Licks)
        r<-paste("Licks -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Licks)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Licks),max(results$Licks))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  print(summary(aov(Licks~Treatment,data=results)))
} 
FeedingEvents.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingEventsBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    Licks<-results$LicksA+results$LicksB
    results<-data.frame(results,Licks)
    r<-paste("Events -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        Licks<-results$LicksA+results$LicksB
        results<-data.frame(results,Licks)
        r<-paste("Events -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$Events)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$Events),max(results$Events))) + ggtitle(r) + xlab("Treatment") +ylab("Licks") + guides(fill=FALSE))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  print(summary(aov(Events~Treatment,data=results)))
} 
FeedingMeanDuration.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanDurBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Durations -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
    print(summary(aov(MeanDuration~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Durations -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
        print(summary(aov(MeanDuration~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
} 
FeedingMeanTimeBtw.OneWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanBtwBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    r<-paste("Time Btw -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
    print(summary(aov(MeanTimeBtw~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        r<-paste("Time Btw -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
        print(summary(aov(MeanTimeBtw~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  
} 
FeedingMeanDuration.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanDurBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    MeanDuration <- ((results$MeanDurationA*results$EventsA)+ (results$MeanDurationB*results$EventsB))/(results$EventsA+results$EventsB)
    results<-data.frame(results,MeanDuration)
    r<-paste("Durations -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
    print(summary(aov(MeanDuration~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        MeanDuration <- ((results$MeanDurationA*results$EventsA)+ (results$MeanDurationB*results$EventsB))/(results$EventsA+results$EventsB)
        results<-data.frame(results,MeanDuration)
        r<-paste("Durations -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanDuration)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanDuration),max(results$MeanDuration))) + ggtitle(r) + xlab("Treatment") +ylab("Duration") + guides(fill=FALSE))
        print(summary(aov(MeanDuration~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
  
} 
FeedingMeanTimeBtw.TwoWell.Trt<-function(monitors,parameters,expDesign,range=c(0,0),divisions=1,SaveToFile=FALSE){
  if(SaveToFile==TRUE){
    filename<-paste("FeedingMeanBtwBoxPlots_TRT",monitors[1],"_",monitors[length(monitors)],".pdf",sep="")
    pdf(file=filename)
  }
  
  ranges<-matrix(rep(NA,divisions*2),ncol=2)
  if(divisions==1)
    ranges[1,]<-range
  else {
    if(range[2]==0){
      ## Come back to here
      dfm<-DFMClass(monitors[1],parameters)
      last.time<-LastSampleData(dfm)$Minutes
      breaks<-seq(from=range[1], to=last.time, length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
    else {
      breaks<-seq(from=range[1], to=range[2], length=divisions+1)
      ranges.1<-range[1]
      ranges.2<-breaks[-1]
      ranges<-round(cbind(ranges.1,ranges.2))
    }
  }
  
  if(divisions==1) {
    tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,range,FALSE)
    results<-tmp$Results
    results<-subset(results,Treatment!="None")
    MeanTimeBtw <- ((results$MeanTimeBtwA*results$EventsA)+ (results$MeanTimeBtwB*results$EventsB))/(results$EventsA+results$EventsB)
    results<-data.frame(results,MeanTimeBtw)
    r<-paste("Time Btw -- Range(min): (",range[1],",",range[2],")",sep="")
    print(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
            ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
    print(summary(aov(MeanTimeBtw~Treatment,data=results)))
  }
  else {
    p<-list()
    for(i in 1:divisions)
      local({
        tmp<-Feeding.Summary.Monitors(monitors,parameters,expDesign,ranges[i,],FALSE)
        results<-tmp$Results
        results<-subset(results,Treatment!="None")
        MeanTimeBtw <- ((results$MeanTimeBtwA*results$EventsA)+ (results$MeanTimeBtwB*results$EventsB))/(results$EventsA+results$EventsB)
        results<-data.frame(results,MeanTimeBtw)
        r<-paste("Time Btw -- Range(min): (",ranges[i,1],",",ranges[i,2],")",sep="")
        p[[i]]<<-(ggplot(results, aes(results$Treatment, results$MeanTimeBtw)) + geom_boxplot(aes(fill = results$Treatment),outlier.size=-1) + geom_jitter(size=3,height=0) +
                    ylim(c(min(results$MeanTimeBtw),max(results$MeanTimeBtw))) + ggtitle(r) + xlab("Treatment") +ylab("Time Between") + guides(fill=FALSE))
        print(summary(aov(MeanTimeBtw~Treatment,data=results)))
      })
    if(divisions<5)
      numcols<-2
    else if(divisions<10)
      numcols<-3
    else if(divisions<17)
      numcols<-4
    else
      numcols<-5
    multiplot(plotlist=p,cols=numcols)
  }
  if(SaveToFile==TRUE)
    graphics.off()
 
} 



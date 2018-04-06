## making plots for figure ##

# colors from their paper:
#
# dh44/+ = goldenrod
# dh44 = royalblue1
#
# + > Kir2.1 = black
# hug > + = goldenrod
# hug > Kir2.1 = royalblue1
#

## mutant plot is a function that makes nice clear plots, no gridlines,
## larger objects, almost publication grade. Change the 'fill' value 
## under the geom_point() definition to modify color of points

mutant.plot <- function (data,genotype) {
  mt <- data.frame(matrix(vector(),length(data[,1]),length(data[1,])-2))  
  
  mt$hour <- round_any(data$Min/60,0.5)
  
  for (i in 1:(length(mt[1,])-1)) {
    avg <- mean(data[,i+2])
    mt[i] <- data[i+2]/avg
  }
  
  mmt <- subset(mt, select = -c(hour))
  
  
  
  mmt <- Filter(function(x) !all(is.na(x)), mmt)
  
  
  mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
                            dimnames=list(c(),c('hour','mean','se'))),
                     stringsAsFactors = F)
  
  mrmt$hour <- mt$hour
  mrmt$mean <- rowMeans(mmt)
  
  for (i in 1:length(mmt[,1])){
    mrmt$se[i] <- sem(mmt[i,])
  }
  
  
  ggplot(mrmt[-(1:48),], aes(x=hour,y=mean)) + 
    geom_line() + 
    geom_errorbar(data=mrmt[-(1:48),],aes(ymin=mean-se,ymax=mean+se), width=3)+
    geom_point(size=2, shape=21, colour = 'black', fill='goldenrod') + 
    ggtitle(sprintf("%s",genotype)) +
    labs(x = "Time (hour)", 
         y = "Normalized feeding activity/fly") +
    scale_x_continuous(breaks=seq(from=0, to=(tail(mt$hour,n=1)+24), by=24)) +
    scale_y_continuous(breaks=seq(0,3,1), labels = (seq(0,3,1)))+
    #theme_light()
    theme_classic() +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.title = element_text(size=18),
          axis.text = element_text(size=12))
}

## mutant plots with royal blue color

royalblue.plot <- function (data,genotype) {
  mt <- data.frame(matrix(vector(),length(data[,1]),length(data[1,])-2))  
  
  mt$hour <- round_any(data$Min/60,0.5)
  
  for (i in 1:(length(mt[1,])-1)) {
    avg <- mean(data[,i+2])
    mt[i] <- data[i+2]/avg
  }
  
  mmt <- subset(mt, select = -c(hour))
  
  
  
  mmt <- Filter(function(x) !all(is.na(x)), mmt)
  
  
  mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
                            dimnames=list(c(),c('hour','mean','se'))),
                     stringsAsFactors = F)
  
  mrmt$hour <- mt$hour
  mrmt$mean <- rowMeans(mmt)
  
  for (i in 1:length(mmt[,1])){
    mrmt$se[i] <- sem(mmt[i,])
  }
  
  
  ggplot(mrmt, aes(x=hour,y=mean)) + 
    geom_line() + 
    geom_point(size=2, shape=21, colour = 'royalblue1', fill='royalblue1') + 
    ggtitle(sprintf("%s",genotype)) +
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Behavior/fly") +
    scale_x_continuous(breaks=seq(from=0, to=(tail(mt$hour,n=1)+24), by=12)) + 
    geom_errorbar(data=mrmt,aes(ymin=mean-se,ymax=mean+se), width=.1)+
    theme_classic() +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5))
}

## control plots black

control.plot <- function (data,genotype) {
  mt <- data.frame(matrix(vector(),length(data[,1]),length(data[1,])-2))  
  
  mt$hour <- round_any(data$Min/60,0.5)
  
  for (i in 1:(length(mt[1,])-1)) {
    avg <- mean(data[,i+2])
    mt[i] <- data[i+2]/avg
  }
  
  mmt <- subset(mt, select = -c(hour))
  
  
  
  mmt <- Filter(function(x) !all(is.na(x)), mmt)
  
  
  mrmt <- data.frame(matrix(vector(),length(mmt[,1]),3,
                            dimnames=list(c(),c('hour','mean','se'))),
                     stringsAsFactors = F)
  
  mrmt$hour <- mt$hour
  mrmt$mean <- rowMeans(mmt)
  
  for (i in 1:length(mmt[,1])){
    mrmt$se[i] <- sem(mmt[i,])
  }
  
  
  ggplot(mrmt, aes(x=hour,y=mean)) + 
    geom_line() + 
    geom_point(size=2, shape=21, colour = 'black', fill='black') + 
    ggtitle(sprintf("%s",genotype)) +
    labs(x = "Time (Hours)", 
         y = "Normalized Feeding Behavior/fly") +
    scale_x_continuous(breaks=seq(from=0, to=(tail(mt$hour,n=1)+24), by=12)) + 
    geom_errorbar(data=mrmt,aes(ymin=mean-se,ymax=mean+se), width=.1)+
    theme_classic() +
    theme(axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5))
}




## this is stuff for making plots with peak feeding time highlighted with vertical bars
ggplot(data = data.pdfdbtl, aes(x=hour, y=mean, color=genotype)) + geom_line() +
  geom_ribbon(data=data.pdfdbtl,aes(ymin=mean-se,ymax=mean+se, fill = genotype), alpha=0.25, colour=NA) +
  scale_color_manual(values = c("iso>DBT(L)"="black", "pdf>DBT(L)" = "red", "pdf>iso" = "gray")) +
  scale_fill_manual(values = c("iso>DBT(L)"="black", "pdf>DBT(L)" = "red", "pdf>iso" = "gray"))+
  theme_classic() +
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        axis.title = element_text(size=18),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))

ggplot(data = data.pdfdbtl.only, aes(x=hour, y=mean, color="red")) + geom_line() +
  geom_ribbon(data=data.pdfdbtl.only,aes(ymin=mean-se,ymax=mean+se, fill = "red"), alpha=0.25, colour=NA) +
  theme_classic() +
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5),
        axis.title = element_text(size=18),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12)) +
  geom_segment(aes(x = data.pdfdbtl.only[3,1], y = 0.25, 
                   xend = data.pdfdbtl.only[3,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash") +
  geom_segment(aes(x = data.pdfdbtl.only[70,1], y = 0.25, 
                   xend = data.pdfdbtl.only[70,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.pdfdbtl.only[125,1], y = 0.25, 
                   xend = data.pdfdbtl.only[125,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.pdfdbtl.only[168,1], y = 0.25, 
                   xend = data.pdfdbtl.only[168,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.pdfdbtl.only[229,1], y = 0.25, 
                   xend = data.pdfdbtl.only[229,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash") +
  geom_segment(aes(x = data.dbtiso.only[3,1], y = 0.2, 
                   xend = data.dbtiso.only[3,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash") +
  geom_segment(aes(x = data.dbtiso.only[66,1], y = 0.2, 
                   xend = data.dbtiso.only[66,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.dbtiso.only[112,1], y = 0.2, 
                   xend = data.dbtiso.only[112,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.dbtiso.only[157,1], y = 0.2, 
                   xend = data.dbtiso.only[157,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.dbtiso.only[212,1], y = 0.2, 
                   xend = data.dbtiso.only[212,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")


+
  geom_segment(aes(x = data.pdfdbtl.only[3,1], y = data.pdfdbtl.only[3,2], 
                   xend = data.pdfdbtl.only[3,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash") +
  geom_segment(aes(x = data.pdfdbtl.only[70,1], y = data.pdfdbtl.only[70,2], 
                   xend = data.pdfdbtl.only[70,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.pdfdbtl.only[125,1], y = data.pdfdbtl.only[125,2], 
                   xend = data.pdfdbtl.only[125,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.pdfdbtl.only[168,1], y = data.pdfdbtl.only[168,2], 
                   xend = data.pdfdbtl.only[168,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.pdfdbtl.only[229,1], y = data.pdfdbtl.only[229,2], 
                   xend = data.pdfdbtl.only[229,1], yend = 0),
               colour="red", size=0.5, linetype = "longdash") +
  geom_segment(aes(x = data.dbtiso.only[3,1], y = data.dbtiso.only[3,2], 
                   xend = data.dbtiso.only[3,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash") +
  geom_segment(aes(x = data.dbtiso.only[66,1], y = data.dbtiso.only[66,2], 
                   xend = data.dbtiso.only[66,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.dbtiso.only[112,1], y = data.dbtiso.only[112,2], 
                   xend = data.dbtiso.only[112,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.dbtiso.only[157,1], y = data.dbtiso.only[157,2], 
                   xend = data.dbtiso.only[157,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")+
  geom_segment(aes(x = data.dbtiso.only[212,1], y = data.dbtiso.only[212,2], 
                   xend = data.dbtiso.only[212,1], yend = 0),
               colour="black", size=0.5, linetype = "longdash")
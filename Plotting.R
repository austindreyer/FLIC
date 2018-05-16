## making plots of various styles ##
## Updated 4/23/2018 ##

## Making violin plots of power/period data

# fill is an argument so the space of each violin in a solid color
p <- ggplot(dilp.trp.data, aes(x = Genotype, y = Power, fill = Genotype)) + 
  geom_violin(trim = FALSE)

# color of fill is chosen using scale_fill_manual, theme_classic() makes a plot
# with no gridlines, geom_jitter() adds raw data points with a little jitter 
# so they do not all line up with one another exactly

p + geom_jitter(shape = 16, position = position_jitter(0.05)) + 
  scale_fill_manual(values = c("gray80", "red3", "gray80")) + 
  theme_classic() + theme(legend.position = "none") +
  coord_cartesian(ylim = c(-50,200))

# include option to rearrange the genotypes, must stipulate colors
# AFTER organizing on x-axis

p + geom_jitter(shape = 16, position = position_jitter(0.05)) + 
  theme_classic() + theme(legend.position = "none") + 
  scale_x_discrete(limits = c("Iso>Kir2.1/TM3", 
                              "Sif-Gal4>Kir.2.1.TM3", 
                              "Sif-Gal4>Iso")) + 
  scale_fill_manual(values = c("gray80", "gray80", "red3")) +
  coord_cartesian(ylim = c(-50,200))

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
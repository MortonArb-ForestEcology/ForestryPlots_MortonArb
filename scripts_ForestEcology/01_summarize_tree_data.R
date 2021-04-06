# Checking our Google Sheet Inventory and doing some quick summary stats

# -------------
# Some quick general things we'll reference later
# -------------
library(ggplot2) # Sometimes I call packages with ::, but I find that's a pain with ggplot 

path.save <- "G:/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/figures"
gtrees <- googlesheets4::gs4_find("ForestryPlots_ForestEco_Master")

# -------------
#Removing other sheets with similar names in other files
gtrees <- gtrees[(c=1),]
# -------------
# Reading in the data and doing some quick stats
# -------------
dat.plot.master <- data.frame(googlesheets4::sheets_read(gtrees, sheet='PlotLevelMaster'))

names(dat.plot.master) <- car::recode(names(dat.plot.master), "'PlotCode'='ForestryPlot'") # Making names consistent across datasets
dat.plot.master

# Note: I convert everything to data frame because I don't work with the tidyverse universe and sometimes weird things happen when I mix styles.  You don't have to do this, but this is how I work.
dat.tree <- data.frame(googlesheets4::sheets_read(gtrees, sheet="TreeData"))
dat.tree$ForestryPlot <- as.factor(dat.tree$ForestryPlot)
dat.tree$SubPlot <- as.factor(dat.tree$SubPlot)
dat.tree$TreeID <- as.factor(dat.tree$TreeID)
dat.tree$Density <- 1/dat.tree$SubPlotRadius_m # calculating a density so we can sum to get total plot density
dat.tree$BasalArea <- pi*(dat.tree$DBH/2)^2
summary(dat.tree)

# Correcting bearings so we're going FROM plot center to tree, not other way; this gives us deviation from north
dat.tree$Azimuth <- dat.tree$Azimuth-180 
# Doing a second Azimuth that ranges from 0-360 just so it makes sense with a compass.
#  We need an ifelse statement because otherwise we
dat.tree$Azimuth2 <- ifelse(dat.tree$Azimuth<0, 360+dat.tree$Azimuth, dat.tree$Azimuth) 
summary(dat.tree)

# Removing trees that are dead
dat.tree <- dat.tree[!grepl("dead", tolower(dat.tree$Notes)),]
summary(dat.tree)
# -------------

# -------------
# Doing a quick example of the plot
# -------------
summary(dat.tree[dat.tree$ForestryPlot=="CAOV-E",]) # Picking Carya ovata as a random example & picking one of the subplots
ggplot(data=dat.tree[dat.tree$SubPlot==721,]) +
  geom_point(aes(x=Azimuth2, y=Dist.from.Center, size=DBH)) +
  scale_x_continuous(limits=c(0,360), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,unique(dat.tree[dat.tree$SubPlot==721,"SubPlotRadius_m"])), expand=c(0,0)) +
  coord_polar()

# Making an example graphing each Forestry plot and saving as a multi-paged PDF
# Uncomment out pdf(...) and dev.off() (below plot) to automatically create a file with these dimensions & resolution
# pdf(file.path(path.save, "ForestryPlot_StemMaps_All.pdf"), height=8, width=8)
for(FP in unique(dat.tree$ForestryPlot)){
  # FP <- "CAOV-E" # soft-coding a forestry plot to graph if you want to test this
  # Note: the print() statement is needed because it's in a loop and sometimes that causes weird thigns
  print(
  ggplot(data=dat.tree[dat.tree$ForestryPlot==FP,]) +
    facet_wrap(~SubPlot, ncol=2) + # Each plot gets its own panel
    geom_point(aes(x=Azimuth2, y=Dist.from.Center, size=DBH)) +
    coord_polar() + # This is what makes it wrap around
    scale_x_continuous(limits=c(0,360), expand=c(0,0),
                       breaks=c(0,90,180,270), labels=c("N", "E", "S", "W")) + # This makes sure the values range 0-360
    scale_y_continuous(limits=c(0,max(dat.tree[dat.tree$ForestryPlot==FP,"SubPlotRadius_m"])), expand=c(0,0)) + # This makes sure the center is 0
    ggtitle(FP) +
    theme_bw()
  )
  
}
# dev.off()

# -------------

# -------------
# Getting some plot-level stats using 'aggregate' funciton
# -------------
# Getting the mean DBH
dat.subplot <- aggregate(dat.tree[,c("DBH", "SubPlotRadius_m")], 
                         by=dat.tree[,c("ForestryPlot", "SubPlot")],
                         FUN=mean) 
# names(dat.subplot)[which(names(dat.subplot)=="x")] <- "DBH.mean"
dat.subplot$DBH.sd <- aggregate(dat.tree[,c("DBH")],
                                by=dat.tree[,c("ForestryPlot", "SubPlot")],
                                FUN=sd)[,"x"] 
dat.subplot$n.trees <- aggregate(dat.tree[,c("DBH")],
                                 by=dat.tree[,c("ForestryPlot", "SubPlot")],
                                 FUN=length)[,"x"] 
dat.subplot[,c("Density", "BasalArea.tot")] <- aggregate(dat.tree[,c("Density", "BasalArea")],
                                                     by=dat.tree[,c("ForestryPlot", "SubPlot")],
                                                     FUN=sum)[,c("Density", "BasalArea")] 
summary(dat.subplot)

# Correcting basal area for differences in plot size; BA is in cm2
dat.subplot$BasalArea.Dens <- dat.subplot$BasalArea.tot/(pi*dat.subplot$SubPlotRadius_m^2)
summary(dat.subplot)


# Merging in some of the plot-level data so we can add colors to our plots
dat.subplot <- merge(dat.subplot, dat.plot.master[,c("ForestryPlot", "Family", "Species", "Jenkins2003Cat", "Drought.Tolerance", "WoodType", "MycoType", "Group")], all.x=T)
summary(dat.subplot)

# png(file.path(path.save, paste0("ForestryPlot_Summary_BasalArea_MycoType.png")), height=8, width=8, units="in", res=120)
ggplot(data=dat.subplot) +
  geom_boxplot(aes(x=ForestryPlot, y=BasalArea.Dens, fill=MycoType)) +
  scale_y_continuous(name="Basal Area cm2/m2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))
# dev.off()

# png(file.path(path.save, paste0("ForestryPlot_Summary_BasalArea_WoodType.png")), height=8, width=8, units="in", res=120)
ggplot(data=dat.subplot) +
  geom_boxplot(aes(x=ForestryPlot, y=BasalArea.Dens, fill=WoodType)) +
  scale_y_continuous(name="Basal Area cm2/m2") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=-45, hjust=0))
# dev.off( )
# -------------

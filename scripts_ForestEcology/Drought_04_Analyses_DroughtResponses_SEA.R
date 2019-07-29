# Very similar to other script, but using classic dendro SEA to verify
library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"

# ---------------------------------------
# 1. Compile drought data
# ---------------------------------------
# Compile drought data for Jami
# PDSI categories:
# Extreme Dry: <= -4.00
# Severe Dry: -3.99 to -3.00
# Mod. Dry: -2.99 to -2.00
# Mid-Range: -1.99 to 1.99
# Mod. Moist: 2.00 to 2.99
# Very Moist: 3.00 to 3.99
# Extreme Moist: >= 4.00

path.base <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/data/Meteorology"
# -----------------
# Western Regional Climate Center's WestWide Drought Tracker
# - From J. Abatzoglou; based on PRISM; probably skewed for Western US, but it'll work
# -----------------
dir(file.path(path.base, "Drought_WRCC"), ".csv")

spei.wrcc <- read.csv(file.path(path.base, "Drought_WRCC/SPEI.csv"))
spei.wrcc[spei.wrcc==-9999] <- NA
summary(spei.wrcc)

pdsi.wrcc <- read.csv(file.path(path.base, "Drought_WRCC/PDSI_raw.csv"))
pdsi.wrcc[pdsi.wrcc==-9999] <- NA
summary(pdsi.wrcc)

wrcc.drought <- stack(pdsi.wrcc[,2:ncol(pdsi.wrcc)])
names(wrcc.drought) <- c("pdsi.wrcc", "month.name")
wrcc.drought$year <- pdsi.wrcc$Year
wrcc.drought$spei.wrcc <- stack(spei.wrcc[,2:ncol(spei.wrcc)])[,1]
wrcc.drought$month <- car::recode(wrcc.drought$month.name, 
                                  "'Jan'='01'; 
                                  'Feb'='02'; 
                                  'Mar'='03'; 
                                  'Apr'='04'; 
                                  'May'='05'; 
                                  'Jun'='06'; 
                                  'Jul'='07'; 
                                  'Aug'='08'; 
                                  'Sep'='09'; 
                                  'Oct'='10'; 
                                  'Nov'='11'; 
                                  'Dec'='12'")
summary(wrcc.drought)
# -----------------

# -----------------
# NCDC Data
# -----------------
dir(file.path(path.base, "Drought_NCDC"), ".csv")

pdsi.ncdc <- read.csv(file.path(path.base, "Drought_NCDC/1102-pdsi-all-7-1895-2019.csv"), skip=3)
pdsi.ncdc$year <-  as.numeric(substr(pdsi.ncdc$Date, 1, 4))
pdsi.ncdc$month <-  as.factor(substr(pdsi.ncdc$Date, 5, 6))
names(pdsi.ncdc)[2] <- "pdsi.ncdc"
summary(pdsi.ncdc)
# -----------------


# -----------------
# Put things together
# -----------------
drought.merge <- merge(pdsi.ncdc[,c("year", "month", "pdsi.ncdc")], wrcc.drought[,c("year", "month", "pdsi.wrcc", "spei.wrcc")], all=F)
summary(drought.merge)

drought.all <- stack(drought.merge[,c("pdsi.ncdc", "pdsi.wrcc", "spei.wrcc")])
drought.all[,c("year", "month")] <- drought.merge[,c("year", "month")]
drought.all$index <- ifelse(substr(drought.all$ind, 1, 4)=="pdsi", "pdsi", "spei")
drought.all$source <- ifelse(stringr::str_sub(drought.all$ind, -4, -1)=="ncdc", "ncdc", "wrcc")
summary(drought.all)

drought.mean <- aggregate(drought.merge[drought.merge$month %in% c("06", "07", "08"),c("pdsi.ncdc", "pdsi.wrcc", "spei.wrcc"),],
                          by=list(drought.merge[drought.merge$month %in% c("06", "07", "08"),c("year"),]),
                          FUN=mean)
names(drought.mean)[1] <- c("year")
drought.mean$type <- as.factor("summer.mean")
summary(drought.mean)

plot(pdsi.ncdc ~ pdsi.wrcc, data=drought.mean)
drought.mean[drought.mean$pdsi.ncdc<=-3,]
drought.mean[drought.mean$pdsi.wrcc<=-3,]
drought.mean[drought.mean$spei.wrcc<=-1,]

drought.min <- aggregate(drought.merge[drought.merge$month %in% c("06", "07", "08"),c("pdsi.ncdc", "pdsi.wrcc", "spei.wrcc"),],
                         by=list(drought.merge[drought.merge$month %in% c("06", "07", "08"),c("year"),]),
                         FUN=min)
names(drought.min)[1] <- c("year")
drought.min$type <- as.factor("summer.min")
summary(drought.min)

plot(pdsi.ncdc ~ pdsi.wrcc, data=drought.min)
drought.min[drought.min$pdsi.ncdc<=-3,]
drought.min[drought.min$pdsi.wrcc<=-3,]
# drought.min[drought.min$spei.wrcc<=-1,]

# Extreme Dry: <= -4.00
# Severe Dry: -3.99 to -3.00
# Mod. Dry: -2.99 to -2.00
# Mid-Range: -1.99 to 1.99
# Mod. Moist: 2.00 to 2.99
# Very Moist: 3.00 to 3.99
# Extreme Moist: >= 4.00

drought.summary <- rbind(drought.mean, drought.min)
# drought.summary$class.ncdc <- ifelse(drought.summary$pdsi.ncdc<=-4, "extreme dry", 
#                                      ifelse(drought.summary$pdsi.ncdc<=-3, "severe dry",
#                                             ifelse(drought.summary$pdsi.ncdc<=-2, "dry", 
#                                                    ifelse(drought.summary$pdsi.ncdc<2, "mid-range", 
#                                                           ifelse(drought.summary, "")))))

ggplot(data=drought.summary) +
  facet_wrap(~type) +
  geom_bar(aes(x=year, y=pdsi.ncdc, color=pdsi.ncdc), stat='identity') +
  scale_color_gradient2(low="red2", high="blue2", mid="gray50", midpoint=0)

png(file.path(path.google, "figures/Drought_Response", "TimeSeries_PDSI_NCDC_MeanMin.png"), height=8, width=10, units="in", res=120)
ggplot(data=drought.summary) +
  facet_grid(type~.) +
  geom_bar(aes(x=year, y=pdsi.ncdc, fill=pdsi.ncdc), stat='identity') +
  scale_fill_gradientn(name="PDSI", colors=c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), limits=max(abs(drought.summary$pdsi.ncdc))*c(-1,1)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Drought (PDSI)") +
  theme(legend.position = "top",
        panel.background = element_rect(color="black", fill="black"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.google, "figures/Drought_Response", "TimeSeries_SPEI_WRCC_MeanMin.png"), height=6, width=10, units="in", res=120)
ggplot(data=drought.summary[drought.summary$type=="summer.mean",]) +
  facet_grid(type~.) +
  geom_bar(aes(x=year, y=spei.wrcc, fill=spei.wrcc), stat='identity') +
  scale_fill_gradientn(name="SPEI", colors=c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), limits=max(abs(drought.summary$spei.wrcc))*c(-1,1)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Drought (SPEI)") +
  theme(legend.position = "top",
        panel.background = element_rect(color="black", fill="black"),
        panel.grid = element_blank())
dev.off()

# -----------------
# ---------------------------------------


# ---------------------------------------
# 2. Bring in tree-ring data to look at responses
# ---------------------------------------
yrs.drought <- unique(dat.all[dat.all$pdsi.ncdc<=-3, "year"])
yrs.drought <- yrs.drought[yrs.drought!=1964] # Taking out 1964 

path.rwl <- file.path(path.google, "data/RingsWidths_Raw/crossdated/")

# Get a list of the files (plots) we have available
files.rwl <- dir(path.rwl, ".rwl")

# Loop through each file and do some stuff to it
# dat.all <- data.frame() # Set up a empty data frame to store everything in
sea.all <- data.frame()
for(plt in 1:length(files.rwl)){
  PLT <- stringr::str_split(files.rwl[plt], "_")[[1]][1]
  
  # We have some plots that don't have "_combined" as part of the name & 
  # we want to exclude .rwl from the PlotID
  if(stringr::str_sub(PLT,-4,-1)==".rwl") PLT <- stringr::str_sub(PLT,1,-5)
  
  # -----------  
  # 1. Read in & detrend the raw ring width data
  # -----------  
  # Get the raw data
  dat.rwl <- dplR::read.rwl(file.path(path.rwl, files.rwl[plt])) # read in raw data
  
  # Detrend ring-widths to remove potentially confounding tree-level temporal trends
  #  - NOTE: There are many ways we could detrend.  We're using spline as a default.
  dat.detrend <- dplR::detrend(dat.rwl, method="Spline")
  
  # Turn things into a chronology
  dat.chron <- dplR::chron(dat.detrend)
  png(file.path(path.rwl, paste0(PLT, "_chronology.png")), height=6, width=8, units="in", res=120)
  dplR::plot.crn(dat.chron, crn.lwd=2, crn.line.col="red2")
  dev.off()
  
  # Do an SEA
  sea.out <- dplR::sea(dat.chron, yrs.drought)
  sea.out$PlotID <- PLT
  
  sea.all <- rbind(sea.all, sea.out)
}
sea.all$PlotID <- as.factor(sea.all$PlotID)
summary(sea.all)


png(file.path(path.google, "figures/Drought_Response", "Drought_Effect_SEA_StatSig.png"), height=8, width=10, units="in", res=120)
ggplot(data=sea.all) +
  facet_wrap(~PlotID) +
  geom_bar(data=sea.all[sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
  geom_bar(data=sea.all[sea.all$p<0.05,], aes(x=as.factor(lag), y=se, fill="sig"), stat="identity") +
  scale_fill_manual(name="", values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Drought Effect") +
  theme_bw() +
  theme(legend.position = "top")
dev.off()

write.csv(sea.all, file.path(path.google, "data/Drought_Response", "DroughtResp_SEA_out.csv"), row.names=F)  
# ---------------------------------------
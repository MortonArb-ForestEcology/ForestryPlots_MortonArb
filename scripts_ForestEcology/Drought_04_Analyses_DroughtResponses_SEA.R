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

# -----------------
# ---------------------------------------


# ---------------------------------------
# 2. Bring in tree-ring data to look at responses
# ---------------------------------------
drt.extreme <- unique(dat.all[dat.all$pdsi.ncdc<=-4, "year"])
drt.extreme <- sort(drt.extreme)

drt.severe <- unique(dat.all[dat.all$pdsi.ncdc<=-3, "year"])
drt.severe <- sort(drt.severe)

path.crn <- file.path(path.google, "data/RingsWidths_Raw/crossdated/")

# Get a list of the files (plots) we have available
files.crn <- dir(path.crn, ".crn")

# Loop through each file and do some stuff to it
# dat.all <- data.frame() # Set up a empty data frame to store everything in
sea.all <- data.frame()
for(plt in 1:length(files.crn)){
  PLT <- stringr::str_split(files.crn[plt], "_")[[1]][1]
  
  # We have some plots that don't have "_combined" as part of the name & 
  # we want to exclude .crn from the PlotID
  if(stringr::str_sub(PLT,-4,-1)==".crn") PLT <- stringr::str_sub(PLT,1,-5)
  
  # -----------  
  # 1. Read in & detrend the raw ring width data
  # -----------  
  # Get the chrnology
  dat.crn <- dplR::read.crn(file.path(path.crn, files.crn[plt])) # read in raw data
  
  for(type.drought in c("severe", "extreme")){
    # Do an SEA
    yrs.drought <- ifelse(type.drought=="extreme", drt.extreme, drt.severe)
    
    sea.out <- dplR::sea(dat.crn, yrs.drought)
    sea.out$PlotID <- PLT
    sea.out$type <- type.drought
    
    sea.all <- rbind(sea.all, sea.out)
  }
}
sea.all$PlotID <- as.factor(sea.all$PlotID)
summary(sea.all)

for(PLT in unique(sea.all$PlotID)){
  # png(file.path(path.google, "figures/Drought_Response", "Drought_Effect_SEA_StatSig_Extreme.png"), height=6, width=6, units="in", res=120)
  if(nrow(sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT & !is.na(sea.all$p),])==0) next
  
  png(file.path(path.google, "figures/Drought_Response/SEM_bySpecies", paste0("Drought_Effect_SEA_StatSig_Severe_", PLT, ".png")), height=6, width=6, units="in", res=120)
  if(nrow(sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p<0.05,])>0){
    print(
      ggplot(data=sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT,]) +
        facet_wrap(~PlotID) +
        geom_bar(data=sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
        geom_bar(data=sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p<0.05,], aes(x=as.factor(lag), y=se, fill="sig"), stat="identity") +
        scale_fill_manual(name="", values=c("gray50", "red2")) +
        geom_hline(yintercept=0, size=0.5) +
        scale_x_discrete(name="Drought Lag") +
        scale_y_continuous(name="Drought Effect") +
        theme_bw() +
        theme(legend.position = "top",
              legend.key = element_rect(fill=NA),
              panel.spacing = unit(0, "lines"),
              panel.grid = element_blank(),
              panel.background=element_rect(fill=NA, color="black"))
    )
    
  } else {
    print(
      ggplot(data=sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT,]) +
        facet_wrap(~PlotID) +
        geom_bar(data=sea.all[sea.all$type=="severe" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
        scale_fill_manual(name="", values=c("gray50", "red2")) +
        geom_hline(yintercept=0, size=0.5) +
        scale_x_discrete(name="Drought Lag") +
        scale_y_continuous(name="Drought Effect") +
        theme_bw() +
        theme(legend.position = "top",
              legend.key = element_rect(fill=NA),
              panel.spacing = unit(0, "lines"),
              panel.grid = element_blank(),
              panel.background=element_rect(fill=NA, color="black"))
    )
    
  }
  dev.off()
  
  if(nrow(sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT & !is.na(sea.all$p),])==0) next
  
  png(file.path(path.google, "figures/Drought_Response/SEA_bySpecies", paste0("Drought_Effect_SEA_StatSig_Extreme_", PLT, ".png")), height=6, width=6, units="in", res=120)
  if(nrow(sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p<0.05,])>0){
    print(
      ggplot(data=sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT,]) +
        facet_wrap(~PlotID) +
        geom_bar(data=sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
        geom_bar(data=sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p<0.05,], aes(x=as.factor(lag), y=se, fill="sig"), stat="identity") +
        scale_fill_manual(name="", values=c("gray50", "red2")) +
        geom_hline(yintercept=0, size=0.5) +
        scale_x_discrete(name="Drought Lag") +
        scale_y_continuous(name="Drought Effect") +
        theme_bw() +
        theme(legend.position = "top",
              legend.key = element_rect(fill=NA),
              panel.spacing = unit(0, "lines"),
              panel.grid = element_blank(),
              panel.background=element_rect(fill=NA, color="black"))
    )
    
  } else {
    print(
      ggplot(data=sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT,]) +
        facet_wrap(~PlotID) +
        geom_bar(data=sea.all[sea.all$type=="extreme" & sea.all$PlotID==PLT & !is.na(sea.all$p) & sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
        scale_fill_manual(name="", values=c("gray50", "red2")) +
        geom_hline(yintercept=0, size=0.5) +
        scale_x_discrete(name="Drought Lag") +
        scale_y_continuous(name="Drought Effect") +
        theme_bw() +
        theme(legend.position = "top",
              legend.key = element_rect(fill=NA),
              panel.spacing = unit(0, "lines"),
              panel.grid = element_blank(),
              panel.background=element_rect(fill=NA, color="black"))
    )
    
  }
  dev.off()
  
}


png(file.path(path.google, "figures/Drought_Response", "Drought_Effect_SEA_StatSig_Extreme.png"), height=8, width=10, units="in", res=120)
ggplot(data=sea.all[sea.all$type=="extreme",]) +
  facet_wrap(~PlotID) +
  geom_bar(data=sea.all[sea.all$type=="extreme" & !is.na(sea.all$p) & sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
  geom_bar(data=sea.all[sea.all$type=="extreme" & !is.na(sea.all$p) & sea.all$p<0.05,], aes(x=as.factor(lag), y=se, fill="sig"), stat="identity") +
  geom_hline(yintercept=0, size=0.5) +
  scale_fill_manual(name="", values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Drought Effect") +
  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Drought_Effect_SEA_StatSig_Severe.png"), height=8, width=10, units="in", res=120)
ggplot(data=sea.all[sea.all$type=="severe",]) +
  facet_wrap(~PlotID) +
  geom_bar(data=sea.all[sea.all$type=="severe" & !is.na(sea.all$p) & sea.all$p>=0.05,], aes(x=as.factor(lag), y=se, fill="n.s."), stat="identity") +
  geom_bar(data=sea.all[sea.all$type=="severe" & !is.na(sea.all$p) & sea.all$p<0.05,], aes(x=as.factor(lag), y=se, fill="sig"), stat="identity") +
  scale_fill_manual(name="", values=c("gray50", "red2")) +
  geom_hline(yintercept=0, size=0.5) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="Drought Effect") +
  theme_bw() +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
outdev.off()

write.csv(sea.all, file.path(path.google, "data/Drought_Response", "DroughtResp_SEA_out.csv"), row.names=F)  
# ---------------------------------------

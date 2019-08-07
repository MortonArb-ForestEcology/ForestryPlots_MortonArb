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
drought.min[drought.min$pdsi.ncdc<=-4,]

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

# ggplot(data=drought.summary) +
#   facet_wrap(~type) +year=~1, 
#   geom_bar(aes(x=year, y=pdsi.ncdc, color=pdsi.ncdc), stat='identity') +
#   scale_color_gradient2(low="red2", high="blue2", mid="gray50", midpoint=0)

png(file.path(path.google, "figures/Drought_Response", "TimeSeries_PDSI_NCDC_Mean.png"), height=8, width=10, units="in", res=120)
ggplot(data=drought.summary[drought.summary$type=="summer.mean",]) +
  facet_grid(type~.) +
  geom_bar(aes(x=year, y=pdsi.ncdc, fill=pdsi.ncdc), stat='identity') +
  scale_fill_gradientn(name="PDSI", colors=c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), limits=max(abs(drought.summary$pdsi.ncdc))*c(-1,1)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Drought (PDSI)") +
  theme(legend.position = "top",
        panel.background = element_rect(color="black", fill="black"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.google, "figures/Drought_Response", "TimeSeries_PDSI_NCDC_Mean2.png"), height=8, width=10, units="in", res=120)
ggplot(data=drought.summary[drought.summary$type=="summer.mean",]) +
  # facet_grid(type~.) +
  geom_bar(aes(x=year, y=pdsi.ncdc, fill=pdsi.ncdc), stat='identity') +
  scale_fill_gradientn(name="PDSI", colors=c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), limits=max(abs(drought.summary$pdsi.ncdc))*c(-1,1)) +
  geom_hline(yintercept=-4, color="red2", linetype="dashed") +
  geom_hline(yintercept=-3, color="orange3", linetype="dashed") +
  scale_color_manual(values=c("red2", "orange3")) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Drought (PDSI)") +
  theme(legend.position = "top",
        panel.background = element_rect(color="black", fill="black"),
        panel.grid = element_blank())
dev.off()

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
dat.tr <- read.csv(file.path(path.google, "data", "Data_TreeRings_compiled_all.csv"))
dat.tr$TreeID <- as.factor(substr(dat.tr$CoreID, 1, 6))
dat.tr <- dat.tr[dat.tr$year<2019,] # Exclude our incomplete year
dat.tr <- dat.tr[!is.na(dat.tr$Crossdated) & !dat.tr$Crossdated=="N",]
summary(dat.tr)

dat.tr$Wood <- NA # Trachied, Ring, Diffuse
dat.tr[dat.tr$PlotID %in% c("PLOC-W", "AEGL-E"), "Wood"] <- c("diffuse")
dat.tr[dat.tr$PlotID %in% c("ASTR-W", "ASTR-E", "CAOV-E", "QUAL-E", "QUBI-W", "ROPS-W"), "Wood"] <- c("ring")
dat.tr[is.na(dat.tr$Wood), "Wood"] <- "tracheid"
dat.tr$Wood <- as.factor(dat.tr$Wood)
summary(dat.tr)

dat.tr$Myco <- NA # AM/EM Mycorrhizal type
dat.tr[dat.tr$PlotID %in% c("AEGL-E", "PLOC-W", "ASTR-W", "ASTR-E", "ROPS-W", "CHPI-E", "JUCH-W", "THOC-W1", "THOC-W2"), "Myco"] <- c("AM")
dat.tr[is.na(dat.tr$Myco), "Myco"] <- "EM"
dat.tr$Myco <- as.factor(dat.tr$Myco)
summary(dat.tr)


# Just roll with the mean summer drought status right now
dat.all <- merge(dat.tr, drought.min, all.x=T)
summary(dat.all)

# ---------------------------------------
# Evaluating overall drought sensitivity
# ---------------------------------------

for(PLT in unique(dat.all$PlotID)){
  png(file.path(path.google, "figures/Drought_Response/LagResponse_LME_bySpecies", paste0("Exploratory_Drought_RWI_", PLT, ".png")), height=6, width=6, units="in", res=120)
  print(
    ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y" & dat.all$PlotID==PLT,]) +
      # facet_wrap(~PlotID, scales="free") +
      ggtitle(PLT) +
      geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
      stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
      scale_x_continuous(name="PDSI") +
      scale_y_continuous(expand=c(0,0), limits=range(dat.all$RWI, na.rm=T)) +
      geom_hline(yintercept=1, linetype="dashed", color="black") +
      theme_bw()
    )
  dev.off()
}

png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_all.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y",]) +
  facet_wrap(~PlotID, scales="free") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  scale_x_continuous(name="PDSI") +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  theme_bw()
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_all_fixedaxes.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y",]) +
  facet_wrap(~PlotID, scales="fixed") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  scale_x_continuous(name="PDSI") +
  theme_bw()
dev.off()


png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_byWood_byMyco.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y",]) +
  facet_grid(Myco~Wood, scales="free") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  scale_x_continuous(name="PDSI") +
  theme_bw()
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_byWood_byMyco_fixedaxes.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y",]) +
  facet_grid(Myco~Wood, scales="fixed") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  scale_x_continuous(name="PDSI") +
  theme_bw()
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_byWood_fixedaxes.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y",]) +
  facet_grid(.~Wood, scales="fixed") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  scale_x_continuous(name="PDSI") +
  theme_bw()
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_byMyco_fixedaxes.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y",]) +
  facet_grid(.~Myco, scales="fixed") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  scale_x_continuous(name="PDSI") +
  theme_bw()
dev.off()


# Plot-by-plot approach
# lm(RWI ~ pdsi.ncdc)

mod.drt.plot <- nlme::lme(RWI ~ pdsi.ncdc*PlotID-1 - pdsi.ncdc, random=list(year=~1, TreeID=~1, CoreID=~1), data=dat.all[!is.na(dat.all$RWI) & dat.all$Crossdated=="Y", ], na.action=na.omit)
summary(mod.drt.plot)
anova(mod.drt.plot)
sum.plot <- summary(mod.drt.plot)
sum.plot <- data.frame(sum.plot$tTable)
sum.plot <- round(sum.plot, 4)
sum.plot$factor <- rep(c("intercept", "slope"), each=length(unique(dat.all$PlotID)))
sum.plot$level <- row.names(sum.plot)
sum.plot$PlotID <- sort(unique(dat.all$PlotID))
sum.plot <- sum.plot[,c("level", "factor", "PlotID", "Value", "Std.Error", "DF", "t.value", "p.value")]
write.csv(sum.plot, file.path(path.google, "data/Drought_Response", "DroughtResponse_LME_stats_PlotID_eff.csv"), row.names=F)

mod.drt.plot.comp <- nlme::lme(RWI ~ pdsi.ncdc*PlotID, random=list(year=~1, TreeID=~1, CoreID=~1), data=dat.all[!is.na(dat.all$RWI) & dat.all$Crossdated=="Y", ], na.action=na.omit)
summary(mod.drt.plot.comp)
anova(mod.drt.plot.comp)


mod.drt.wood <- nlme::lme(RWI ~ pdsi.ncdc*Wood - pdsi.ncdc - 1, random=list(PlotID=~1, TreeID=~1, CoreID=~1, year=~1), data=dat.all[!is.na(dat.all$RWI) & dat.all$Crossdated=="Y", ], na.action=na.omit)
summary(mod.drt.wood)
anova(mod.drt.wood)
sum.wood <- summary(mod.drt.wood)
sum.wood <- data.frame(sum.wood$tTable)
sum.wood <- round(sum.wood, 4)
sum.wood$factor <- rep(c("intercept", "slope"), each=length(unique(dat.all$Wood)))
sum.wood$level <- row.names(sum.wood)
sum.wood$WoodType <- sort(unique(dat.all$Wood))
sum.wood <- sum.wood[,c("level", "factor", "WoodType", "Value", "Std.Error", "DF", "t.value", "p.value")]
write.csv(sum.wood, file.path(path.google, "data/Drought_Response", "DroughtResponse_LME_stats_Wood_effect.csv"), row.names=F)

mod.drt.wood.comp <- nlme::lme(RWI ~ pdsi.ncdc*relevel(Wood, "ring") , random=list(PlotID=~1, TreeID=~1, CoreID=~1, year=~1), data=dat.all[!is.na(dat.all$RWI) & dat.all$Crossdated=="Y", ], na.action=na.omit)
summary(mod.drt.wood.comp)
anova(mod.drt.wood.comp)
sum.wood.comp <- summary(mod.drt.wood.comp)
sum.wood.comp <- data.frame(sum.wood.comp$tTable)
sum.wood.comp <- round(sum.wood.comp, 4)
sum.wood.comp$factor <- c("intercept", "slope", rep(c("intercept (diff)", "slope (diff)"), each=length(unique(dat.all$Wood))-1))
sum.wood.comp$level <- row.names(sum.wood.comp)
sum.wood.comp$WoodType <- c("ring", "ring", "diffuse", "tracheid", "diffuse", "tracheid")
sum.wood.comp <- sum.wood.comp[,c("level", "factor", "WoodType", "Value", "Std.Error", "DF", "t.value", "p.value")]
write.csv(sum.wood.comp, file.path(path.google, "data/Drought_Response", "DroughtResponse_LME_stats_Wood_effect.csv"), row.names=F)


mod.drt.myco <- nlme::lme(RWI ~ pdsi.ncdc*Myco - pdsi.ncdc - 1, random=list(PlotID=~1, TreeID=~1, CoreID=~1, year=~1), data=dat.all[!is.na(dat.all$RWI) & dat.all$Crossdated=="Y", ], na.action=na.omit)
summary(mod.drt.myco)
anova(mod.drt.myco)
sum.myco <- summary(mod.drt.myco)
sum.myco <- data.frame(sum.myco$tTable)
sum.myco <- round(sum.myco, 4)
sum.myco$factor <- rep(c("intercept", "slope"), each=length(unique(dat.all$Myco)))
sum.myco$level <- row.names(sum.myco)
sum.myco$MycoType <- sort(unique(dat.all$Myco))
sum.myco <- sum.myco[,c("level", "factor", "MycoType", "Value", "Std.Error", "DF", "t.value", "p.value")]
write.csv(sum.myco, file.path(path.google, "data/Drought_Response", "DroughtResponse_LME_stats_Myco_effect.csv"), row.names=F)



mod.drt.myco.comp <- nlme::lme(RWI ~ pdsi.ncdc*Myco, random=list(PlotID=~1, TreeID=~1, CoreID=~1, year=~1), data=dat.all[!is.na(dat.all$RWI) & dat.all$Crossdated=="Y", ], na.action=na.omit)
summary(mod.drt.myco.comp)
anova(mod.drt.myco.comp)
sum.myco.comp <- summary(mod.drt.myco.comp)
sum.myco.comp <- data.frame(sum.myco.comp$tTable)
sum.myco.comp <- round(sum.myco.comp, 4)
sum.myco.comp$factor <- c("intercept", "slope", rep(c("intercept (diff)", "slope (diff)"), each=length(unique(dat.all$Myco))-1))
sum.myco.comp$level <- row.names(sum.myco.comp)
sum.myco.comp$MycoType <- c("AM", "AM", "EM", "EM")
sum.myco.comp <- sum.myco.comp[,c("level", "factor", "MycoType", "Value", "Std.Error", "DF", "t.value", "p.value")]
write.csv(sum.myco.comp, file.path(path.google, "data/Drought_Response", "DroughtResponse_LME_stats_Myco_effect.csv"), row.names=F)

# ---------------------------------------

# Extreme Drought years
drt.extreme <- unique(dat.all[dat.all$pdsi.ncdc<=-4, "year"])
drt.extreme <- sort(drt.extreme)

drt.severe <- unique(dat.all[dat.all$pdsi.ncdc<=-3, "year"])
drt.severe <- sort(drt.severe)

# drt.extreme <- drt.extreme[!drt.extreme %in% c(1964, 2006)] # Taking out 1964 because it's on the heels of 1963
# drt.extreme <- drt.extreme[years.drought %in% ]

# There's gotta be a better way to do the lag designation, but this works
dat.all$lag.extreme <- NA
dat.all$lag.extreme[dat.all$year %in% (drt.extreme-5)] <- -5
dat.all$lag.extreme[dat.all$year %in% (drt.extreme+5)] <- +5
dat.all$lag.extreme[dat.all$year %in% (drt.extreme-4)] <- -4
dat.all$lag.extreme[dat.all$year %in% (drt.extreme+4)] <- +4
dat.all$lag.extreme[dat.all$year %in% (drt.extreme-3)] <- -3
dat.all$lag.extreme[dat.all$year %in% (drt.extreme+3)] <- +3
dat.all$lag.extreme[dat.all$year %in% (drt.extreme-2)] <- -2
dat.all$lag.extreme[dat.all$year %in% (drt.extreme+2)] <- +2
dat.all$lag.extreme[dat.all$year %in% (drt.extreme-1)] <- -1
dat.all$lag.extreme[dat.all$year %in% (drt.extreme+1)] <- +1
dat.all$lag.extreme[dat.all$year %in% drt.extreme] <- 0

dat.all$lag.severe <- NA
dat.all$lag.severe[dat.all$year %in% (drt.severe-5)] <- -5
dat.all$lag.severe[dat.all$year %in% (drt.severe+5)] <- +5
dat.all$lag.severe[dat.all$year %in% (drt.severe-4)] <- -4
dat.all$lag.severe[dat.all$year %in% (drt.severe+4)] <- +4
dat.all$lag.severe[dat.all$year %in% (drt.severe-3)] <- -3
dat.all$lag.severe[dat.all$year %in% (drt.severe+3)] <- +3
dat.all$lag.severe[dat.all$year %in% (drt.severe-2)] <- -2
dat.all$lag.severe[dat.all$year %in% (drt.severe+2)] <- +2
dat.all$lag.severe[dat.all$year %in% (drt.severe-1)] <- -1
dat.all$lag.severe[dat.all$year %in% (drt.severe+1)] <- +1
dat.all$lag.severe[dat.all$year %in% drt.severe] <- 0

summary(dat.all)

dat.all$RWI.extreme <- dat.all$RWI-1
dat.all$RWI.severe <- dat.all$RWI-1

# Trying to re-center drought event & recovery; this should result in something similar to the standard dendro SEA, but using more ecological-based stats 
for(CORE in unique(dat.all$CoreID)){
  # Doing recentering on extreme droughts
  for(YR in unique(dat.all[!is.na(dat.all$lag.extreme) & dat.all$CoreID==CORE & dat.all$lag.extreme==0,"year"])){
    val.cent <- mean(dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR-1) & dat.all$lag.extreme<0 & !is.na(dat.all$lag.extreme),"RWI"], na.rm=T)
    dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR+5),"RWI.extreme"] <- dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR+5),"RWI"] - val.cent
 } # end years 
  
  for(YR in unique(dat.all[!is.na(dat.all$lag.severe) & dat.all$CoreID==CORE & dat.all$lag.severe==0,"year"])){
    val.cent <- mean(dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR-1) & dat.all$lag.severe<0 & !is.na(dat.all$lag.severe),"RWI"], na.rm=T)
    dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR+5),"RWI.severe"] <- dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR+5),"RWI"] - val.cent
  } # end years 
  
} # end cores
summary(dat.all)


# --------------
# Running the calculation
# --------------
# Setting up a table to stick our output
drought.resp <- data.frame(drought.type = rep(c("extreme", "severe"), each=length(-5:5)*length(unique(dat.all$PlotID))),
                           lag=rep(-5:5),
                           PlotID=rep(unique(dat.all$PlotID), each=length(-5:5)),
                           estimate=NA,
                           std.err=NA,
                           t.stat=NA,
                           p.val=NA)

for(PLT in unique(dat.all$PlotID)){
  
  for(TYPE in c("extreme", "severe")){
    dat.tmp <- dat.all[dat.all$PlotID==PLT & !is.na(dat.all$Crossdated) & dat.all$Crossdated=="Y" 
                       & !is.na(dat.all[,paste0("lag.", TYPE)]) & !is.na(dat.all[,paste0("RWI.", TYPE)]), ]

    if(nrow(dat.tmp)==0){
      warning(paste("No events for", PLT, TYPE, "-- skipping!"))
      next
    }
    dat.tmp$RWI.rel <- dat.tmp[,paste0("RWI.", TYPE)]
    dat.tmp$drought.lag <- dat.tmp[,paste0("lag.", TYPE)]
    
    if(length(which(dat.tmp$drought.lag==0))==0){
      warning(paste("No events for", PLT, TYPE, "-- skipping!"))
      next
    }
    
    mod.lag <- nlme::lme(RWI.rel ~ as.factor(drought.lag)-1, random=list(year=~1, TreeID=~1, CoreID=~1), data=dat.tmp)
    mod.sum <- summary(mod.lag)
    # mod.sum$tTable
    
    drought.resp[drought.resp$drought.type==TYPE & drought.resp$PlotID==PLT,"estimate"] <- mod.sum$tTable[,"Value"]
    drought.resp[drought.resp$drought.type==TYPE & drought.resp$PlotID==PLT,"std.err"] <- mod.sum$tTable[,"Std.Error"]
    drought.resp[drought.resp$drought.type==TYPE & drought.resp$PlotID==PLT,"t.stat"] <- mod.sum$tTable[,"t-value"]
    drought.resp[drought.resp$drought.type==TYPE & drought.resp$PlotID==PLT,"p.val"] <- mod.sum$tTable[,"p-value"]
    
  }
  
}
summary(drought.resp)

write.csv(drought.resp, file.path(path.google, "data/Drought_Response", "DroughtResp_LAG_LME_summary_output.csv"), row.names=F)  
# --------------------------------------

# --------------------------------------
# Graphing the drought response -- 
# *****Jamy, downlaod the csv file from google, change the file path, and start here! *****
# --------------------------------------
drought.resp <- read.csv(file.path(path.google, "data/Drought_Response", "DroughtResp_LAG_LME_summary_output.csv"))


ggplot(data=drought.resp) +
  facet_grid(drought.type~PlotID) +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val>=0.001,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val<0.001,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val<0.001 & drought.resp$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=drought.resp[!is.na(drought.resp$p.val) & drought.resp$p.val>=0.001 & drought.resp$lag==0,], aes(x=as.factor(lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme(panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))


dat.extreme <- dat.all[!is.na(dat.all$RWI.extreme) & !is.na(dat.all$lag.extreme) & dat.all$Crossdated=="Y",c("year", "CoreID", "PlotID", "Crossdated", "RWI.extreme", "lag.extreme", "pdsi.ncdc")]
names(dat.extreme) <- car::recode(names(dat.extreme), "'RWI.extreme'='RWI.rel'; 'lag.extreme'='lag'")
dat.extreme$drought.type <- as.factor("extreme")
summary(dat.extreme)

dat.severe <- dat.all[!is.na(dat.all$RWI.severe) & !is.na(dat.all$lag.severe) & dat.all$Crossdated=="Y",c("year", "CoreID", "PlotID", "Crossdated", "RWI.severe", "lag.severe", "pdsi.ncdc")]
names(dat.severe) <- car::recode(names(dat.severe), "'RWI.severe'='RWI.rel'; 'lag.severe'='lag'")
dat.severe$drought.type <- as.factor("severe")
summary(dat.severe)


dat.drought <- rbind(dat.extreme, dat.severe)
dat.drought <- merge(dat.drought, drought.resp, all.x=T)
dat.drought$sig[!is.na(dat.drought$p.val)] <- ifelse(dat.drought$p.val[!is.na(dat.drought$p.val)]<0.01, "sig", "n.s.")
dat.drought$sig <- as.factor(dat.drought$sig)
summary(dat.drought)

png(file.path(path.google, "figures/Drought_Response", "Drought_LagEffect_LME_StatSig_Extreme_Severe.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.drought[!is.na(dat.drought$lag),]) +
  facet_grid(drought.type~PlotID, scales="fixed") +
  geom_boxplot(aes(x=as.factor(lag), y=RWI.rel, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="RWI difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Drought_LagEffect_LME_StatSig_Severe.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.drought[!is.na(dat.drought$lag) & dat.drought$drought.type=="severe",]) +
  facet_wrap(~PlotID, scales="fixed") +
  geom_boxplot(aes(x=as.factor(lag), y=RWI.rel, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="RWI difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
dev.off()

png(file.path(path.google, "figures/Drought_Response", "Drought_LagEffect_LME_StatSig_Extreme.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.drought[!is.na(dat.drought$lag) & dat.drought$drought.type=="extreme",]) +
  facet_wrap(~PlotID, scales="fixed") +
  geom_boxplot(aes(x=as.factor(lag), y=RWI.rel, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="RWI difference") +
  theme(legend.position = "top",
        legend.key = element_rect(fill=NA),
        panel.spacing = unit(0, "lines"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill=NA, color="black"))
dev.off()

# ---------------------------------------


# ---------------------------------------
# 
# ---------------------------------------

# ---------------------------------------

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
dat.tr <- read.csv(file.path(path.google, "data", "Data_TreeRings_compiled_all.csv"))
dat.tr$TreeID <- as.factor(substr(dat.tr$CoreID, 1, 6))
summary(dat.tr)

# Just roll with the mean summer drought status right now
dat.all <- merge(dat.tr, drought.mean, all.x=T)
summary(dat.all)


png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_RWI_all.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[,]) +
  facet_wrap(~PlotID, scales="free") +
  geom_point(aes(x=pdsi.ncdc, y=RWI), size=0.5, color="gray50") +
  stat_smooth(aes(x=pdsi.ncdc, y=RWI), method="lm", color="blue", fill="blue", alpha=0.5) +
  geom_hline(yintercept=1, linetype="dashed", color="black") +
  theme_bw()
dev.off()

yrs.drought <- unique(dat.all[dat.all$pdsi.ncdc<=-3, "year"])
yrs.drought <- yrs.drought[yrs.drought!=1964] # Taking out 1964 because it's on the heels of 1963
# yrs.drought <- yrs.drought[years.drought %in% ]

# There's gotta be a better way to do the lag designation, but this works
dat.all$drought.lag <- NA
dat.all$drought.lag[dat.all$year %in% (yrs.drought-5)] <- -5
dat.all$drought.lag[dat.all$year %in% (yrs.drought+5)] <- +5
dat.all$drought.lag[dat.all$year %in% (yrs.drought-4)] <- -4
dat.all$drought.lag[dat.all$year %in% (yrs.drought+4)] <- +4
dat.all$drought.lag[dat.all$year %in% (yrs.drought-3)] <- -3
dat.all$drought.lag[dat.all$year %in% (yrs.drought+3)] <- +3
dat.all$drought.lag[dat.all$year %in% (yrs.drought-2)] <- -2
dat.all$drought.lag[dat.all$year %in% (yrs.drought+2)] <- +2
dat.all$drought.lag[dat.all$year %in% (yrs.drought-1)] <- -1
dat.all$drought.lag[dat.all$year %in% (yrs.drought+1)] <- +1
dat.all$drought.lag[dat.all$year %in% yrs.drought] <- 0

summary(dat.all)

png(file.path(path.google, "figures/Drought_Response", "Exploratory_Drought_lag_RWI_all.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$drought.lag),]) +
  facet_wrap(~PlotID, scales="free") +
  geom_boxplot(aes(x=as.factor(drought.lag), y=RWI)) +
  geom_hline(yintercept=1, linetype="solid", color="blue") +
  theme_bw()
dev.off()


dat.all$RWI.cent <- dat.all$RWI-1

# Trying to re-center drought event & recovery; this should result in something similar to the standard dendro SEA, but using more ecological-based stats 
for(CORE in unique(dat.all$CoreID)){
  for(YR in unique(dat.all[!is.na(dat.all$drought.lag) & dat.all$CoreID==CORE & dat.all$drought.lag==0,"year"])){
    val.cent <- mean(dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR-1) & dat.all$drought.lag<0 & !is.na(dat.all$drought.lag),"RWI"], na.rm=T)
    dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR+5),"RWI.rel"] <- dat.all[dat.all$CoreID==CORE & dat.all$year %in% (YR-5):(YR+5),"RWI"] - val.cent
 } # end years 
} # end cores
summary(dat.all)


# --------------
# Running the calculation
# --------------
# Setting up a table to stick our output
drought.resp <- data.frame(drought.lag=rep(-5:5),
                           PlotID=rep(unique(dat.all$PlotID), each=length(-5:5)),
                           estimate=NA,
                           p.val=NA)

for(PLT in unique(dat.all$PlotID)){
  drought.lag <- nlme::lme(RWI.rel ~ as.factor(drought.lag)-1, random=list(TreeID=~1, CoreID=~1), data=dat.all[dat.all$PlotID==PLT & !is.na(dat.all$drought.lag) & !is.na(dat.all$RWI.rel), ])
  mod.sum <- summary(drought.lag)
  # mod.sum$tTable
  
  drought.resp[drought.resp$PlotID==PLT,"estimate"] <- mod.sum$tTable[,"Value"]
  drought.resp[drought.resp$PlotID==PLT,"p.val"] <- mod.sum$tTable[,"p-value"]
  
}
summary(drought.resp)


ggplot(data=drought.resp) +
  facet_wrap(~PlotID) +
  geom_bar(data=drought.resp[drought.resp$p.val>=0.001,], aes(x=as.factor(drought.lag), y=estimate), stat="identity", fill="gray50") +
  # geom_vline(xintercept=as.factor(0), color="red") +
  geom_bar(data=drought.resp[drought.resp$p.val<0.001,], aes(x=as.factor(drought.lag), y=estimate), stat="identity", fill="black") +
  geom_bar(data=drought.resp[drought.resp$p.val<0.001 & drought.resp$drought.lag==0,], aes(x=as.factor(drought.lag), y=estimate), stat="identity", fill="red") +
  geom_bar(data=drought.resp[drought.resp$p.val>=0.001 & drought.resp$drought.lag==0,], aes(x=as.factor(drought.lag), y=estimate), stat="identity", fill="red", alpha=0.5) +
  theme_bw()

dat.all <- merge(dat.all, drought.resp, all.x=T)
dat.all$sig[!is.na(dat.all$p.val)] <- ifelse(dat.all$p.val[!is.na(dat.all$p.val)]<0.01, "sig", "n.s.")
dat.all$sig <- as.factor(dat.all$sig)
summary(dat.all)

png(file.path(path.google, "figures/Drought_Response", "Drought_Effect_LME_StatSig.png"), height=8, width=10, units="in", res=120)
ggplot(data=dat.all[!is.na(dat.all$drought.lag),]) +
  facet_wrap(~PlotID, scales="free") +
  geom_boxplot(aes(x=as.factor(drought.lag), y=RWI.rel, fill=sig)) +
  geom_hline(yintercept=0, linetype="solid", color="blue") +
  scale_fill_manual(values=c("gray50", "red2")) +
  scale_x_discrete(name="Drought Lag") +
  scale_y_continuous(name="RWI difference") +
  theme_bw() +
  theme(legend.position = "top")
dev.off()
# ---------------------------------------

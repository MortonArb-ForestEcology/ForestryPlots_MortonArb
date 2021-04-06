# Analysis of timing of ciritical period for growth
library(ggplot2)

#path.google <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"
path.google <- "G:/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"

# ----------------------------------
# 1. read in & format datasets: tree ring, climate
# ----------------------------------
dat.tr <- read.csv(file.path(path.google, "data", "Data_TreeRings_compiled_all.csv"))
dat.tr$TreeID <- as.factor(substr(dat.tr$CoreID, 1, 6))
dat.tr <- dat.tr[dat.tr$year<2019,]
summary(dat.tr)

# # Find valid DBH reconstructions
# dat.tree <- aggregate(dat.tr[,c("RW.mm", "DBH.cm", "RWI")],
                      # by=dat.tr[,c("PlotID", "TreeID", "CoreID", "Crossdated")],
                      # FUN=function(x){length(which(!is.na(x)))})
# summary(dat.tree)
# dat.tree[dat.tree$DBH.cm<10,]
# hist(dat.tree$DBH.cm)
# summary(dat.tr[dat.tr$DBH.cm>=80,])

dat.tree <- aggregate(dat.tr[dat.tr$Crossdated=="Y",c("RWI", "BAI.cm2")],
                      by=dat.tr[dat.tr$Crossdated=="Y",c("PlotID", "year", "TreeID")],
                      FUN=mean, na.rm=T)
summary(dat.tree)
# --------------
# Bring in Daymet & do some stuff
# --------------
dat.daymet <- read.csv(file.path(path.google, "data/Meteorology/", "Daymet_MortonArb_1980-2018.csv"))
summary(dat.daymet)

# Doing a 7-day smoothing of the met data; anchored on the date so that it's a cumulative effect
# vars.pred <- c("prcp.mm", "tmax.C", "tmin.C", "vp.Pa", "srad.Wm2")
vars.pred <- c("prcp.mm", "tmax.C")
for(i in 1:length(vars.pred)){
  dat.daymet[,paste0(vars.pred[i], ".wk")] <- zoo::rollapply(dat.daymet[,vars.pred[i]], width=7, align="right", FUN=mean, fill=NA)
  
}

# Setting up a ~6-month lag (after the solstice)
yday.solstice.sum <- lubridate::yday("2019-06-21")
yday.equinox.aut <- lubridate::yday("2019-09-23")
dat.lag <- dat.daymet[dat.daymet$yday>=yday.equinox.aut & dat.daymet$yday<366,]
dat.lag$year <- dat.lag$year+1
dat.lag$yday <- dat.lag$yday-365
summary(dat.lag)

dat.daymet <- rbind(dat.daymet, dat.lag)
summary(dat.daymet)
# --------------

# Bring the met data & ring data together
dat.all <- merge(dat.tree[!is.na(dat.tr$RWI),], dat.daymet[,4:ncol(dat.daymet)], all.x=F)
summary(dat.all)
# ----------------------------------


# ----------------------------------
# Running the analyses as a first pass
# ----------------------------------
plt.use <- unique(dat.all$PlotID)
days.use <- min(dat.all$yday):max(dat.all$yday)
vars.resp <- c("RWI")
# vars.resp="RWI"
mod.out <- data.frame(yday=rep(days.use), 
                      PlotID=rep(rep(plt.use, each=length(days.use)), length.out=length(days.use)*length(plt.use)*length(vars.resp)*length(vars.pred)),
                      resp=rep(rep(vars.resp, each=length(days.use)*length(plt.use)), length.out=length(days.use)*length(plt.use)*length(vars.resp)*length(vars.pred)),
                      pred=rep(vars.pred, each=length(days.use)*length(plt.use)*length(vars.resp)), 
                      t.stat=NA, p.val=NA, r.sq.m=NA)
mod.out$resp <- factor(mod.out$resp, levels=rev(vars.resp))
mod.out$pred <- factor(mod.out$pred, levels=vars.pred)
summary(mod.out)
summary(mod.out[mod.out$PlotID=="PIST-E",])

summary(mod.out[mod.out$PlotID=="PIST-E" & mod.out$resp=="RWI",])
summary(mod.out[mod.out$PlotID=="PIST-E" & mod.out$pred=="tmax.C",])
summary(mod.out[mod.out$PlotID=="PIST-E" & mod.out$pred=="prcp.mm",])

# head(mod.out)
# tail(mod.out)

# Looping through all of the models we could possibly want
pb <- txtProgressBar(min=0, max=nrow(mod.out), style = 3)
pb.ind <- 1
for(plt in unique(dat.all$PlotID)){
  # if(nrow(dat.all[dat.all$PlotID==plt & dat.all$Crossdated=="Y",])==0){
  #   warning(paste("No Crossdated Cores in", plt, "-- Skipping for now"))
  #   pb.ind=pb.ind + length(days.use)*length(vars.pred)
  #   next
  # }
  for(i in days.use){
    # Subset to just crossdated samples
    dat.tmp <- dat.all[dat.all$PlotID==plt & dat.all$yday==i,]

    for(VAR in vars.pred){
      # dat.tmp$PRED <- dat.tmp[,VAR]
      dat.tmp$PRED <- dat.tmp[,paste0(VAR, ".wk")]
      dat.tmp <- dat.tmp[!is.na(dat.tmp$PRED),]
      if(VAR=="prcp.mm"){
        dat.tmp$PRED[dat.tmp$PRED<=1e-6] <- 1e-6
        dat.tmp$PRED <- log(dat.tmp$PRED)
      }
      
      
      for(RESP in vars.resp){
        # Update our Progress bar
        setTxtProgressBar(pb, pb.ind)
        pb.ind = pb.ind+1
        
        out.ind <- which(mod.out$PlotID==plt & mod.out$pred==VAR & mod.out$yday==i & mod.out$resp==RESP)

        if(!is.na(mod.out[out.ind, "t.stat"])) next
        
        # Set up the response variable for our model to make it generalzied
        dat.tmp$RESP <- dat.tmp[,RESP]
        
        # Rather than having true 0 in the response, make it very small
        dat.tmp <- dat.tmp[!is.na(dat.tmp$RESP),]
        # dat.tmp <- dat.tmp[dat.tmp$RWI>=0.1,] # At least for now; drop missing/VERY tiny rings
        # summary(dat.tmp[dat.tmp$RESP<0.01,])
        if(RESP=="RWI"){
    	    dat.tmp$RESP <- log(dat.tmp$RESP)
        }

        
        # Run a simple mixed-effect model & save the summary so we can get the t-table
        # mod.var <- nlme::lme(RESP ~ PRED, random=list(TreeID=~1, CoreID=~1), data=dat.tmp[dat.tmp$CoreID!="608012B",], na.action=na.omit)
        
        # system.time(
        mod.var <- nlme::lme(RESP ~ PRED, random=list(TreeID=~1, year=~1), data=dat.tmp[!is.na(dat.tmp$RESP),], na.action=na.omit)
        # )
        mod.sum <- summary(mod.var)
        
        # Save our t-stat & pvalue for the climate predictor
        mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
        mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
        mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
      }
    }
  }
}
summary(mod.out)
summary(mod.out[mod.out$PlotID=="QUBI-W",])
summary(mod.out[!is.na(mod.out$p.val),])
write.csv(mod.out, file.path(path.google, "data/CriticalPeriods", "ClimateCorrs_Daily.csv"), row.names=F)  
# ----------------------------------

# ----------------------------------
# Plotting and exploring the output
# ----------------------------------
mod.out <- read.csv(file.path(path.google, "data/CriticalPeriods", "ClimateCorrs_Daily.csv"))
summary(mod.out)

yrs.mark <- data.frame(Label=c("p.Oct 1", "Jan 1", "Apr 1", "Jul 1", "Oct 1"), 
                       Date=c("2018-10-01", "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01"))
yrs.mark$mark.yday <- lubridate::yday(yrs.mark$Date)
yrs.mark$mark.yday[1] <- yrs.mark$mark.yday[1]-365

PRGn5 <- c("#7b3294", "#c2a5cf", "gray50", "#a6dba0", "#008837")

for(PLT in unique(mod.out$PlotID)){
  png(file.path(path.google, "figures/CriticalPeriods/t-scores_0.01_bySpecies", paste0("ClimateCorr_Daily_Smoothed7_t-stat_", PLT,".png")), height=10, width=8, units="in", res=120)
  print(
  ggplot(data=mod.out[mod.out$PlotID==PLT,]) +
    facet_grid(PlotID~., scales="free") +
    # ggtitle(PLT) +
    geom_tile(data=mod.out[mod.out$PlotID==PLT & mod.out$p.val>=0.01,], aes(x=yday, y=pred), fill="gray50") +
    geom_tile(data=mod.out[mod.out$PlotID==PLT & mod.out$p.val<0.01,], aes(x=yday, y=pred, fill=t.stat)) +
    # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
    geom_vline(xintercept = 0, linetype="dashed") +
    scale_y_discrete(expand=c(0,0)) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
    scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
    theme(legend.position="top",
          axis.title.y=element_blank())
  )
  dev.off()
}



png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_Smoothed7_t-stat.png"), height=10, width=8, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(pred~., scales="free") +
  ggtitle("Daily Climate Corr.: Sig. T-stat; RWI") +
  geom_tile(data=mod.out[mod.out$p.val>=0.05,], aes(x=yday, y=PlotID), fill="gray50") +
  geom_tile(data=mod.out[mod.out$p.val<0.05,], aes(x=yday, y=PlotID, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Forestry Plot", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme(legend.position="top")
dev.off()

png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_Smoothed7_t-stat_alpha0.01.png"), height=10, width=8, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(pred~., scales="free") +
  ggtitle("Daily Climate Corr.: Sig. T-stat; RWI") +
  geom_tile(data=mod.out[mod.out$p.val>=0.01,], aes(x=yday, y=PlotID), fill="gray50") +
  geom_tile(data=mod.out[mod.out$p.val<0.01,], aes(x=yday, y=PlotID, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Forestry Plot", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme(legend.position="top")
dev.off()

png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_Smoothed7_t-stat_alpha0.01_byPlot.png"), height=10, width=8, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(PlotID~., scales="free") +
  ggtitle("Daily Climate Corr.: Sig. T-stat; RWI") +
  geom_tile(data=mod.out[mod.out$p.val>=0.01,], aes(x=yday, y=pred), fill="gray50") +
  geom_tile(data=mod.out[mod.out$p.val<0.01,], aes(x=yday, y=pred, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Forestry Plot", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme(legend.position="top")
dev.off()

png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_Smoothed7_t-stat_alpha0.05_byPlot.png"), height=10, width=8, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(PlotID~., scales="free") +
  ggtitle("Daily Climate Corr.: Sig. T-stat; RWI") +
  geom_tile(data=mod.out[mod.out$p.val>=0.05,], aes(x=yday, y=pred), fill="gray50") +
  geom_tile(data=mod.out[mod.out$p.val<0.05,], aes(x=yday, y=pred, fill=t.stat)) +
  # geom_tile(aes(x=yday, y=resp, fill=t.stat)) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Forestry Plot", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="t-stat", colors=PRGn5, limits=max(mod.out$t.stat)*c(-1,1))+
  theme(legend.position="top")
dev.off()




png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_Smoothed7_r-val.png"), height=10, width=8, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(pred~., scales="free") +
  ggtitle("Daily Climate Corr.: r-value; RWI") +
  geom_tile(aes(x=yday, y=PlotID, fill=sqrt(r.sq.m)*sign(t.stat))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Forestry Plot", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="Marginal\nR-value", colors=PRGn5, limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
  theme(legend.position="top")
dev.off()

png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_Smoothed7_r-val_byPlot.png"), height=10, width=8, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(PlotID~., scales="free") +
  ggtitle("Daily Climate Corr.: r-value; RWI") +
  geom_tile(aes(x=yday, y=pred, fill=sqrt(r.sq.m)*sign(t.stat))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_y_discrete(name="Forestry Plot", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels = yrs.mark$Label) +
  scale_fill_gradientn(name="Marginal\nR-value", colors=PRGn5, limits=max(sqrt(mod.out$r.sq.m))*c(-1,1))+
  theme(legend.position="top")
dev.off()

# ----------------------------------

# ----------------------------------
# Doing some analyses and sumamries by mycorhizal type
# ----------------------------------
mod.out <- read.csv(file.path(path.google, "data/CriticalPeriods", "ClimateCorrs_Daily.csv"))

mod.out$Wood <- NA # Trachied, Ring, Diffuse
mod.out[mod.out$PlotID %in% c("PLOC-W", "AEGL-E"), "Wood"] <- c("diffuse")
mod.out[mod.out$PlotID %in% c("ASTR-W", "ASTR-E", "CAOV-E", "QUAL-E", "QUBI-W", "ROPS-W"), "Wood"] <- c("ring")
mod.out[is.na(mod.out$Wood), "Wood"] <- "tracheid"
mod.out$Wood <- as.factor(mod.out$Wood)
summary(mod.out)

mod.out$Myco <- NA # AM/EM Mycorrhizal type
mod.out[mod.out$PlotID %in% c("AEGL-E", "PLOC-W", "ASTR-W", "ASTR-E", "ROPS-W", "CHPI-E", "JUCH-W", "THOC-W1", "THOC-W2"), "Myco"] <- c("AM")
mod.out[is.na(mod.out$Myco), "Myco"] <- "EM"
mod.out$Myco <- as.factor(mod.out$Myco)
summary(mod.out)


png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_t-stat_byWood.png"), height=8, width=10, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(pred~Wood) +
  geom_point(aes(x=yday, y=t.stat), color="gray50", size=0.5, alpha=.5) +
  stat_smooth(aes(x=yday, y=t.stat)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing = unit(0, "lines"))
dev.off()

png(file.path(path.google, "figures/CriticalPeriods", "ClimateCorr_Daily_t-stat_byMyco.png"), height=8, width=10, units="in", res=120)
ggplot(data=mod.out) +
  facet_grid(pred~Myco) +
  geom_point(aes(x=yday, y=t.stat), color="gray50", size=0.5, alpha=.5) +
  stat_smooth(aes(x=yday, y=t.stat)) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        panel.spacing = unit(0, "lines"))
dev.off()


gam.prcp.wood <- mgcv::gamm(t.stat ~ s(yday, by=Wood), random=list(PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(gam.prcp.wood$lme)
summary(gam.prcp.wood$gam)

anova(gam.prcp.wood$gam)
anova(gam.prcp.wood$lme)


gam.prcp.myco <- mgcv::gamm(t.stat ~ s(yday, by=Myco), random=list(PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(gam.prcp.myco$gam)
anova(gam.prcp.myco$gam)

summary(gam.prcp.myco$lme)
anova(gam.prcp.myco$lme)

# ----------
# Precipitation
# ----------
lme.prcp.myco <- nlme::lme(t.stat ~ Myco, random=list(yday=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(lme.prcp.myco)
# lme.prcp.mycob <- nlme::lme(t.stat ~ Myco-1, random=list(yday=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
# summary(lme.prcp.mycob)

lme.prcp.myco2 <- nlme::lme(t.stat ~ Myco, random=list(yday=~1, Wood=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(lme.prcp.myco2)

lme.prcp.myco2b <- nlme::lme(t.stat ~ Myco-1, random=list(yday=~1, Wood=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(lme.prcp.myco2b)

lme.prcp.wood <- nlme::lme(t.stat ~ Wood, random=list(yday=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(lme.prcp.wood)

lme.prcp.wood2 <- nlme::lme(t.stat ~ Wood, random=list(yday=~1, Myco=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(lme.prcp.wood2)

lme.prcp.wood2b  <- nlme::lme(t.stat ~ Wood-1, random=list(yday=~1, Myco=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])
summary(lme.prcp.wood2b)

# ----------

# ----------
# Temperature effects
# ----------
lme.tmax.myco <- nlme::lme(t.stat ~ Myco, random=list(yday=~1, PlotID=~1), data=mod.out[mod.out$pred=="tmax.C",])
summary(lme.tmax.myco)

lme.tmax.myco2 <- nlme::lme(t.stat ~ Myco, random=list(yday=~1, Wood=~1, PlotID=~1), data=mod.out[mod.out$pred=="tmax.C",])
summary(lme.tmax.myco2)

lme.tmax.wood <- nlme::lme(t.stat ~ Wood, random=list(yday=~1, PlotID=~1), data=mod.out[mod.out$pred=="tmax.C",])
summary(lme.tmax.wood)

# Take into account mycorrhizal association
lme.tmax.wood2b <- nlme::lme(t.stat ~ Wood-1, random=list(yday=~1, Myco=~1, PlotID=~1), data=mod.out[mod.out$pred=="tmax.C",])
summary(lme.tmax.wood2b) # All are sensitive to temperature; overall negative --> warmer = worse

lme.tmax.wood2 <- nlme::lme(t.stat ~ Wood, random=list(yday=~1, Myco=~1, PlotID=~1), data=mod.out[mod.out$pred=="tmax.C",])
summary(lme.tmax.wood2) # Ring porous are more sensitive than other wood types
anova(lme.tmax.wood2)

# Ring porous more sensitive than diffuse & tracheid; tr
# ----------

# resp.prcp.wood <- nlme::lme(t.stat ~ Wood, random=list(yday=~1, PlotID=~1), data=mod.out[mod.out$pred=="prcp.mm",])

# ----------------------------------
# Analysis of timing of ciritical period for growth
library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"

# ----------------------------------
# 1. read in & format datasets: tree ring, climate
# ----------------------------------
dat.tr <- read.csv(file.path(path.google, "data", "Data_TreeRings_compiled_all.csv"))
dat.tr$TreeID <- as.factor(substr(dat.tr$CoreID, 1, 6))
dat.tr <- dat.tr[dat.tr$year<2019,]
summary(dat.tr)

# Find valid DBH reconstructions
dat.tree <- aggregate(dat.tr[,c("RW.cm", "DBH.cm", "RWI")],
                      by=dat.tr[,c("PlotID", "TreeID", "CoreID")],
                      FUN=function(x){length(which(!is.na(x)))})
summary(dat.tree)
dat.tree[dat.tree$DBH.cm<10,]
hist(dat.tree$DBH.cm)

# --------------
# Bring in Daymet & do some stuff
# --------------
dat.daymet <- read.csv(file.path(path.google, "data/Meteorology/", "Daymet_MortonArb_1980-2018.csv"))
summary(dat.daymet)

# Doing a 7-day smoothing of the met data; anchored on the date so that it's a cumulative effect
vars.pred <- c("prcp.mm", "tmax.C", "tmin.C", "vp.Pa", "srad.Wm2")
for(i in 1:length(vars.pred)){
  dat.daymet[,paste0(vars.pred[i], ".wk")] <- zoo::rollapply(dat.daymet[,vars.pred[i]], width=7, align="right", FUN=mean, fill=NA)
  
}

# Setting up a ~6-month lag (after the solstice)
dat.lag <- dat.daymet[dat.daymet$yday>172 & dat.daymet$yday<366,]
dat.lag$year <- dat.lag$year+1
dat.lag$yday <- dat.lag$yday-365
summary(dat.lag)

dat.daymet <- rbind(dat.daymet, dat.lag)
summary(dat.daymet)
# --------------

# Bring the met data & ring data together
dat.all <- merge(dat.tr[!is.na(dat.tr$RW.cm),], dat.daymet[,4:ncol(dat.daymet)], all.x=F)
summary(dat.all)
# ----------------------------------


# ----------------------------------
# Running the analyses as a first pass
# ----------------------------------
plt.use <- unique(dat.all$PlotID)
days.use <- min(dat.all$yday):max(dat.all$yday)
vars.resp <- c("BAI.cm2", "RWI")
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
  for(i in days.use){
    dat.tmp <- dat.all[dat.all$PlotID==plt & dat.all$yday==i,]
    for(VAR in vars.pred){
      # dat.tmp$PRED <- dat.tmp[,VAR]
      dat.tmp$PRED <- dat.tmp[,paste0(VAR, ".wk")]
      dat.tmp <- dat.tmp[!is.na(dat.tmp$PRED),]
      # if(VAR=="prcp.mm"){
        # dat.tmp$PRED[dat.tmp$PRED==0] <- 1e-3
        # dat.tmp$PRED <- log(dat.tmp$PRED)
      # } 
      
      
      for(RESP in vars.resp){
        # Update our Progress bar
        setTxtProgressBar(pb, pb.ind)
        pb.ind = pb.ind+1
        
        # Set up the response variable for our model to make it generalzied
        dat.tmp$RESP <- dat.tmp[,RESP]
        
        # Rather than having true 0 in the response, make it very small
        dat.tmp <- dat.tmp[!is.na(dat.tmp$RESP),]
        dat.tmp$RESP[dat.tmp$RESP==0] <- 1e-3
        
        # Run a simple mixed-effect model & save the summary so we can get the t-table
        # mod.var <- nlme::lme(RESP ~ PRED, random=list(TreeID=~1, CoreID=~1), data=dat.tmp[dat.tmp$CoreID!="608012B",], na.action=na.omit)
        
        # system.time(
        mod.var <- nlme::lme(RESP ~ PRED, random=list(year=~1, TreeID=~1, CoreID=~1), data=dat.tmp[!is.na(dat.tmp$RESP),], na.action=na.omit)
        # )
        mod.sum <- summary(mod.var)
        
        # Save our t-stat & pvalue for the climate predictor
        out.ind <- which(mod.out$PlotID==plt & mod.out$pred==VAR & mod.out$yday==i & mod.out$resp==RESP)
        mod.out[out.ind, "t.stat"] <- mod.sum$tTable["PRED","t-value"]
        mod.out[out.ind, "p.val"] <- mod.sum$tTable["PRED","p-value"]
        mod.out[out.ind, "r.sq.m"] <- MuMIn::r.squaredGLMM(mod.var)[,"R2m"]
      }
    }
  }
}
summary(mod.out)
summary(mod.out[mod.out$PlotID=="QUBI-W",])
write.csv(mod.out, file.path(path.google, "data/CriticalPeriods", "ClimateCorrs_Daily.csv"), row.names=F)  

# ----------------------------------
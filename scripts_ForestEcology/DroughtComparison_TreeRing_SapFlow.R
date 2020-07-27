library(ggplot2)

path.google <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/"

dat.tr <- read.csv(file.path(path.google, "data/Drought_Response", "DroughtResponse_LME_stats_PlotID_eff.csv"), stringsAsFactors = T)
summary(dat.tr)

dat.root <- read.csv(file.path(path.google, "data/Newton_ESA_2020/esa2020_condensed.csv"), stringsAsFactors = T)
summary(dat.root)

dat.all <- merge(dat.tr, dat.root, by.x="PlotID", by.y="plot", all.x=T)
dat.all[dat.all$PlotID %in% c("PIST-E", "PIST-W"), c("reduction", "biomass", "length", "srl")] <- dat.root[dat.root$plot=="PIST-A",c("reduction", "biomass", "length", "srl")]
summary(dat.all)


srl.red <- ggplot(data=dat.all[dat.all$factor=="slope",], aes(x=srl,y=reduction)) +
  ggtitle("Root Data: Sapflow ~ SRL") +
  geom_smooth(method="lm") +
  geom_text(aes(label=PlotID)) + 
  scale_x_continuous(expand=c(0.15,0.15)) +
  labs(x="SRL", y="Sap Flow Reduction") +
  theme_bw()


dendro.sap <- ggplot(data=dat.all[dat.all$factor=="slope",], aes(x=reduction, y=Value)) +
  ggtitle("Comparison: DendroSlope ~ SapFlow Reduction") +
  geom_smooth(method="lm") +
  geom_text(aes(label=PlotID)) + 
  labs(y="Tree Ring PDSI Slope", x="Sap Flow Reduction") +
  scale_x_continuous(expand=c(0.15,0.15)) +
  theme_bw()

dendro.srl <- ggplot(data=dat.all[dat.all$factor=="slope",], aes(x=srl,y=Value)) +
  ggtitle("Comparison: DendroSlope ~ SRL") +
  geom_smooth(method="lm") +
  geom_text(aes(label=PlotID)) + 
  scale_x_continuous(expand=c(0.15,0.15)) +
  labs(y="Tree Ring PDSI Slope", x="SRL") +
  theme_bw()

dendro.bm <- ggplot(data=dat.all[dat.all$factor=="slope",], aes(x=biomass, y=Value)) +
  ggtitle("Comparison: DendroSlope ~ Root Biomass") +
  geom_smooth(method="lm") +
  geom_text(aes(label=PlotID)) + 
  scale_x_continuous(expand=c(0.15,0.15)) +
  labs(y="Tree Ring PDSI Slope", x="root biomass") +
  theme_bw()

dendro.length <- ggplot(data=dat.all[dat.all$factor=="slope",], aes(x=length, y=Value)) +
  ggtitle("Comparison: DendroSlope ~ Root Length") +
  geom_smooth(method="lm") +
  geom_text(aes(label=PlotID)) + 
  scale_x_continuous(expand=c(0.15,0.15)) +
  labs(y="Tree Ring PDSI Slope", x="root length") +
  theme_bw()


library(cowplot)
png(file.path(path.google, "figures/Roots_v_TreeRingDrought.png"), height=11, width=8, units="in", res=180)
plot_grid(srl.red, dendro.sap, dendro.srl, dendro.bm, dendro.length, ncol=2)
dev.off()

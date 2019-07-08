# Combining ring width lists for crossdating;
# NOTE: need to install dplR if don't already have it
# library(dplR)

# -------------
# Some quick general things we'll reference later
# -------------
library(ggplot2) # Sometimes I call packages with ::, but I find that's a pain with ggplot 

path.dat <- "/Volumes/GoogleDrive/My Drive/Forestry Plots (1)/Rollinson_2019_REU_ForestryPlots/data/"
# -------------

# -------------
# Reading in the data and doing some quick stats
# -------------
PLOT <- "TSCA-W2"

fplot <- dir(file.path(path.dat, "RingsWidths_Raw", PLOT), ".rwl")

for(i in 1:length(fplot)){
  # need to read in a file and rename it; i=1
  nm.new <- strsplit(fplot[i], "-")[[1]]
  nm.new <- paste0(nm.new[3:5], collapse="")
  
  fnow <- dplR::read.tucson(file.path(path.dat, "RingsWidths_Raw", PLOT, fplot[i]))
  names(fnow) <- nm.new
  
  if(i==1){
    rwl.all <- fnow
  } else {
    rwl.all <- dplR::combine.rwl(rwl.all, fnow)
  }
}
summary(rwl.all)

dplR::write.rwl(rwl.all, file.path(path.dat, "RingWidths_Combined_Undated", paste0(PLOT,".rwl")), long.names=T)

# xdate1 <- dplR::corr.rwl.seg(rwl.all, seg.length=10)

# -------------

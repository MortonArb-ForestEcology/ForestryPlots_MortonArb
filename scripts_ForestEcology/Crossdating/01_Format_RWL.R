# Combining ring width lists for crossdating;
# NOTE: need to install dplR if don't already have it
# library(dplR)

# -------------
# Some quick general things we'll reference later
# -------------
# library(ggplot2) # Sometimes I call packages with ::, but I find that's a pain with ggplot 

# -------------

# -------------
# Reading in the data and doing some quick stats
# -------------
## Hard-coding approach: simpler code, but have to change more parts manually
## Change this to the path of where the data you're working with right now is.
# path.raw <- "/Volumes/GoogleDrive/My Drive/Forestry Plots (1)/Rollinson_2019_REU_ForestryPlots/data/RingsWidths_Raw/PIST-E/"
# fplot <- dir(path.raw, ".rwl")

# Soft-coding approach = better more flexible code, but easier to break if you don't understand what's going on
PLOT <- "PLOC-W" #
path.dat <- "/Volumes/GoogleDrive/My Drive/Forestry Plots (1)/Rollinson_2019_REU_ForestryPlots/data/"
fplot <- dir(file.path(path.dat, "RingsWidths_Raw/preliminary", PLOT), ".rwl")

for(i in 1:length(fplot)){
  # need to read in a file and rename it; i=1
  nm.new <- strsplit(fplot[i], "-")[[1]]
  nm.new <- paste0(nm.new[3:5], collapse="")
  
  fnow <- dplR::read.tucson(file.path(path.dat, "RingsWidths_Raw/preliminary", PLOT, fplot[i]))
  names(fnow) <- nm.new
  
  if(i==1){
    rwl.all <- fnow
  } else {
    rwl.all <- dplR::combine.rwl(rwl.all, fnow)
  }
}
summary(rwl.all)


# Where you want to save the file
path.out <- file.path("COFECHA/", PLOT)
if(!dir.exists(path.out)) dir.create(path.out)
# What you want the name of the file to be
# file.prefix <- "TEST"
file.prefix <- paste(PLOT, Sys.Date(), sep="_")

iter = 1
while(file.exists(file.path(path.out, paste0(file.prefix, ".rwl")))){
  iter=iter+1
  file.prefix <- paste(PLOT, Sys.Date(), iter, sep="_")
}

dplR::write.rwl(rwl.all, file.path(path.out, paste0(file.prefix, ".rwl")), long.names=T)

xdate1 <- dplR::corr.rwl.seg(rwl.all, seg.length=10)

# -------------

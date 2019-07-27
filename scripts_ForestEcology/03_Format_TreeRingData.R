# --------------------------------------------------------
# Collate and format tree-ring data
# --------------------------------------------------------
# What we need to do:
# 1. Read in all of the ring widths 
#    - detrend ring widths in this section to produce RWI
#      - probably just going to use a splien for now.
# 2. Merge ring width data with core metadata so 
#    we can keep track of whether it's been crossdated 
#    or not &/or whether there are any other important 
#    notes.
# 3. Merge ring width data with tree data so we can 
#    calculate growth etc.
# 4. Reformat into a 'long' data format (data frame) 
#    that tends to be easier to work with in analyses etc.
#     - note: some standard tree-ring analyses may not use
#       this data frame becuase they are set up to work
#       with ring widht lists
# 5. Save data frame as a .csv so everyone can work with it
# --------------------------------------------------------

# --------------------------------------------------------
# Doing Everythign
# --------------------------------------------------------
# Establish a connection with the googlesheet file
dat.proj <- googlesheets::gs_title("ForestryPlots_ForestEco_Master")

# Pull & format our tree data
dat.tree <- data.frame(googlesheets::gs_read(dat.proj, ws="TreeData"))
dat.tree$ForestryPlot <- as.factor(dat.tree$ForestryPlot)
dat.tree$SubPlot <- as.factor(dat.tree$SubPlot)
dat.tree$TreeID <- as.factor(dat.tree$TreeID)
dat.tree$TreeID2 <- as.factor(paste(dat.tree$SubPlot, dat.tree$TreeID, sep=""))
dat.tree <- dat.tree[!is.na(dat.tree$ForestryPlot),]
summary(dat.tree)

# File path to where the raw tree-ring data is stored
path.rwl <- "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/data/RingsWidths_Raw/crossdated/"

# Get a list of the files (plots) we have available
files.rwl <- dir(path.rwl, ".rwl")

# Loop through each file and do some stuff to it
dat.all <- data.frame() # Set up a empty data frame to store everything in
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
  
  # reverse the orders of our raw & detrended stuff so present is on top 
  # (easier for my sanity)
  dat.rwl <- dat.rwl[nrow(dat.rwl):1,]
  dat.detrend <- dat.detrend[nrow(dat.detrend):1,]
  
  # -------------
  # Using a loop to calculate growth etc. 
  # NOTE: This is not the most efficient way to do this and I could figure out something
  # using apply(), BUT our dataset isn't THAT big, so it's tractable at the moment.  If 
  # you start adapting this to bigger datasets, this might be an important consideration
  # -------------
  # If we don't have a ring widht for 2019, assume it's because it hasn't put on growth
  # NOTE: This can get changed once we have the core metadata so we can know whether it 
  #       didn't grow or it we're missing the outside
  dat.rwl[1,] <- ifelse(is.na(dat.rwl[1,]), 0, dat.rwl[1,])
  
  # create a new data frame that will reconstruct diameter
  dat.bai <- dat.ba <- dat.dbh <- dat.rwl # make a new data frame the same as dat.rwl so we get dimnames etc right
  dat.bai[,] <- dat.ba[,] <- dat.dbh[,] <- NA # Make everything blank to make things easier
  for(j in 1:ncol(dat.dbh)){
    # Insert the DBH at time of sampling here & calculate the curent basal area
    treeid <- substr(names(dat.dbh)[j],1,6)
    if(!treeid %in% dat.tree$TreeID2){
      warning(paste("Tree", treeid, "in plot", PLT, "does not exist! Skipping", sep=" "))
      next
    }
    dat.dbh[1,j] <- dat.tree[dat.tree$TreeID2==treeid,"DBH"]
    dat.ba[1,j] <- pi*(dat.dbh[1,j]/2)^2
    
    # Calculating DBH as we go backwards in time
    # Note: we're calculating DBH at the end of that year, 
    #  so we subtract the previous year's radial increment 
    #  times 2 to convert it to diamter; also multiply by 0.1
    #  so that it is in cm, which is what we measure DBH in
    for(i in 1:(nrow(dat.dbh)-1)){
      if(is.na(dat.rwl[i,j])) next
      
      # Calculate the DBH & BA for the next time step
      dat.dbh[i+1,j] <- dat.dbh[i,j] - dat.rwl[i,j]*0.1*2
      dat.ba[i+1, j] <- pi*(dat.dbh[i+1,j]/2)^2
      
      # Basal Area Increment 
      dat.bai[i+1,j] <- dat.ba[i,j] - dat.ba[i+1,j]
    }
    
    # Sometimes due to ecentric growh, we end up with negative DBH or growth
    # increments. This is not biologically possible.  In these instances, we
    # need to use the ring widths to work from the inside out rather than 
    # outside-in.
    
    # Find trees that have negative DBH and work from inside out on those
    if(any(c(dat.dbh[!is.na(dat.dbh[,j]),j], dat.bai[!is.na(dat.bai[,j]),j])<0)){
      dat.dbh[,j] <- dat.ba[,j] <- dat.bai[,j] <- NA # Clear things out and start over
      for(i in nrow(dat.rwl):1){
        
        if(is.na(dat.rwl[i,j])) next # If there's nothing, skip it
        
        if(i==nrow(dat.rwl) | is.na(dat.rwl[i+1,j])) { # if we have a value, but there's nothing to add to, start here
          dat.dbh[i,j] <- dat.rwl[i,j]*0.1*2
          dat.ba[i,j] <- pi*(dat.dbh[i,j]/2)^2
          dat.bai[i,j] <- dat.ba[i,j]
        } else {
          dat.dbh[i,j] <- dat.dbh[i+1,j] + dat.rwl[i,j]*0.1*2
          dat.ba[i, j] <- pi*(dat.dbh[i,j]/2)^2
          
          # Basal Area Increment 
          dat.bai[i,j] <- dat.ba[i,j] - dat.ba[i+1,j]
        } # End ifelse
      } # end i loop
    } # End negative growth case
  } # End column loop
  # head(dat.dbh); 
  # head(dat.bai)
  # -------------
  
  # -----------  
  
  # -----------  
  # 2. Putting everything into a "long" format data frame
  # -----------  
  dat.df <- stack(dat.rwl)
  names(dat.df) <- c("RW.cm", "CoreID")
  dat.df$year <- as.numeric(row.names(dat.rwl))
  dat.df$PlotID <- as.factor(PLT)
  dat.df$RWI <- stack(dat.detrend)[,1]
  dat.df$DBH.cm <- stack(dat.dbh)[,1]
  dat.df$BA.cm2 <- stack(dat.ba)[,1]
  dat.df$BAI.cm2 <- stack(dat.bai)[,1]
  
  # Re-ordering to make Christy happy
  dat.df <- dat.df[,c("PlotID", "CoreID", "year", "RW.cm", "RWI", "DBH.cm", "BA.cm2", "BAI.cm2")]
  # summary(dat.df)
  
  # Add our new plot into the dat.all data frame where everything will be stored
  # - NOTE: This is not the most efficient way to do this, but it's easiest.  If
  #   we start working with a LOT of data, this will become probihitively slow &
  #   we'll have to do a more efficient, but slightly more complicated approach
  dat.all <- rbind(dat.all, dat.df)
  # -----------  
  
}
summary(dat.all)

# "/Volumes/GoogleDrive/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/data/RingsWidths_Raw/crossdated/"
write.csv(dat.all, file.path(path.rwl, "../..", "Data_TreeRings_compiled_all.csv"), row.names=F)
# --------------------------------------------------------

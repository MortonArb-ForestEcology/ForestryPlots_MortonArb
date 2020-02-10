# Make the Daymet & PRISM data into more user-friendly csv files

site.id <- "MortonArb-VisitorCenter"
site.lat <- 41.813613
site.lon <- -88.071797
start.date <- "1895-01-01"
end.date <- Sys.Date()

path.save.reu <- "G:/My Drive/Forestry Plots/Rollinson_2019_REU_ForestryPlots/data/Meteorology/"
path.save.met <- "G:/My Drive/Arboretum Met Data/"

# ----------------------------
# Daymet: 1980-2018 point extraction for Arb on Google
# - Temporal Resolution: Daily
# - Temporal Extent: 1980-present
# - Spatial Resolution: 1 km
# - Variables: 
#    - tmax = max daily air temperature (K), 
#    - tmin = min daily air temperature (K), 
#    - prcp = precipitation flux (mm/s), 
#    - srad = shortwave radiation (W/m2), 
#    - dayl = day length (sec), 
#    - swe  = snow water equivalent (mm) 
#    - vp   = vapor pressure (Pa), 
# ----------------------------
path.daymet <- "G:/My Drive/Arboretum Met Data/Daymet/MortonArb-VisitorCenter/"
file.daymet <- dir(file.path(path.daymet, "netcdf"), ".nc")

daymet.df <- data.frame() # we're just going to use rbind to append new data; this will be slower than some alternatives, but it's easier for the moment; See PRISM below for the better way to do this
for(i in 1:length(file.daymet)){
  ncT <- ncdf4::nc_open(file.path(path.daymet, "netcdf", file.daymet[i]))
  
  df.tmp <- data.frame(site = site.id,
                       lat = site.lat,
                       lon = site.lon,
                       year = as.numeric(strsplit(file.daymet[i], "[.]")[[1]][2]),
                       yday = ncdf4::ncvar_get(ncT, "time"),
                       tmax.C = ncdf4::ncvar_get(ncT, "maximum_air_temperature")-273.15, 
                       tmin.C = ncdf4::ncvar_get(ncT, "minimum_air_temperature")-273.15,
                       prcp.mm = ncdf4::ncvar_get(ncT, "precipitation_flux")*60*60*24,
                       srad.Wm2 = ncdf4::ncvar_get(ncT, "surface_downwelling_shortwave_flux_in_air"),
                       dayl.sec = ncdf4::ncvar_get(ncT, "day_length"),
                       swe.mm  = ncdf4::ncvar_get(ncT, "liquid_water_content_of_surface_snow"),
                       vp.Pa   = ncdf4::ncvar_get(ncT, "water_vapor_partial_pressure_in_air") 
                       )
  # summary(df.tmp)
  ncdf4::nc_close(ncT)

  daymet.df <- rbind(daymet.df, df.tmp)
  
}
# daymet.df$Date <- as.Date(paste(daymet.df$year, daymet.df$yday, sep="-"), format="%Y-%j")
summary(daymet.df)

write.csv(daymet.df, file.path(path.save.met, "Daymet", "MortonArb-VisitorCenter", paste0("Daymet_MortonArb_", min(daymet.df$year), "-", max(daymet.df$year), ".csv")), row.names=F)
write.csv(daymet.df, file.path(path.save.reu, paste0("Daymet_MortonArb_", min(daymet.df$year), "-", max(daymet.df$year), ".csv")), row.names=F)
# ----------------------------




# ----------------------------
# PRISM: full US rasters stored locally on iMac 
# - Temporal Resolution: monthly
# - Spatial Resolution: 4 km
# - Variables: precip, dewpoint, max/min/mean air temperature, max/min vpd
# ----------------------------
dir.prism <- "~/Desktop/SpatialData/PRISM/monthly"
var.prism <- c("ppt", "tmax", "tmin", "tmean", "vpdmin", "vpdmax")

mos <- stringr::str_pad(1:12, width=2, side="left", pad=0)
yrs.met <- 1895:2018 # Just going ahead and specifying the year range we want

prism.df <- data.frame(site  = rep(site.id, length.out=length(yrs.met)*length(mos)),
                       lat   = rep(site.lat, length.out=length(yrs.met)*length(mos)),
                       lon   = rep(site.lon, length.out=length(yrs.met)*length(mos)),
                       year  = rep(yrs.met, each=length(mos)),
                       month = rep(mos, length.out=length(yrs.met)))
summary(prism.df)
# tail(prism.df)

# Making our location a spatial file so we can use built-in raster functions
site.loc <- sp::SpatialPoints(coords=data.frame(site.lon, site.lat), proj4string=sp::CRS("+proj=longlat"))

pb <- txtProgressBar(min=0, max=length(var.prism)*length(yrs.met)*length(mos), style=3)
pb.ind=0
for(VAR in var.prism){
  prism.df[,VAR] <- NA # Initialize with an empty vector
  # yrs.met <- dir(file.path(dir.prism, VAR)) Use t
  # 
  for(YR in yrs.met){
    for(MO in mos){
      setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
      
      ind.now <- which(prism.df$year==YR & prism.df$month==MO)
      ftry <- paste("PRISM", VAR, "stable_4kmM1", paste0(YR, MO), "bil.bil", sep="_")
      
      # See if we're using M1, M2, or M3
      if(!file.exists(file.path(dir.prism, VAR, YR, ftry))) ftry <- paste("PRISM", VAR, "stable_4kmM2", paste0(YR, MO), "bil.bil", sep="_")
      if(!file.exists(file.path(dir.prism, VAR, YR, ftry))) ftry <- paste("PRISM", VAR, "stable_4kmM3", paste0(YR, MO), "bil.bil", sep="_")
      
      # If we don't have a stable M2, then just skip
      if(!file.exists(file.path(dir.prism, VAR, YR, ftry))){ print(warning("No stable PRISM file for: ", paste(VAR, YR, MO))); next }
      
      fnow <- raster::raster(file.path(dir.prism, VAR, YR, ftry))
      
      prism.df[ind.now,VAR] <- raster::extract(fnow, site.loc)
    }
  }
}
# prism.df[,c("vpdmax", "vpdmin")] <- prism.df[,c("vpdmax", "vpdmin")]*100
names(prism.df) <- c("site", "lat", "lon", "year", "month", "prcp.mm", "tmax.C", "tmin.C", "tmean.C", "vpmin.hPa", "vpdmax.hPa")
summary(prism.df)


write.csv(prism.df, file.path(path.save.met, "PRISM", paste0("PRISM_monthly_MortonArb_", min(prism.df$year), "-", max(prism.df$year), ".csv")), row.names=F)
write.csv(prism.df, file.path(path.save.reu, paste0("PRISM_monthly_MortonArb_", min(prism.df$year), "-", max(prism.df$year), ".csv")), row.names=F)
# ----------------------------


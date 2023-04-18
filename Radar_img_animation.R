# MMD RADAR DATA PROCESSING (PNG)

# load packages
library(raster)

#set working directory
setwd('J:/Backup_main/2023/20220705_Banjir_Baling/Data/MMD/WSDS Bayan Lepas/CAPPI/04072022')

#get all png in working dir
filename_list <- list.files(path = getwd(), pattern='png$', full.names=TRUE)

#read png as rasterstack (for multiband)
##if use raster to read, will only read first band
all_raster_list <- lapply(filename_list, stack)


#call single raster element
all_raster_list[[1]]

#to run a function on an individual raster e.g., plot 
plot(all_raster_list[[1]])


# RASTER INFO

#view coordinate reference system
all_raster_list[[1]]@crs

#view raster extent
all_raster_list[[1]]@extent

#check image resolution
res(all_raster_list[[1]])


##########
# GEOREFERENCE PNG

# plot image
plotRGB(all_raster_list[[1]])


# for single raster in rasterstack
## set image extent
extent(all_raster_list[[1]]) <- c(99.3654, 101.70433, 4.39238, 6.201997)
## define projection
raster::crs(all_raster_list[[1]]) <- "EPSG:4326"


# define projection and set extent for all raster in rasterstack
for (i in 1:length(all_raster_list)) {
  raster::crs(all_raster_list[[i]]) <- "EPSG:4326"
  extent(all_raster_list[[i]]) <- c(99.3654, 101.70433, 4.39238, 6.201997)
}


#checking
all_raster_list[[1]]@crs
all_raster_list[[1]]@extent



##########
# LOAD SHAPEFILES

library(tidyverse)
library(rgdal) # read shapefile

# map country and coordinate data

## shapefile
basin_shp <- readOGR("J:/Backup_main/2023/20220705_Banjir_Baling/GIS/shp/Kupang_basin3.shp",
                     stringsAsFactors = F) #EPSG3375 Kertau RSO m

pm_shp <- readOGR("J:/Backup_main/GIS_data/Boundary/state/state_pmsia_short.shp",
                  stringsAsFactors = F)

sel_shp <- subset(pm_shp, STATE %in% c("Kedah", "Perak", "Pulau Pinang"))


crs(sel_shp)

crs(basin_shp)

### change projection to WGS84 (original Kertau)
#basin_shp2 <-spTransform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

sel_shp2 <-spTransform(sel_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
basin_shp2 <-spTransform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

crs(sel_shp2)
crs(basin_shp2)


# layout map

map <- ggplot() + 
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group), 
               fill = "grey", alpha = 0.3, colour = "red") +
  geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group), 
               fill = "grey", alpha = 0.3, colour = "black") +
  coord_map(xlim=c(100.1, 101.2), ylim=c(5.2, 5.8)) +
  theme_void()

map

##########
# PLOT RASTER (base)
#try with single plot


#convert to df to use geom_raster
df_try <- as.data.frame(all_raster_list[[1]], xy = TRUE, na.rm = TRUE)

df_try_name <- strsplit(names(all_raster_list[[1]]), split = "dBZ")[[1]][1]
df_try_name <- strsplit(df_try_name, split = "\\.")[[1]][1]

str(df_try)

#rename bands to RGB
colnames(df_try)[3:5] <- c("Red", "Green", "Blue")


#drop data without rgb info
#df_try <- df_try %>% 
#  filter(Red != 0)   


p <- ggplot() + 
  geom_raster(data = df_try, aes(x = x, y = y),
    fill = rgb(r = df_try$Red,
               g = df_try$Green,
               b = df_try$Blue,
               maxColorValue = 255),
              show.legend = FALSE) +
  scale_fill_identity() + 
  geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group), 
               fill = NA, alpha = 0.3, colour = "white", size = 1, linetype = 2) +
  geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group), 
               fill = NA, alpha = 0.3, colour = "red", size = 1) +
  coord_fixed(xlim=c(100.1, 101.6), ylim=c(5.2, 6.3)) +
  #labs(title = date) +
  #theme(plot.background = element_rect(fill = "black")) +
  theme_void()

p



##########
# CREATE LIST OF ALL MAPS

maplist <- list()

j = 1

for (j in 1:length(all_raster_list)) {
  
  #date <- df_date[j,]
  
  # show points for each day
  #sel_pt <- RF_data_day_stn %>% 
  #  filter(Date == date)
  
  df_raster <- as.data.frame(all_raster_list[[j]], xy = TRUE, na.rm = TRUE)
  
  #rename bands to RGB
  colnames(df_raster)[3:5] <- c("Red", "Green", "Blue")
  
  # plot map
  q <- ggplot() + 
    geom_raster(data = df_raster, aes(x = x, y = y),
                fill = rgb(r = df_raster$Red,
                           g = df_raster$Green,
                           b = df_raster$Blue,
                           maxColorValue = 255),
                show.legend = FALSE) +
    scale_fill_identity() + 
    geom_polygon(data = sel_shp2, aes(x = long, y = lat, group = group), 
                 fill = NA, alpha = 0.3, colour = "white", size = 1, linetype = 2) +
    geom_polygon(data = basin_shp2, aes(x = long, y = lat, group = group), 
                 fill = NA, alpha = 0.3, colour = "red", size = 1) +
    coord_fixed(xlim = c(100.1, 101.6), ylim = c(5.2, 6.3)) +
    theme_void()
  

  
  maplist[[j]] <- q
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


#if cannot plot, try
dev.off()
plot(rnorm(50), rnorm(50))


#########################
# ANIMATION

library(animation)

a = 1

saveGIF({
  
  #for (a in 1:5){
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 500)


##########
# SAVE RASTERSTACK FILE

##cannot save rasterstack list

#writeRaster(all_raster_list,"WSDS_BayanLepas_20220704_Kupang.tif", format="GTiff",
#            progress='text', option = c('COMPRESS = LWZ'))

#stackSave(all_raster_list,"WSDS_BayanLepas_20220704_Kupang.tif")


# MMD RADAR DATA PROCESSING (PNG)

# load packages
library(raster)


#set working directory
setwd('D:/gyc/2026/20251208_Forensik_banjir/Data/MMD/KLIA_04122025/IMEJ RADAR')

#get all png in working dir
filename_list <- list.files(path = getwd(), pattern='png$', full.names=TRUE)

#read png as rasterstack (for multiband)
##if use raster to read, will only read first band
all_raster_list <- lapply(filename_list, stack)
## flip the rasters if they are upside down
all_raster_list <- lapply(all_raster_list, flip)


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
extent(all_raster_list[[1]]) <- c(100.6106735324176356, 103.3379512513572536, 1.7728681443627687, 3.9231703615482667)
## define projection
raster::crs(all_raster_list[[1]]) <- "EPSG:4326"


# define projection and set extent for all raster in rasterstack
for (i in 1:length(all_raster_list)) {
  raster::crs(all_raster_list[[i]]) <- "EPSG:4326"
  extent(all_raster_list[[i]]) <- c(100.6106735324176356, 103.3379512513572536, 1.7728681443627687, 3.9231703615482667)
}


#checking
all_raster_list[[1]]@crs
all_raster_list[[1]]@extent



##########
# LOAD SHAPEFILES

library(tidyverse)
#library(rgdal) # read shapefile, retired in 2023
library(sf) # replace rgdal

# map country and coordinate data

## shapefile
#EPSG3375 GDM 2000 Peninsular
basin_shp <- read_sf("D:/GIS_data/Malaysia/JPS_PortalGIS/LEMBANGAN_SEMENANJUNG_dis.shp")

#EPSG3375 GDM 2000 Peninsular
pm_shp <- read_sf("D:/GIS_data/JUPEM_topo/PM/Demarcation/DA0040_State_Cover_A_dis2.shp")

#sel_shp <- subset(pm_shp, STATE %in% c("SELANGOR", "KUALA LUMPUR", "PUTRAJAYA"))
sel_shp <- pm_shp


crs(sel_shp)

crs(basin_shp)


### change projection to WGS84 (original GDM2000)

sel_shp2 <- st_transform(sel_shp, crs = 4326) #WGS84 = EPSG4326
basin_shp2 <- st_transform(basin_shp, crs = 4326) #WGS84 = EPSG4326

crs(sel_shp2)
crs(basin_shp2)


# layout map

map <- ggplot() + 
  geom_sf(data = sel_shp2, fill = "grey", alpha = 0.3, colour = "red") +
  geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "black") +
  coord_sf(xlim=c(100.7, 102.1), ylim=c(2.5, 3.9)) +
  theme_void()

map


##########
# PLOT RASTER (base)
#try with single plot


#convert to df to use geom_raster
df_try <- as.data.frame(all_raster_list[[1]], xy = TRUE, na.rm = TRUE)

df_try_name <- strsplit(names(all_raster_list[[1]]), split = "dBZ")[[1]][1]
df_try_name <- strsplit(df_try_name, split = "\\.")[[1]][1]

# date sequence
df_time <- seq(as.POSIXct("2025-12-4 0:00"), as.POSIXct("2025-12-4 23:55"), by = "5 min")


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
  geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "white", size = 1) +
  #geom_sf(data = sel_shp2, fill = NA, alpha = 0.3, colour = "black", size = 1, linetype = 2) +
  coord_sf(xlim=c(100.7, 102.1), ylim=c(2.5, 3.9)) +
  labs(title = paste0("WSDS KLIA (CAPPI) 2km: ", df_time[1])) +
  #theme(plot.background = element_rect(fill = "black")) +
  theme_void()

p



##########
# CREATE LIST OF ALL MAPS


# selected rasters (starting from 14:00)
sel_raster_list <- all_raster_list[117:235]


# date sequence
df_time <- seq(as.POSIXct("2025-12-4 14:00"), as.POSIXct("2025-12-4 23:55"), by = "5 min")

# remove missing dates from sequence
# remove_dates <- as.POSIXct(c("2022-7-4 16:35", "2022-7-4 19:35", "2022-7-4 20:35", "2022-7-4 21:40", "2022-7-4 23:45"))
# df_time <- df_time[!df_time %in% remove_dates]


# loop map plotting

maplist <- list()

j = 1

for (j in 1:length(sel_raster_list)) {
  
  #date <- df_date[j,]
  
  # show points for each day
  #sel_pt <- RF_data_day_stn %>% 
  #  filter(Date == date)
  
  df_raster <- as.data.frame(sel_raster_list[[j]], xy = TRUE, na.rm = TRUE)
  
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
    geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "white", size = 1) +
    #geom_sf(data = sel_shp2, fill = NA, alpha = 0.3, colour = "black", size = 1, linetype = 2) +
    coord_sf(xlim=c(100.7, 102.1), ylim=c(2.5, 3.9)) +
    labs(title = paste0("WSDS KLIA (CAPPI) 2km: ", df_time[j])) +
    theme_void()
  

  
  maplist[[j]] <- q
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


maplist[[97]]

#if cannot plot, try
dev.off()
plot(rnorm(50), rnorm(50))


#########################
# ANIMATION

library(animation)

a = 1 #73

saveGIF({
  
  #for (a in 73:120){
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'WSDSKLIA2km_20251204_1400.gif', interval = 0.2, ani.width = 600, ani.height = 500)


##########
# SAVE RASTERSTACK FILE

##cannot save rasterstack list

#writeRaster(all_raster_list,"WSDS_BayanLepas_20220704_Kupang.tif", format="GTiff",
#            progress='text', option = c('COMPRESS = LWZ'))

#stackSave(all_raster_list,"WSDS_BayanLepas_20220704_Kupang.tif")



##########
# SAVE PLOTS IN LIST INDIVIDUALLY


# Save plots to tiff. Makes a separate file for each plot.
save_path <- "D:/gyc/2026/20251208_Forensik_banjir/Analysis/Radar/"



k = 1 # (14:00)

for (k in 1:length(sel_raster_list)) {
  ggsave(maplist[[k]], file = paste0(save_path, "KLIA_2km_20251204_", k, ".png"), scale = 1.5,
         dpi = 300, width = 10, height = 10, units = "cm")
}


##########
# SAVE PLOTS IN LIST AS FACET MAP

library(gridExtra)

# replot selected range only (time as title only)

maplist_sel <- list()

m = 1



for (m in 1:length(sel_raster_list)) { 
  
  date <- format(as.POSIXct(df_time[m]), format = "%H:%M")
  
  # show points for each day
  #sel_pt <- RF_data_day_stn %>% 
  #  filter(Date == date)
  
  df_raster <- as.data.frame(sel_raster_list[[m]], xy = TRUE, na.rm = TRUE)
  
  #rename bands to RGB
  colnames(df_raster)[3:5] <- c("Red", "Green", "Blue")
  
  # plot map
  s <- ggplot() + 
    geom_raster(data = df_raster, aes(x = x, y = y),
                fill = rgb(r = df_raster$Red,
                           g = df_raster$Green,
                           b = df_raster$Blue,
                           maxColorValue = 255),
                show.legend = FALSE) +
    scale_fill_identity() + 
    geom_sf(data = basin_shp2, fill = NA, alpha = 0.2, colour = "white", size = 0.3) +
    #geom_sf(data = sel_shp2, fill = NA, alpha = 0.3, colour = "black", size = 1, linetype = 2) +
    coord_sf(xlim=c(100.7, 102.1), ylim=c(2.5, 3.9)) +
    labs(title = date) +
    theme_void()
  
  
  
  maplist_sel[[m]] <- s
  
  
}



# get legend

mylegend <- ggplot() + 
  geom_raster(data = df_try, aes(x = x, y = y),
              fill = rgb(r = df_try$Red,
                         g = df_try$Green,
                         b = df_try$Blue,
                         maxColorValue = 255),
              show.legend = FALSE) +
  scale_fill_identity() + 
  coord_sf(xlim=c(102.78,103.3), ylim=c(2.55, 3.9)) +
  theme_void()

mylegend



# arrange facet map
facet_map <- grid.arrange(grobs = maplist_sel[2:25], ncol = 6) #14:05-16:00

facet_map2 <- grid.arrange(grobs = maplist_sel[26:49], ncol = 6) #16:05-18:00

facet_map3 <- grid.arrange(grobs = maplist_sel[50:73], ncol = 6) #18:05-20:00

facet_map4 <- grid.arrange(grobs = maplist_sel[74:97], ncol = 6) #20:05-22:00



## title font format
title = grid::textGrob(paste0("MMD WSDS KLIA (CAPPI) 2km 2025-12-04"), 
                       gp = grid::gpar(fontsize = 14))

# arrange whole page
facet_legend_map <- grid.arrange(facet_map4, mylegend, 
                                 top = title, 
                                 #nrow = 2, heights = c(9, 1)
                                 ncol = 2, widths = c(9, 1)
)



#print last plot to file
ggsave(paste0(save_path, "WSDS_KLIA_2km_20251204_fct2005.png"), facet_legend_map, dpi = 400,
       width = 17, height = 10, units = "in")

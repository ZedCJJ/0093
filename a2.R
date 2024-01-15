library(sf)
library(sp)
library(dplyr)
library(terra)
library(tmap)
library(ggplot2)
library(leaflet)
library(spatstat)
library(gridExtra)
library(raster)
setwd("E:/UCL/Space/ASSI/A2/")
idn=read_sf("metadata-idn_admin123boundaries_tabulardata-xlsx.csv")
plants_planed=read_sf('power plant Indonesia.xlsx')
plot(idn$geometry)
plot(plants_planed)
library(sf)
library(ggplot2)
library(readxl)
library(raster)
library(rgdal)
library(terra)
indonesia_boundary <- st_read("idn_admbnda_adm0_bps_20200401.shp")
if (is.na(st_crs(indonesia_boundary))) {
  indonesia_boundary <- st_set_crs(indonesia_boundary, 4326)
}
power_plant_data <- read_excel("power plant Indonesia.xlsx")
power_plant_data_clean <- power_plant_data[!is.na(power_plant_data$longitude) & !is.na(power_plant_data$latitude), ]
power_plant_data_sf <- st_as_sf(power_plant_data_clean, coords = c("longitude", "latitude"), crs = 4326)
if (st_crs(indonesia_boundary) != st_crs(power_plant_data_sf)) {
  indonesia_boundary <- st_transform(indonesia_boundary, st_crs(power_plant_data_sf))
}

library(ncdf4) #library to read and process netcdf data
ssrd <- nc_open("ssrd.nc" )
lon <- ncvar_get(ssrd, "longitude")
lat <- ncvar_get(ssrd, "latitude")
time <- ncvar_get(ssrd, "time")
time
library(chron) #deal with chronological objects
tunits <- ncatt_get(ssrd,"time","units") #tunits <- ncatt_get(era,"longitude","units")
#convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ") #strsplit: split the element of character vector. we can convert  "hours since 1900-01-01" to "hours"      "since"      "1900-01-01"
tdstr <- strsplit(unlist(tustr)[3], "-") #convert "1900-01-01" to "1900" "01"   "01"
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

chron(time/24, origin=c(tmonth, tday, tyear) ) #this function is of great help. It can convert the hours since format to the format we are quite familiar with.

#get Variables :u10,v10, ssrd. similar to get dimension data, we are going to use the same method, ncvar_get()
ssrd_array <- ncvar_get(ssrd,"ssrd") #get the Surface solar radiation downwards
dim(ssrd_array) #dimension is 501 * 186 *8. Think about the Figure 1. The reason why it is called array is it is composed of 8 slices 

dlname <- ncatt_get(ssrd,"ssrd","long_name")
dunits <- ncatt_get(ssrd,"ssrd","units")
fillvalue <- ncatt_get(ssrd,"ssrd","_FillValue")

library(lattice)
library(RColorBrewer)


#Get a single time slice of the data using ssrd_array
ssrd_slice <- ssrd_array[,,1,2] 
#The ssrd_slice is actually a matrix. class(ssrd_slice)
# What does 2 in ssrd_array[,,2] indicate?  What if I want to slice all four time slice for "07/01/19"? 

length(na.omit(as.vector(ssrd_slice))) /length(as.vector(ssrd_slice)) #8.5% are valid
dim(ssrd_slice )
image(ssrd_slice, col=rev(brewer.pal(10,"RdBu")) )
max_rad <- max(ssrd_slice, na.rm=TRUE)
max_rad

lonlat <- as.matrix( (expand.grid(lon, lat))) #lon and lat are what we extracted in step 2.
dim(lonlat) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186

ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)

ssrd_df <- data.frame( cbind( lonlat,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat", "ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 

#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")
tm_shape(ssrd_sf)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")



library(terra)
library(raster)

idnssrd_sp <- raster::as(idnssrd_raster, "SpatialPixelsDataFrame")


library(tmap)
tmap_mode("view")
tm_shape(idnssrd)+
  tm_dots(col="ssrd", style = "quantile", size=.001, palette = "viridis")



ssrd <- nc_open("ssrd.nc" )
ssrd_array <- ncvar_get(ssrd,"ssrd")
ssrd_slice <- ssrd_array[,,1,] 
dim(ssrd_slice )
lonlattime <- as.matrix( (expand.grid(lon, lat, time))) #lon and lat are what we extracted in step 2.
dim(lonlattime) #we now have a matrix that has 93186 rows and 2columns. 93186=501*186
ssrd_vec <- as.vector( ssrd_slice) 
length(ssrd_vec)
ssrd_df <- data.frame( cbind( lonlattime,ssrd_vec  ))
colnames(ssrd_df) <- c("lon", "lat","time" ,"ssrd")
ssrd_df_value <- na.omit (ssrd_df)
head(ssrd_df_value, 3) 

write.csv(ssrd_df_value, "ssrd_df_value.csv", row.names = FALSE)



#Creating a spatial object from a lat/lon table
library(sf)
ssrd_sf<- st_as_sf( ssrd_df_value, coords = c(  "lon", "lat")  ) #convert long and lat to point in simple feature format
#To make it a complete geographical object we assign the WGS84 projection, which has the EPSG code 4326
st_crs(ssrd_sf) <- 4326 
ssrd_sf <- st_transform(ssrd_sf, 4326 )

library(tmap)
tmap_mode("view")


library(gstat)
library(sp)


data <- read.csv("average_ssrd_values.csv")
coordinates(data) <- ~lon+lat
x_min <- min(90)
x_max <- max(145)
y_min <- min(-14)
y_max <- max(12)
step_size <- 0.05  
grid_data <- expand.grid(x = seq(from = x_min, to = x_max, by = step_size),
                         y = seq(from = y_min, to = y_max, by = step_size))
coordinates(grid_data) <- ~x+y
grid_sp <- SpatialPointsDataFrame(grid_data, data.frame(id = row.names(grid_data)))
idw_result <- idw(formula = ssrd ~ 1, data, newdata = grid_sp, idp = 5.0)

idw_result_sf <- st_as_sf(idw_result)
st_crs(idw_result_sf) <- 4326
idw_result_sf <- st_transform(idw_result_sf, st_crs(indonesia_boundary))

clipped_idw <- st_intersection(idw_result_sf, st_union(indonesia_boundary))
st_write(clipped_idw, "clipped_idw_5.0.shp")
clipped_idw=st_read("clipped_idw_5.0.shp")

tmap_mode("plot")
tm_shape(clipped_idw) +
  tm_dots(col = "var1.pred", size = 0.001, palette = "viridis") +
  tm_layout(title = "IDW Interpolation of ssrd")
#-------------------------------------------------
IDN_Protected <- st_read("IDN_Protected_union.shp")
library(terra)
setwd("E:/UCL/Space/ASSI/A2/")
IDN_alt<-rast("IDN_msk_alt.vrt") 
IDN_slope <- terrain(IDN_alt, "slope")
IDN_aspect <- terrain(IDN_alt, "aspect")
IDN_population
plot(IDN_slope)
IDN_cov<-rast("IDN_msk_cov.vrt") 
IDN_roads <- st_read("IDN_roads.shp")
IDN_grid <- st_read("grid.geojson")
pop=rast("pop.vrt") 
IDN_water=rast("waterIndonesia.TIF") 
plot(IDN_Protected)
clipped_idw=st_read("data_1.shp")
plot(IDN_cov)
VIEW(IDN_cov)
View(IDN_cov)
tmap_mode("view")
tm_shape(IDN_cov) +
  tm_raster(style = "cont", palette = "-RdYlBu") +
  tm_layout(legend.show = FALSE)

tm_shape(IDN_Protected) +
  tm_fill(col = "blue", border.col = "black") +
  tm_layout(legend.show = FALSE)

memory.limit(size = 2500)
tm_shape(clipped_idw) +
  tm_dots(col = "blue", border.col = "black") +
  tm_layout(legend.show = FALSE)

#----------------------------------------------------------


st_crs(clipped_idw)
st_crs(IDN_grid)
IDN_grid <- st_transform(IDN_grid, st_crs(clipped_idw))

distances_grid <- st_distance(clipped_idw,IDN_grid)
min_distances_grid <- apply(distances_grid, 1, min)
clipped_idw$to_nearest_road <- min_distances
clipped_idw$to_nearest_grid <- min_distances_grid
class(clipped_idw)

distances_2 <- st_distance(clipped_idw,IDN_roads)
min_distances <- apply(distances, 1, min)
clipped_idw$to_nearest_road <- min_distances

#-------------------------------
plot(IDN_population)

library(sf)
library(dplyr)
clipped_idw$var1_vr <- NULL

sp_population <- as(clipped_idw, "Spatial")
sp_population_vect <- vect(sp_population)
p <- extract(pop, sp_population_vect)
clipped_idw$p<- p
clipped_idw$p$ID <- NULL
clipped_idw$p <- extract(pop, sp_population_vect)[, "idn_msk_pop"]

#加入slope
s <- extract(IDN_slope, sp_population_vect)
clipped_idw$s<- slope
clipped_idw$s$ID <- NULL
clipped_idw$s <- extract(IDN_slope, sp_population_vect)[, "slope"]

#加入aspect
a <- extract(IDN_aspect, sp_population_vect)
clipped_idw$a<- a
clipped_idw$a$ID <- NULL
clipped_idw$a <- extract(IDN_aspect, sp_population_vect)[, "aspect"]

#加入land
land <- extract(IDN_cov, sp_population_vect)
clipped_idw$land<- land
clipped_idw$land$ID <- NULL
clipped_idw$land <- extract(IDN_cov, sp_population_vect)[, "IDN_msk_cov"]

#加入IDW5
  clipped_idw_raster <- rast(clipped_idw_non_protected)
  sp_population <- as(clipped_idw_non_protected, "Spatial")
  sp_population_vect <- vect(sp_population)
  values(clipped_idw_raster) <- clipped_idw$var1_pred
  pre <- extract(clipped_idw_raster, sp_population_vect)
  clipped_idw_non_protected$predicted<- pre
  clipped_idw_non_protected$predicted$ID <- NULL
  clipped_idw_non_protected$vr1_prd <- NULL
  st_write(clipped_idw_non_protected, "data_12.shp")
  clipped=st_read("data_12.shp")

#删除有缺失值的row
clipped_idw_non_protected <- na.omit(clipped_idw_non_protected)
st_write(clipped_idw_clean, "data_7.shp")
st_write(clipped_idw, "data_5.shp")


#合并保护区
IDN_Protected_0 <- st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp_0/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
IDN_Protected_1 <- st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp_1/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")
IDN_Protected_2 <- st_read("WDPA_WDOECM_Jan2024_Public_IDN_shp_2/WDPA_WDOECM_Jan2024_Public_IDN_shp-polygons.shp")

IDN_Protected_combined <- rbind(IDN_Protected_0, IDN_Protected_1, IDN_Protected_2)
IDN_Protected_union <- st_union(IDN_Protected_combined)
IDN_Protected_union_sf <- st_as_sf(IDN_Protected_union)
st_write(IDN_Protected_union_sf, "IDN_Protected_union.shp")

plot(IDN_Protected_union_sf)
tm_shape(IDN_Protected) +
  tm_fill(col = "blue", border.col = "black") +
  tm_layout(legend.show = FALSE)





#1从印尼地图挖出保护区
st_crs(indonesia_boundary) <- st_crs(4326) 
IDN_Protected <- st_transform(IDN_Protected, st_crs(indonesia_boundary))
indonesia_boundary <- st_make_valid(indonesia_boundary)
IDN_Protected <- st_make_valid(IDN_Protected)
indonesia_boundary_geom <- st_geometry(indonesia_boundary)
IDN_Protected_geom <- st_geometry(IDN_Protected)
non_protected_areas <- st_difference(indonesia_boundary_geom, IDN_Protected_geom)
non_protected_areas_sf <- st_sf(geometry = non_protected_areas)
tmap_options(check.and.fix = TRUE)

# 图:印尼非保护区
tm_IDN_NON_Pro=tm_shape(non_protected_areas_sf) +
  tm_borders(col = "gray", lwd = 1) +
  tm_fill(col = "#21918c") +
  tm_layout(main.title = "Non protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))
print(tm_IDN_NON_Pro)

# 图:印尼保护区
tm_IDN_Pro=tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "gray") + 
  tm_shape(IDN_Protected) +
  tm_fill(col = "#21918c") + 
  tm_layout(main.title = "Protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))
print(tm_IDN_Pro)


  
#2从C_IDW挖出protected
st_crs(IDN_Protected_union_sf) <- 4326
library(sf)
IDN_Protected_union_sf <- st_make_valid(IDN_Protected_union_sf)
intersections <- st_intersects(clipped_idw_clean,IDN_Protected)
clipped_idw_non_protected <- clipped_idw_clean[lengths(intersections) == 0, ]
library(dplyr)
clipped_idw_non_protected <- clipped_idw_non_protected %>%
  filter(!rowSums(is.na(.)))
st_write(clipped_idw_non_protected, "data_8.shp")
clipped_idw_non_protected=st_read("data_8.shp")

tmap_mode("view")
tm_clipped_out <- tm_shape(indonesia_boundary) +
  tm_borders() +
  tm_shape(clipped_idw_non_protected) +
  tm_dots(size = 0.0005, col = clipped_idw_non_protected$vr1_prd, palette = "magma", style = "quantile")+
  tm_layout(main.title = "IDW non protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))
print(tm_clipped_out)

tmap_mode("view")
tm_clipped_out <- tm_shape(indonesia_boundary) +
  tm_borders() +
  tm_shape(clipped_idw_non_protected) +
  tm_dots(col = "vr1_prd", size = 0.001, palette = "viridis")+
  tm_layout(main.title = "IDW non protected area")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))
print(tm_clipped_out)





#10-foldcv
library(gstat)
library(sp)
library(sf)

data <- read.csv("average_ssrd_values.csv")
indonesia = st_read("idn_admbnda_adm0_bps_20200401.shp")


wind_sf<- st_as_sf( data, coords = c(  "lon", "lat")  )
# 检查 wind_sf 当前的 CRS
print(st_crs(wind_sf))
st_crs(wind_sf) <- 4326

wind_sf = st_transform(wind_sf, 4326)
st_crs(indonesia) <- 4326
indonesia = st_transform(indonesia, st_crs(wind_sf))

coor = as.data.frame(st_coordinates(wind_sf))
wind_sf$x = coor$X
wind_sf$y = coor$Y
wind_nogeom = st_drop_geometry(wind_sf) #get rid of geometry but keep all other attributes
wind_nogeom=na.omit(wind_nogeom)

gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=2)) #data should be in data frame format
gs
st_bbox(indonesia)
#1 degree = 111.1 km, so 0.01 is around 1.11km which is 1110 metre; 0.1 is 11.1km, roughly 11100 metre
raster_template = rast( resolution = 0.05,
                        xmin=95.01079 , ymin=-11.00762  ,xmax=141.01940 , ymax=6.07693  ,  crs = st_crs(indonesia)$wkt)
raster_template #check basic information about the raster template we created
idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred)
idw_mask <- mask(idw, indonesia)
plot(idw_mask$var1.pred)
names(idw_mask) = c( "predicted","observed" )
tmap_mode("view") #tmap can also do raster plot
tm_shape(idw_mask$predicted) + 
  tm_raster(col="predicted", style = "quantile", n = 10, palette= "Blues", legend.show = TRUE)



RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

null <- RMSE(mean(wind_sf$ssrd), wind_sf$ssrd)
null #1.656786 is the baseline. 



n_idp = 20 #examine power ranging from 1 to 20
n_fold =10

rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(7713)
kf <- sample(1:n_fold, nrow(wind_nogeom), replace=TRUE)
va = data.frame( c(1:n_idp), NA)
colnames(va) =c("idp","rmse") 



for (j in 1:n_idp) 
{
  for (i in 1:n_fold) {
    test <- wind_nogeom[kf == 1, ]
    train <- wind_nogeom[kf != 1, ]
    gs <- gstat(formula=ssrd~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$ssrd, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}





va[which(va$rmse==min(va)),]
library(ggplot2)
ggplot(va) +
  geom_point(aes(x = idp, y= rmse))+
  geom_hline(yintercept=min(va), linetype="dashed", color = "red")+
  theme_classic()


par(mfrow = c(2,1)) # 2row by 3 columns
idp_list= c(7:8)
for (i in 7:8){
  gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=i)) #data should be in data frame format
  idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
  plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", i)) 
}


par(mfrow = c(1,1)) # 2row by 3 columns
idp_list= c(7:8)
for (i in 7:8){
  gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=i)) #data should be in data frame format
  idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
  plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", idp_list[i])) 
}

gs <- gstat(formula=ssrd~1, locations=~x+y, data=wind_nogeom, nmax=Inf, set=list(idp=7)) #data should be in data frame format
idw <- interpolate(raster_template, gs, debug.level=0) #interpolate is the function comes with terra
plot(idw$var1.pred, main=paste0("IDW interpolation, idp=", 7))



#Q1-------------------------------------------------------
library(ggplot2)
library(sf)
library(ggspatial)
library(readxl)
library(dplyr)
library(ggspatial)
library(tmap)
plant <- read_excel("power plant Indonesia.xlsx")
colnames(plant)
plant <- plant %>%
  filter(!is.na(longitude) & !is.na(latitude))
plant_sf <- st_as_sf(plant, coords = c("longitude", "latitude"), crs = 4326)
ggplot() +
  geom_sf(data = indonesia_boundary, fill = "white", color = "black") +
  geom_sf(data = plant_sf, aes(size = capacity_mw, color = status), alpha = 0.7) +
  scale_color_manual(values = c("existing" = "dodgerblue", "construction" = "firebrick", "planned" = "gold")) +
  theme_minimal() +
  labs(title = "Indonesia renewable energy power plants",
       color = "Status") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"))




#Q2------------------------------------------------------
tmap_options(check.and.fix = TRUE)
library(dplyr)
library(sf)

AHP <- clipped_idw_non_protected %>%
  filter(p < 1000)

AHP <- AHP %>%
  filter(!land %in% c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 15, 17, 18, 20, 21, 22))

AHP <- AHP %>%
  filter(t_nrst_r >= 200)

AHP <- AHP %>%
  filter(s < 20)
colnames(AHP)[colnames(AHP) == "predicted"] <- "ssrd_idw"

coordinates <- st_coordinates(AHP)
AHP_coords <- cbind(AHP, lon = coordinates[,1], lat = coordinates[,2])
write.csv(AHP_coords, "AHP.csv", row.names = FALSE)


#------------------------------------------------------------------

IDN_roads <- st_read("IDN_roads.shp")
tmap_options(check.and.fix = TRUE)
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(IDN_roads) +
  tm_lines(col = "darkred") + 
  tm_layout(
    main.title = "Roads",
    legend.position = c("left", 0.17),
    legend.text.size = 0.5,
    legend.title.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDN_grid <- st_read("grid.geojson")
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(IDN_grid) +
  tm_lines(col = "green") +  
  tm_layout(
    main.title = "Grids",
    legend.position = c("left", 0.17),
    legend.text.size = 0.5,
    legend.title.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDW=st_read("clipped_idw_5.0.shp")
IDW$var1_pred <- IDW$var1_pred/14400000
IDW <- IDW %>%
  rename(GW = var1_pred)
tm_shape(IDW) +
  tm_dots(col = "GW", size = 0.001, palette = "viridis") +
  tm_layout(
    main.title = "Capacity(GW)",
    legend.position = c(0.01, 0.13),
    legend.width = 1,           
    legend.text.size = 0.4,       
    legend.title.size = 0.6
  ) +
  tm_compass(type = "arrow", position = c("right", "0.65")) +
  tm_scale_bar(position = c(0.01, 0.01))


IDW=st_read("data_7.shp")
IDW <- IDW %>% rename(To_grid = t_nrst_g)
tmap_mode("plot")
tm_shape(IDW) +
  tm_dots(col = "To_grid", size = 0.001, palette = "viridis", 
          breaks = c(0,1000, 2000, 5000, 10000, 50000, 150000, 200000, 250000)) +
  tm_shape(IDN_grid) +
  tm_lines(col = "darkred") +  
  tm_layout(
    main.title = "Grid and Distance to grid",
    legend.position = c(0.01, 0.13),
    legend.width = 1,           
    legend.text.size = 0.4,       
    legend.title.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "0.7")) +
  tm_scale_bar(position = c(0.01, 0.01))



IDW=st_read("data_7.shp")
IDW <- IDW %>% rename(To_road = t_nrst_r)
tm_shape(clipped_idw) +
  tm_dots(col = "To_road", size = 0.001, palette = "cividis", 
          breaks = c(0,1000, 2000, 5000, 10000, 50000, 150000, 200000, 250000)) +
  tm_shape(IDN_roads) +
  tm_lines(col = "purple") + 
  tm_layout(
    main.title = "Road and Distance to road",
    legend.position = c(0.01, 0.13),
    legend.width = 1,           
    legend.text.size = 0.4,       
    legend.title.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "0.7")) +
  tm_scale_bar(position = c(0.01, 0.01))

color_breaks <- c(0, 100, 500, 1000, 2000,  20000)
color_palette <- colorRampPalette(c("lightgreen","lightblue", "blue", "darkred","red"))(length(color_breaks) - 1)
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(pop) +
  tm_raster(
    palette = color_palette,
    title = "Pop_density",
    breaks = color_breaks ) +
  tm_layout(
    main.title = "Population density per km square",
    legend.position = c("left", 0.16),
    legend.text.size = 0.5,
    legend.title.size = 0.6 ) +
  tm_compass(type = "arrow", position = c("right", "0.65")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDN_cov<-rast("IDN_msk_cov.vrt") 
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") + 
  tm_shape(IDN_cov) +
  tm_raster( palette = "-viridis", title = "Land Cover"  ) +
  tm_layout(main.title = "Land Cover", 
            legend.position = c("left", 0.17), 
            legend.text.size = 0.5, 
            legend.title.size = 0.6)+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

color_vector <- c("Unsuitable" = c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 15, 17,18,20, 21, 22), "Suitable" = c(9, 10, 13, 14, 16, 17, 18, 19))
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(IDN_cov) +
  tm_raster(style = "fixed", breaks = color_vector, palette = c("pink", "blue")) +
  tm_layout(main.title = "Suitability",legend.show = FALSE) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDN_alt<-rast("IDN_msk_alt.vrt") 
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(IDN_alt) +
  tm_raster(palette = "viridis", title = "Altitude") +  
  tm_layout(
    main.title = "Altitude",
    legend.position = c("left", 0.17),
    legend.text.size = 0.5,
    legend.title.size = 0.6 ) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDN_slope <- terrain(IDN_alt, "slope")
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(IDN_slope) +
  tm_raster(palette = "magma", title = "Slope") +  
  tm_layout(
    main.title = "Slope",
    legend.position = c("left", 0.17),
    legend.text.size = 0.5,
    legend.title.size = 0.6
  ) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

colors <- c("pink", "blue") 
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(IDN_slope) +
  tm_raster(style = "fixed", breaks = c(0, 20, 100), palette = colors) +  
  tm_layout(main.title = "Suitability", legend.show = FALSE) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDN_Protected <- st_read("IDN_Protected_union.shp")
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") + 
  tm_shape(IDN_Protected) +
  tm_fill(col = "darkred") + 
  tm_layout(main.title = "Protected Region")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

IDN_Protected <- st_read("IDN_Protected_union.shp")
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") + 
  tm_fill(col = "pink",alpha = 0.7) + 
  tm_shape(IDN_Protected) +
  tm_fill(col = "blue") + 
  tm_layout(main.title = "Suitability")+
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))




colors <- c("pink", "blue")  
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(pop) +
  tm_raster(style = "fixed", breaks = c(0, 1000, 200000), palette = colors) +  
  tm_layout(main.title = "Suitability", legend.show = FALSE) +
  tm_compass(type = "arrow", position = c("right", "top")) +
  tm_scale_bar(position = c(0.01, 0.01))

tm_shape(AHP) +
  tm_dots(col = "ssrd_idw", size = 0.001, palette = "viridis") +
  tm_layout(
    main.title = "Final ssrd_idw points",
    legend.position = c(0.01, 0.13),
    legend.width = 1,           
    legend.text.size = 0.4,       
    legend.title.size = 0.6
  ) +
  tm_compass(type = "arrow", position = c("right", "0.7")) +
  tm_scale_bar(position = c(0.01, 0.01))


















library(readxl)
setwd("E:/UCL/Space/ASSI/A2/")
score <- read_xlsx("score.xlsx")
score <- st_as_sf(score, coords = c("lon", "lat"), crs = 4326, agr = "constant")


tmap_mode("plot")
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(score) +
  tm_dots(col = "AHP Score", size = 0.01, palette = "viridis") +
  tm_layout(
    main.title = "AHP score",
    legend.position = c(0.01, 0.13),
    legend.width = 1,           
    legend.text.size = 0.4,       
    legend.title.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "0.7")) +
  tm_scale_bar(position = c(0.01, 0.01))


Final <- read_xlsx("FINAL.xlsx")
Final <- st_as_sf(Final, coords = c("lon", "lat"), crs = 4326, agr = "constant")
tm_shape(indonesia_boundary) +
  tm_borders(lwd = 1, col = "black") +
  tm_shape(Final) +
  tm_dots(col = "blue", size = 0.2) +
  tm_shape(IDN_grid) +
  tm_lines(col = "darkred") +  
  tm_layout(
    main.title = "Final plants location and Grid",
    legend.position = c(0.01, 0.13),
    legend.width = 1,           
    legend.text.size = 0.4,       
    legend.title.size = 0.6) +
  tm_compass(type = "arrow", position = c("right", "0.7")) +
  tm_scale_bar(position = c(0.01, 0.01))












annual_revenue <- 6437.2367
i <- 0.05  
lifetime_yrs <- 25
CAPEX <- 40699.2018
OPEX <- 0  


calc_NPV <- function(annual_revenue, i, lifetime_yrs, CAPEX, OPEX) {
  NPV <- -CAPEX  
  for (year in 1:lifetime_yrs) {
    NPV <- NPV + annual_revenue / ((1 + i) ^ year) 
  }
  return(NPV)
}
NPV <- calc_NPV(annual_revenue, i, lifetime_yrs, CAPEX, OPEX)
print(NPV)

Life_span_generation_kWH <- function (yearly_generation_kWH=62497444020, discount = 0.03, lifetime_yrs = 25) {
  t <- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G, 0))
}

LCOE <- function(NPV_cost=40699201800, Life_span_generation_KWH) {
  lcoe <- NPV_cost / Life_span_generation_KWH
  return(round(lcoe, 2))
}

life_span_gen <- Life_span_generation_kWH()
lcoe_result <- LCOE(Life_span_generation_KWH = life_span_gen)

print(lcoe_result)

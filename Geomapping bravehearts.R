pacman::p_load(plotKML, XML, lubridate, raster, maptools, ggplot2, rasterVis, rgdal, rgl, htmlwidgets)

shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


# Parse the GPX file
pfile <- htmlTreeParse('bravehearts.gpx',
                       error = function(...) {}, useInternalNodes = T)
# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = '//trkpt/ele', xmlValue))

times <- xpathSApply(pfile, path = '//trkpt/time', xmlValue)
coords <- xpathSApply(pfile, path = '//trkpt', xmlAttrs)
if(length(elevations) < length(times)){elevations[length(times)] <- NA}
# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords['lat',])
lons <- as.numeric(coords['lon',])
# Put everything in a dataframe and get rid of old variables
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
rm(list=c('elevations', 'lats', 'lons', 'pfile', 'times', 'coords'))
head(geodf)

# Shift vectors for lat and lon so that each row also contains the next position.
geodf$lat.p1 <- shift.vec(geodf$lat, -1)
geodf$lon.p1 <- shift.vec(geodf$lon, -1)

# Calculate distances (in metres) using the function pointDistance from the ‘raster’ package.
# Parameter ‘lonlat’ has to be TRUE!
geodf$dist.to.prev <- apply(geodf, 1, FUN = function (row) {
  pointDistance(c(as.numeric(row['lat.p1']),
                  as.numeric(row['lon.p1'])),
                c(as.numeric(row['lat']), as.numeric(row['lon'])),
                lonlat = T)
})
# Transform the column ‘time’ so that R knows how to interpret it.
geodf$time <- strptime(geodf$time, format = '%Y-%m-%dT%H:%M:%OS')
# Shift the time vector, too.
geodf$time.p1 <- shift.vec(geodf$time, -1)
# Calculate the number of seconds between two positions.
geodf$time.diff.to.prev <- as.numeric(difftime(geodf$time.p1, geodf$time))
# Calculate metres per seconds, kilometres per hour and two LOWESS smoothers to get rid of some noise.
geodf$speed.m.per.sec <- geodf$dist.to.prev / geodf$time.diff.to.prev
geodf$speed.km.per.h <- geodf$speed.m.per.sec * 3.6
geodf$speed.km.per.h <- ifelse(is.na(geodf$speed.km.per.h), 0, geodf$speed.km.per.h)
geodf$lowess.speed <- lowess(geodf$speed.km.per.h, f = 0.05)$y
geodf$lowess.ele <- lowess(geodf$ele, f = 0.05)$y
geodata_all <- geodf
# Transforming the time column
geodata_all$time <- as.character(strptime(geodata_all$time, format = "%Y-%m-%dT%H:%M:%OS"))



#Elevation profile Of the areas
library(maptools)
library(raster)
srtm <- getData("SRTM", lon =  mean(geodata_all$lon)+0.1, lat = mean(geodata_all$lat))
#srtm <- getData("GADM", country = 'deu',lon =  mean(geodata_all$lon), lat = mean(geodata_all$lat))

# crop to area
e2 <- extent(min(geodata_all$lon) - 0.05, # xmin
             max(geodata_all$lon) + 0.05, # xmax
             min(geodata_all$lat) - 0.1, # ymin
             max(geodata_all$lat) + 0.1) # ymax
srtm_c <- crop(srtm, e2)

slope <- terrain(srtm_c, opt = "slope")
aspect <- terrain(srtm_c, opt = "aspect")
hill <- hillShade(slope, aspect, angle = 45, direction = 45, normalize = TRUE)
#plot(hill, col = grey(0:100/100), legend = FALSE)
#plot(srtm_c, col = rainbow(25, alpha = 0.35), add = TRUE)





#Because the 2D plot doesn’t show the elevation very clearly, I wanted to plot this in 3D
library(scatterplot3d)

# http://gis.stackexchange.com/questions/142156/r-how-to-get-latitudes-and-longitudes-from-a-rasterlayer
#r.pts <- rasterToContour(srtm_c, maxpixels=100000)
r.pts <- rasterToPoints(srtm, spatial = T)
geo.prj <- proj4string(r.pts)
r.pts <- spTransform(r.pts, CRS(geo.prj)) 
bg_matrix <- data.frame(lon = coordinates(r.pts)[,1],
                        lat = coordinates(r.pts)[,2])

ex_bg <- extract(srtm, bg_matrix, cellnumbers = TRUE, df = TRUE)
bg_matrix$ele <- ex_bg[,3]

#ex_points <- extract(srtm, cbind(geodata_all$lon, geodata_all$lat), cellnumbers = TRUE, df = TRUE)


##plotly
geodata_all$time.p1 <- as.POSIXct(geodata_all$time.p1)
library(plotly)
p <- plot_ly(bg_matrix, x = ~lon, y = ~lat, z = ~ele, color = ~ele, colors = terrain.colors(10), alpha = 0.8) %>%
  add_markers() %>% 
  add_trace(data = geodata_all, x = ~lon, y = ~lat, z = ~lowess.ele, mode = "markers", type = "scatter3d", 
            marker = list(size = 5, color = "red", symbol = 104))%>%
  layout(scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Elevation')))
p
system("say Your script is now complete")





#Interactive 3d plot

library(rgdal)
library(rasterVis)
library(rgl)
library(htmlwidgets)
options(rgl.printRglwidget = TRUE)

r <- raster(volcano)
open3d()
plot3D(srtm_c)
decorate3d()
plot3D(r)

points3d(xy)
#Plot Region route
options(rgl.printRglwidget = TRUE)
open3d()
plot3d(bg_matrix[, 1], bg_matrix[, 2], bg_matrix[, 3],#ex_points$srtm_39_02, 
       xlab = "Longitude", ylab = "Latitude", zlab = "Elevation",
       col = 'blue',# add = TRUE, 
       size = 5, alpha = .5,
       # lit = TRUE,
       box = FALSE#, axes = FALSE
)

#Add tracks to it
plot3d(geodata_all$lon, geodata_all$lat, geodata_all$lowess.ele,#ex_points$srtm_39_02,
       xlab = "Longitude", ylab = "ele", zlab = "Latitude",
       col = 'red', add = TRUE,
       size = 5, alpha = .5,
       # lit = TRUE,
       box = FALSE#, axes = FALSE
)



#Add 2D Map Overlay with ggplot

library(ggplot2)
library(ggmap)
map_theme <- list(theme(legend.position = "top",
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_blank(),
                        plot.background = element_rect(fill = "white"),
                        panel.border = element_blank(),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        plot.title = element_text(size = 18)))
map <- get_map(c(lon = mean(geodata_all$lon)-0.003, lat = mean(geodata_all$lat)), zoom = 14, maptype = "satellite", source = "google")

#Plot coordinates against speed / elevation 2D
ggmap(map) + 
  geom_point(data = geodata_all, aes(x = lon, y = lat, color = ele)) +
  scale_colour_gradientn(colours = terrain.colors(10), name = "Elevation") +
  map_theme
remove(list=ls())

library(sf)
library(spdep)

polygons = st_read("./shapefiles/USA_ZIP_Code_Boundaries.shp")
data = readr::read_rds("./pre_processed_moran_data.Rds")

geoms = c()
for (zip in unique(data$Zip)) {
  expanded_zips = c()
  
  new_zip = as.numeric(paste(zip, "00", sep=""))
  for (i in 0:99) {
    if (as.character(new_zip + i) %in% polygons$ZIP_CODE) {
      expanded_zips = append(expanded_zips, new_zip + i)
    }
  }
  
  all_zip_polygons = polygons[polygons$ZIP_CODE %in% expanded_zips,]
  unioned_polygons = st_union(all_zip_polygons)
  
  geoms = append(geoms, unioned_polygons)
}

neighbouring_polygons = poly2nb(geoms, queen=TRUE)
neighbouring_polygon_weights <- nb2listw(neighbouring_polygons, style="W", zero.policy=TRUE)

#~~~~~~~~~~~~~~~~Moran's I~~~~~~~~~~~~~~~~
print(moran.test(data$ResistanceRate, neighbouring_polygon_weights, alternative="greater", zero.policy=TRUE))
mc = moran.mc(data$ResistanceRate, neighbouring_polygon_weights, nsim = 999, alternative = "greater", zero.policy=TRUE)
plot(mc, xlab="Moran's I")

#~~~~~~~~~~~~~~~~Geary's C~~~~~~~~~~~~~~~~
print(geary.test(data$ResistanceRate, neighbouring_polygon_weights, alternative="greater", zero.policy=TRUE))
mc = geary.mc(data$ResistanceRate, neighbouring_polygon_weights, nsim = 999, alternative = "greater", zero.policy=TRUE)
plot(mc, xlab="Geary's C")
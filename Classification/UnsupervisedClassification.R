library(terra)

folder <- "C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite"
date <- "20240807"

# Charge Images
band_files <- list.files(file.path(folder, paste0("S2A_", date, "_ImagesPark\\band")), pattern = "\\.tif$", full.names = TRUE)
raster_list <- lapply(band_files, rast)

# Charge park limits
limitPark <- vect(file.path(folder, "LimitPark.geojson"))
limitPark <- project(limitPark, rast(band_files[[1]][1]))


# Take a reference for resolution
ref_index <- which.min(sapply(raster_list, function(x) max(res(x))))  
raster_ref <- raster_list[[ref_index]]

# apply crop and resample for all band
raster_resampled_list <- lapply(raster_list, function(r) {
  r_resampled <- resample(r, raster_ref, method = "bilinear")  # resample
  r_mask <- mask(r_resampled, limitPark)  # apply mask
  return(r_mask)
})

# Merge raster
stacked_raster <- rast(raster_resampled_list)

# Convert raster into Matrix dataframe (With NA)
raster_df <- as.data.frame(stacked_raster, xy = TRUE, na.rm = FALSE)

# Get valid (no NA) pixels
valid_indices <- !is.na(raster_df[[ref_index + 2]]) # + 2 because there is x and y coordinated

# Get valid data for classification
data_valid <- raster_df[valid_indices, -c(1,2)]  # Exclude columns x et y

# Apply K-means fonction (Classification)
n_clusters <- 10
kmeans_result <- kmeans(data_valid, centers = n_clusters, nstart = 10)

# Create classification's raster
classified_raster <- stacked_raster[[ref_index]]
values(classified_raster) <- NA  # Initialize at NA


# Affect results to valid pixels
classified_raster[which(valid_indices)] <- kmeans_result$cluster

# Plot
plot(classified_raster, main = "Classification non supervisée par K-means")


#Save
writeRaster(classified_raster, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\classified_raster_", date, ".tif"), format = "GTiff", overwrite = TRUE)
writeRaster(stacked_raster, file.path(folder,paste0("rasters_10m_", date, ".tif")), overwrite = TRUE)


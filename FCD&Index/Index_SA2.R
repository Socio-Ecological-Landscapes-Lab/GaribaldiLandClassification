library(terra)


folder <- "C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite"
date <- "20240807"

# Charge Images
band_files <- list.files(file.path(folder, date, paste0("S2A_", date, "_ImagesPark\\band")), pattern = "\\.tif$", full.names = TRUE)
raster_list <- lapply(band_files, rast)

# Charge park limits
limitPark <- vect(file.path(folder, "LimitPark.geojson"))
limitPark <- project(limitPark, rast(band_files[[1]][1]))

# Take a reference for resolution
ref_index <- which.min(sapply(raster_list, function(x) max(res(x))))
raster_ref <- raster_list[[ref_index]]

# Apply crop and resample for all bands
raster_resampled_list <- lapply(raster_list, function(r) {
  r_resampled <- resample(r, raster_ref, method = "near")  # can aslo be average
  r_mask <- mask(r_resampled, limitPark)  # apply mask
  return(r_mask)
})

# Merge raster
stacked_raster <- rast(raster_resampled_list)

# Normalize bands to 0-255
normalize_band <- function(band) {

  band <- band/10000 *255
  band <- terra::clamp(band, low = -Inf, up = 255)
  
  return(band)
}

raster_norm <- normalize_band(stacked_raster)



# Rename bands
for (i in 1:nlyr(raster_norm)) {
  band <- raster_norm[[i]]

  original_name <- names(raster_norm)[i]
  
  band_number <- sub(".*B0*([1-9]\\d*[A]?).*", "\\1", original_name)
  
  
  new_name <- paste0("band", band_number)

  names(band) <- new_name
  assign(new_name, band)
  print(paste("Bande originale:", original_name, "-> Nouveau nom:", new_name))
}


# Calculate NDVI (Normalized Difference Vegetation Index)
ndvi <- (band8 - band4) / (band8 + band4)
names(ndvi) <- "NDVI"
plot(ndvi, main = "NDVI")
writeRaster(ndvi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\ndvi_", date, ".tif"), overwrite = TRUE)

# Calculate NDWI (Normalized Difference Water Index)
band3 <- stacked_raster[[3]]  # Green
ndwi <- (band3 - band8) / (band3 + band8)
names(ndwi) <- "NDWI"
plot(ndwi, main = "NDWI")
writeRaster(ndwi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\ndwi_", date, ".tif"), overwrite = TRUE)

# Calculate NDSI (Normalized Difference Snow Index)
ndsi <- (band3 - band11) / (band3 + band11)
names(ndsi) <- "NDSI"
plot(ndsi, main = "NDSI")
writeRaster(ndsi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\ndsi_", date, ".tif"), overwrite = TRUE)

# Calculate NDBI (Normalized Difference Built-up Index)
ndbi <- (band11 - band8) / (band11 + band8)
names(ndbi) <- "NDBI"
plot(ndbi, main = "NDBI")
writeRaster(ndbi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\ndbi_", date, ".tif"), overwrite = TRUE)

# Calculate NDMI (Normalized Difference Moisture Index)
ndmi <- (band8 - band11) / (band8 + band11)
names(ndmi) <- "NDMI"
plot(ndmi, main = "NDMI")
writeRaster(ndmi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\ndmi_", date, ".tif"), overwrite = TRUE)




# Calculate SI (Shadow Index)
si <- ((256 - band4) * (256 - band3) * (256 - band2))^(1/3)
names(si) <- "SI"
plot(si, main = "Shadow Index (SI)")
writeRaster(si, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\si_", date, ".tif"), overwrite = TRUE)
# Calculate normalized SI
minmaxSI <- minmax(si)
siNorm <- (si - minmaxSI[1])/ (minmaxSI[2] - minmaxSI[1])  # Simplification
plot(siNorm, main = "Shadow Index (SI) Norm")
writeRaster(siNorm, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\siNorm_", date, ".tif"), overwrite = TRUE)

# Calculate AVI (Advanced Vegetation Index)
avi <- ((band8 + 1) * (256 - band4) * (band8 - band4))^(1/3)
avi[band8 < band4] <- 0  # If B4 < B3 after normalization, set AVI to 0
names(avi) <- "AVI"
plot(avi, main = "Advanced Vegetation Index (AVI)")
writeRaster(avi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\avi_", date, ".tif"), overwrite = TRUE)
minmaxAVI <- minmax(avi)
AVINorm <- (avi - minmaxAVI[1])/ (minmaxAVI[2] - minmaxAVI[1])  # Normalisation
plot(AVINorm, main = "Advanced Vegetation Index (AVI) Norm")
writeRaster(AVINorm, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\AVINorm_", date, ".tif"), overwrite = TRUE)


# Calculate Bareness Index
bsi <- (band12 + band4) - (band8 - band2) / (band12 + band4 + band8 + band2)
names(bsi) <- "BSI"
plot(bsi, main = "Bareness Index (BSI)")
writeRaster(bsi, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\bsi_", date, ".tif"), overwrite = TRUE)
minmaxBSI <- minmax(bsi)
BSINorminv <- 1 - (bsi - minmaxBSI[1])/ (minmaxBSI[2] - minmaxBSI[1])  # Normalisation
plot(BSINorminv, main = "Bareness Index (BSI) Norm")
writeRaster(BSINorm, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\BSINorm_", date, ".tif"), overwrite = TRUE)




# Convert SpatRaster to matrix for PCA
avi_matrix <- as.matrix(AVINorm)
bsi_matrix <- as.matrix(BSINorm)


# Prepare data for PCA
# every row is a pixel, every column is a variable (band)
data <- cbind(c(avi_matrix), c(bsi_matrix))

# Strandardize the data
data_standardized <- scale(data)
data_cleaned <- na.omit(data_standardized)

# application of PCA
pca <- prcomp(data_cleaned, center = TRUE, scale. = TRUE)

# Extract the first principal component (PC1) scores
pc1_scores <- as.numeric(pca$x[, 1])

pc1_original <- rep(NA, nrow(data_standardized))

pc1_original[!is.na(data_standardized[, 1])] <- pc1_scores

VD <- data.frame(PC1 = pc1_original)
  
# VD <- AVINorm - BSINorm # Manual combinaison test

# Create a SpatRaster for PC1
VD_raster <- avi * 0 # Create an empty raster with the same dimensions and resolution as avi
VD_raster[[1]] <- VD$PC1  # fill the raster with PC1 scores
names(VD_raster) <- "PC1"  
minmaxVD <- minmax(VD_raster)
VDNorm <- (VD_raster - minmaxVD[1])/ (minmaxVD[2] - minmaxVD[1]) # Normalisation
plot(VDNorm,main="Vegetation Density (VD) Norm")
writeRaster(VDNorm, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\vdNorma1b-1_", date, ".tif"), overwrite = TRUE)



# Calculate FCD (Forest Canopy Density)
FCD <- (VDNorm * siNorm + 1)^(1/2) - 1

plot(FCD, main = "FCD")

# Save FCD raster
writeRaster(FCD, paste0("C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result\\FCDa1b-1_", date, ".tif"), overwrite = TRUE)

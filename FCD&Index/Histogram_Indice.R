library(terra)
library(ggplot2)
library(sf)

# Define the folder and date
folder <- "C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite\\Result"
date <- "20240807"

# List of indices to process
indices <- c("ndvi", "ndwi", "ndsi", "ndbi", "ndmi")


# Construct the file path
file_path <- file.path(folder, paste0(indices[1], "_", date, ".tif"))

# Load the raster
raster <- rast(file_path)

# Extract NDVI values
ndvi_values <- na.omit(as.vector(raster))

# Create a data frame for NDVI values
ndvi_df <- data.frame(NDVI = ndvi_values)

# Create a histogram of NDVI values
ggplot(ndvi_df, aes(x = NDVI)) +
  geom_histogram(binwidth = 0.005, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.1)) +
  labs(title = "Histogramme des valeurs NDVI",
       x = "Valeurs NDVI",
       y = "Nombre de pixels") +
  theme_minimal()


# Define a threshold value for vegetation
threshold_low <-  0.2
threshold_high <- 1

# Apply the threshold to create a binary raster
vegetation_raster <- raster
vegetation_raster[vegetation_raster < threshold_low] <- NA
vegetation_raster[vegetation_raster > threshold_high] <- NA

# Plot the vegetation raster
plot(vegetation_raster, main = sprintf("NDVI with [%.3f, %.3f] Threshold Applied", threshold_low, threshold_high))

  # Save the vegetation raster
writeRaster(vegetation_raster, file.path(folder, paste0("Vegetationall_", date, ".tif")), overwrite = TRUE)


# Create a mask for vegetation
vegetation_mask <- vegetation_raster
vegetation_mask[vegetation_raster < threshold_low | is.na(vegetation_raster) | vegetation_raster > threshold_high] <- NA
vegetation_mask[vegetation_raster >= threshold_low | vegetation_raster <= threshold_low] <- 1

# Convert the raster mask to polygons
vegetation_polygons <- as.polygons(vegetation_mask)


# Save the polygons to a GeoPackage file
output_file_path <- "C:/Users/beauq/Documents/Cours/UvicGaribaldiProject/ImageSatellite/Result/Envelope_Polygon.gpkg"
writeVector(vegetation_polygons, output_file_path, overwrite = TRUE)

  
# Convert raster to a data frame
df <- as.data.frame(raster, na.rm = TRUE)

# Rename the column to a consistent name, e.g., "value"
names(df) <- "value"


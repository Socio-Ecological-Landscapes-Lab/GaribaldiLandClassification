library(terra)
library(ggplot2)

# Charge data
dataPath <- "C:/Users/beauq/Documents/Cours/UvicGaribaldiProject/Field Work/Point/GlobalData.csv"
data <- read.csv(dataPath)

# Charge FCD map
FCDPath <- "C:/Users/beauq/Documents/Cours/UvicGaribaldiProject/ImageSatellite/Result/FCD_20240807.tif"
FCD <- rast(FCDPath)

# Create a data frame with points
points <- data.frame(
  longitude = data$longitude,
  latitude = data$latitude
)

# Convert data frame to a spatial vector
v <- vect(points, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# Porject the vector to the same CRS as the raster
v_projected <- project(v, crs(FCD))

# extract FCD values at the points
FCD_values <- extract(FCD, v_projected)

# plot the FCD raster
plot(FCD, main = "FCD Map with Points")

# add points to the plot
points(v_projected, pch = 20, col = "red", cex = 0.6)

# extract FCD values and add to the data frame
data$FCD_extracted <- FCD_values[, 2]


# Convert Avr.Den to numeric
data$Avr.Den <- as.numeric(gsub(",", ".", data$Avr.Den))

# plot the relation between Avr.Den and FCD extracted
ggplot(data, aes(x = Avr.Den, y = FCD_extracted)) +
  geom_point(color = "blue") +
  labs(title = "Relation between Avr.Den and FCD Extracted",
       x = "Avr.Den",
       y = "FCD Extracted") +
  scale_x_continuous(labels = function(x) round(x, digits = 2)) +
  theme_minimal()



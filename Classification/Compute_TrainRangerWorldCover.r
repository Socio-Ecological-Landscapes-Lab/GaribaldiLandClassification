library(terra)
library(sf)
library(dplyr)
library(caret)
library(ranger)

# Paths
date <- "20240905"
folder <- paste0("/project/def-nbl/come/PLANET_8B_", date)
raster_path <- "/scratch/come/composite.tif"
worldcover_path <- "/project/def-nbl/come/worldcoverData/WorldCover_ParkClassification.tif"

# Charge data
worldcover <- rast(worldcover_path)
raster <- rast(raster_path)

# Initialisation training data
training_data <- data.frame()

# Extraction of WorldCover values
print("Extraction des valeurs de la couche WorldCover...")


for (class_value in unique(values(worldcover))[!is.na(unique(values(worldcover)))]) {
  print(paste("Processing of the class:", class_value))

  class_mask <- worldcover == class_value
  class_cells <- which(class_mask[])

  if (length(class_cells) > 0) {
    set.seed(123)
    sample_cells <- sample(class_cells, min(100000, length(class_cells)))

    class_samples <- extract(raster, sample_cells) %>%
      na.omit() %>%
      mutate(class = as.factor(make.names(as.character(class_value))))

    training_data <- bind_rows(training_data, class_samples)
  }
}

# Calcul of class weights
class_weights <- 1 / table(training_data$class)
names(class_weights) <- levels(training_data$class)

print("Training model with Random Forest...")
# Train Random Forest model
rf_model <- ranger(
  class ~ .,
  data = training_data,
  num.trees = 500,
  mtry = sqrt(ncol(training_data) - 1),
  importance = "impurity"
)

# Save model
saveRDS(rf_model, "/project/def-nbl/come/models/rf_model_worldCover.rds")
print("Model saved to /project/def-nbl/come/models/rf_model_worldCover.rds")


print("Validation of model")
# Prediction on test data
preds <- predict(rf_model, training_data)$predictions

# Confusion matrix
conf_matrix <- confusionMatrix(
  data = preds,
  reference = training_data$class,
  positive = levels(training_data$class)[1]
)

# Display results
print("Confusion Matrix:")
print(conf_matrix$table)

print("Global accuracy:")
print(conf_matrix$overall['Accuracy'])

print("Precision by class:")
print(conf_matrix$byClass[c('Accuracy', 'Sensitivity', 'Specificity')])

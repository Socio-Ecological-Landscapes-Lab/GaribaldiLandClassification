library(ranger)
library(terra)


folder <- "C:\\Users\\beauq\\Documents\\Cours\\UvicGaribaldiProject\\ImageSatellite"
date <- "20240905"

# Charge image
raster <- rast(file.path(folder, paste0("PLANET_8B_", date, "\\composite.tif")))

#### -- Train the ranger model -- ####

# List .geojson files
geojson_files <- list.files(file.path(folder, paste0("PLANET_8B_", date), "SupervisedClassification2"), pattern = "\\.geojson$", full.names = TRUE)


# Extract name
class_names <- gsub(".geojson$", "", basename(geojson_files))


# Charge polygon merge and assing a class
training_samples <- do.call(rbind, lapply(seq_along(geojson_files), function(i) {
  poly <- vect(geojson_files[i])
  poly$class <- class_names[i]
  return(poly)
}))

# Extract polygon data from raster
full_data <- extract(raster, training_samples)

# Replace ID class by class name
full_data$class <- training_samples$class[full_data$ID]
full_data$ID <- NULL

# Divide data into training and test sets
set.seed(123)  # seed for reproducibility
test_ratio <- 0.2
test_indices <- sample(1:nrow(full_data), size = round(nrow(full_data) * test_ratio))
train_data <- full_data[-test_indices, ]
test_data <- full_data[test_indices, ]

# Convert as factor for the classification
train_data$class <- as.factor(train_data$class)

# Train ranger model (work with trees)
rf_model <- ranger(class ~ ., data = train_data, num.trees = 500, importance = 'impurity')

# Save test data
saveRDS(test_data, file.path(folder, "test_data.rds"))

# Save model
saveRDS(rf_model,file.path(folder, "rf_model.rds"))

#### -- Evaluate the model -- ####

# Predict classes for the test data set
predictions <- predict(rf_model, test_data)$predictions

# Create the confusion matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$class)

# Print confusion matrix
print(confusion_matrix)

# Calculate the performances of the model 
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print everything
cat("Précision globale :", accuracy, "\n")
cat("Précision par classe :", precision, "\n")
cat("Rappel par classe :", recall, "\n")
cat("Score F1 par classe :", f1_score, "\n")

# Save performances results
evaluation_results <- list(
  confusion_matrix = confusion_matrix,
  accuracy = accuracy,
  precision = precision,
  recall = recall,
  f1_score = f1_score
)

# Save model
saveRDS(evaluation_results, file.path(folder, "evaluation_results.rds"))


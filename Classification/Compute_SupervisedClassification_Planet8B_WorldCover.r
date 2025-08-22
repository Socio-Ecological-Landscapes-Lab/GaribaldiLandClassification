# Install requied packages
library(terra)
library(ranger)
library(foreach)
library(doParallel)


n_cores <- 14  # Number of cores to use
chunk_size <- 40000  # Size of each chunk for prediction # DOTO adjust in fonction of available RAM

# Path
date <- "20240905"
folder <- paste0("/project/def-nbl/come/PLANET_8B_", date)
raster_path <- "/scratch/come/composite.tif"
model_path <- "/project/def-nbl/come/models/rf_model_worldCover.rds"
raw_dir <- "/scratch/come/rasters_blocks_raw"
out_dir <- file.path(folder, "rasters_blocks")
log_file <- file.path("/scratch/come/my_job/log_classif.txt")



# log output
cat("=== Begening of script ===\n", file = log_file)

# Load model
modele_rf <- readRDS(model_path)

# Load raster
raster <- rast(raster_path)


# # Charger et projeter la zone d'intérêt
# area <- vect(file.path(folder,"LittleArea.geojson"))
# area <- project(area, crs(raster))
# raster <- crop(raster, area) 


# Cut the raster into blocks
divide_raster_by_rows <- function(raster, n_blocks) {
  # Get dimensions of the raster
  nrows <- nrow(raster)
  ncols <- ncol(raster)

  # Calculate the number of rows per block
  rows_per_block <- ceiling(nrows / n_blocks)

  # Initialize a list to store the blocks
  blocks <- list()

  # Divide the raster into blocks
  for (i in seq(1, nrows, rows_per_block)) {
    row_start <- i
    row_end <- min(i + rows_per_block - 1, nrows)

    # Calculate the extent of the block
    ymax_block <- ymax(raster) - (row_start - 1) * res(raster)[2]
    ymin_block <- ymax(raster) - row_end * res(raster)[2]
    e <- ext(xmin(raster), xmax(raster), ymin_block, ymax_block)

    # Crop the raster to the block extent
    r_block <- crop(raster, e)

    # Add the block to the list
    blocks[[length(blocks) + 1]] <- r_block
  }

  return(blocks)
}

# Call the function to divide the raster into blocks
blocks <- divide_raster_by_rows(raster, n_blocks = 14) # 14 blocks


cat("Nombre de blocs créés :", length(blocks), "\n", file = log_file)

# Save the blocks to disk
for (i in seq_along(blocks)) {
  writeRaster(blocks[[i]], file.path(raw_dir, paste0("block_", i, ".tif")), overwrite = TRUE)
}


# Create a parallel cluster with the specified number of cores
cl <- makeCluster(n_cores)
registerDoParallel(cl)

cat("[INFO] Classification of blocks in parallel...\n", file = log_file, append = TRUE)

# For each block, classify the data using the model
classified_files <- foreach(i = seq_len(length(blocks)), 
                            .packages = c("terra", "ranger"), 
                            .export = c("modele_rf", "raw_dir", "out_dir", "log_file", "chunk_size")) %dopar% {
  block_path <- file.path(raw_dir, paste0("block_", i, ".tif")) # Path
  cat(sprintf("[%s] Block %d charge\n", Sys.time(), i), file = log_file, append = TRUE)

  # Load the block and convert to data frame
  r <- rast(block_path)
  df <- as.data.frame(r, na.rm = FALSE)
  cat(sprintf("[%s] Block %d : DF made, taille = %.2f MB, %d rows\n", Sys.time(), i, 
              object.size(df)/1024^2, nrow(df)), file = log_file, append = TRUE)


  # Remove NA pixels
  valid <- !is.na(df[,1]) # Every row is a pixel, collumn are bands
  dfok <- df[valid, ] # Get only valid pixels
  cat(sprintf("[%s] Block %d : NA omitted, %d lines/pixels valids\n", Sys.time(), i, nrow(dfok)), file = log_file, append = TRUE)

  # Predict using the model in chunks (to avoid memory issues)
  pred_all <- rep(NA, nrow(dfok)) # Initialize predictions vector
  n_chunks <- ceiling(nrow(dfok) / chunk_size) # Number of chunks

  # Loop through each chunk and predict
  for (j in seq_len(n_chunks)) {
    idx_start <- (j - 1) * chunk_size + 1
    idx_end <- min(j * chunk_size, nrow(dfok))
    df_chunk <- dfok[idx_start:idx_end, ]
    p <- predict(modele_rf, df_chunk)$predictions # Predict using the model
    pred_all[idx_start:idx_end] <- p
    rm(df_chunk, p)
    gc()
  } 

  # Save the predictions to a new raster
  r_out <- r[[1]]
  values(r_out) <- NA
  values(r_out)[valid] <- pred_all
  out_path <- file.path(out_dir, paste0("classif_bloc_", i, ".tif"))
  writeRaster(r_out, out_path, overwrite = TRUE)


      

    # Vérifier les valeurs uniques
      cat(sprintf("[%s] avec les valeurs uniques %s\n", Sys.time(), paste(unique(pred_all), collapse = ", ")), file = log_file, append = TRUE)


  # Memory cleanup
  rm(r, r_out, df, dfok, pred_all)
  gc()

  return(out_path)
}

stopCluster(cl) # Stop the cluster

# Mosaic the classified blocks into a single raster
rasters <- lapply(classified_files, rast)
final_raster <- do.call(mosaic, c(rasters, list(fun="first")))
writeRaster(final_raster, file.path(folder, paste0("classification_worldCover_", date, ".tif")), overwrite = TRUE)
cat("[FIN] Classification terminée\n", file = log_file, append = TRUE)

rm(rasters, final_raster)
gc()

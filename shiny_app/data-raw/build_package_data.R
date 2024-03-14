
# Directories
indir <- "data/processed"
outdir <- "shiny_app/data"

# Read data
food_key <- readRDS(file.path(indir, "food_key.Rds"))
scores <- readRDS(file.path(indir, "scores.Rds"))
food_composition_data <- readRDS(file.path(indir, "food_composition_data.Rds"))

# Export data
saveRDS(food_key, file=file.path(outdir, "food_key.Rds"))
saveRDS(scores, file=file.path(outdir, "scores.Rds"))
saveRDS(food_composition_data, file=file.path(outdir, "food_composition_data.Rds"))
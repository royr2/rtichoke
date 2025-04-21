# Script to install required packages for rtichoke blog posts
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

# List of required packages
packages <- c(
  # Core data manipulation
  "dplyr",      # Data manipulation and transformations
  "magrittr",   # Pipe operators
  "tidyr",      # Data reshaping (pivot_longer, pivot_wider)
  "data.table", # Fast data manipulation
  "tibble",     # Enhanced data frames
  "devtools", 
  "gganimate", 
  "metR",
  "mvtnorm",
  "plotly",
  "pROC",
  
  # Visualization
  "ggplot2",    # Data visualization
  "patchwork",  # Combine multiple ggplots
  "corrplot",   # Correlation visualization
  
  # Statistics and modeling
  "rsample",    # Data sampling and resampling
  "forecast",   # Time series forecasting
  "tseries",    # Time series analysis
  
  # Data import and handling
  "readxl",     # Read Excel files
  "naniar",     # Missing data visualization and analysis
  "mice",        # Multiple imputation for missing data
  
  "xgboost", 
  "ParBayesianOptimization", 
  "mlbench", 
  "recipes"
)

# Install packages if they're not already installed
sapply(packages, install_if_missing)

# Print confirmation message
cat("\nPackage installation complete.\n")
cat("The following packages were processed:\n")
cat(paste(packages, collapse = ", "), "\n")

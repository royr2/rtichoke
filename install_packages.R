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
  "tidyverse",  # Collection of R packages for data science
  "readr",      # Read rectangular data (csv, tsv, etc.)
  "stringr",    # String manipulation functions
  
  # Visualization
  "ggplot2",    # Data visualization
  "patchwork",  # Combine multiple ggplots
  "corrplot",   # Correlation visualization
  "gganimate",  # Create animations with ggplot2
  "scales",     # Scale functions for visualization
  "metR",       # Tools for meteorology (used for geom_arrow)
  "plotly",     # Interactive web-based graphs
  
  # Statistics and modeling
  "rsample",    # Data sampling and resampling
  "forecast",   # Time series forecasting
  "tseries",    # Time series analysis
  "recipes",    # Preprocessing and feature engineering
  "tidymodels", # Collection of modeling packages
  "xgboost",    # Extreme gradient boosting
  "vip",        # Variable importance plots
  "probably",   # Tools for post-processing predictions
  "ROSE",       # Random over-sampling examples (for imbalanced data)
  "ParBayesianOptimization", # Bayesian optimization
  "mlbench",    # Machine learning benchmark problems
  
  # Data import and handling
  "readxl",     # Read Excel files
  "naniar",     # Missing data visualization and analysis
  "mice",       # Multiple imputation for missing data
  "knitr",      # Dynamic report generation
  
  # Deep learning and neural networks
  "torch",      # Deep learning framework
  "neuralnet",  # Neural network training
  
  # Financial and optimization
  "pso",        # Particle swarm optimization
  "quantmod",   # Quantitative financial modeling
  
  # Text analytics
  "quanteda",           # Quantitative analysis of textual data
  "quanteda.textstats", # Text statistics for quanteda
  "quanteda.textplots", # Text plots for quanteda
  "tidytext",
  
  # Python integration
  "reticulate", # Interface to Python
  
  # Other utilities
  "devtools",   # Tools to make developing R packages easier
  "pROC"        # ROC curve analysis
)

# Install packages if they're not already installed
sapply(packages, install_if_missing)

# Print confirmation message
cat("\nPackage installation complete.\n")
cat("The following packages were processed:\n")
cat(paste(packages, collapse = ", "), "\n")

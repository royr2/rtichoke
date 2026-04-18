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
  "mlbench",    # Machine learning benchmark problems
  "caret",      # Classification and regression training
  "mlr3",       # Machine learning framework
  "mlr3learners", # Additional learners for mlr3
  "h2o",        # Scalable machine learning platform
  "qeML",      # Quick and easy machine learning
  "ranger",     # Fast random forests
  "mvtnorm",    # Multivariate normal distribution
  "rBayesianOptimization",
  "randomForest",
  
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
  "purrr",      # Functional programming tools
  "slider",     # Sliding window functions
  
  # Text analytics
  "quanteda",           # Quantitative analysis of textual data
  "quanteda.textstats", # Text statistics for quanteda
  "quanteda.textplots", # Text plots for quanteda
  "tidytext",
  
  # Python integration
  "reticulate", # Interface to Python
  
  # Other utilities
  "devtools",   # Tools to make developing R packages easier
  "pROC",       # ROC curve analysis. 
  "DT",         # Interactive data tables for R
  "foreach",    # Foreach looping construct
  "doParallel", # Parallel backend for foreach
  "tictoc",     # Timing functions
  "RSQLite",    # SQLite interface for R
  "pacman",     # Package management tool
  "pak",        # Package installation tools
  "renv",       # Reproducible environments
  
  # Add more packages as needed for future posts
  "ggrepel",    # Non-overlapping text labels for ggplot2
  "DBI",        # Database interface (used by ragnar's DuckDB store)
  "ellmer",     # LLM interaction and tool registration. 
  "ragnar"     # Retrieval-augmented generation store (DuckDB + VSS)
)

if(!require(pak)) install.packages("pak")
pak::pkg_install(packages)

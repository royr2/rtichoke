# Install Required Packages for ML Model Builder
# This script uses the pak package for fast and reliable package installation

# Function to install pak if not available
install_pak_if_needed <- function() {
  if (!requireNamespace("pak", quietly = TRUE)) {
    cat("Installing pak package...\n")
    install.packages("pak", repos = "https://r-lib.github.io/p/pak/stable/")
  }
}

# Function to install all required packages
install_required_packages <- function() {
  cat("=== ML Model Builder Package Installation ===\n")
  
  # Install pak first
  install_pak_if_needed()
  
  # Load pak
  library(pak)
  
  # Define required packages
  required_packages <- c(
    # Core Shiny packages
    "shiny",
    "shinydashboard",
    "shinyjs",
    
    # Data manipulation and visualization
    "dplyr",
    "tidyverse",
    "ggplot2",
    "plotly",
    "DT",
    
    # Machine learning packages
    "caret",
    "randomForest",
    "e1071",
    "glmnet",
    "rpart",
    "rpart.plot",
    
    # Statistical analysis
    "corrplot",
    "car",
    
    # Data processing
    "tidyr",
    "readr",
    
    # Testing (optional)
    "testthat"
  )
  
  cat("Installing packages:", paste(required_packages, collapse = ", "), "\n")
  cat("This may take a few minutes...\n\n")
  
  # Install packages using pak
  tryCatch({
    pak::pak(required_packages)
    cat("\n✅ All packages installed successfully!\n")
    return(TRUE)
  }, error = function(e) {
    cat("\n❌ installing packages:", e$message, "\n")
    cat("Trying alternative installation method...\n")
    
    # Fallback to regular install.packages
    failed_packages <- c()
    for (pkg in required_packages) {
      tryCatch({
        if (!requireNamespace(pkg, quietly = TRUE)) {
          install.packages(pkg, dependencies = TRUE)
          cat("✅", pkg, "installed\n")
        } else {
          cat("✓", pkg, "already installed\n")
        }
      }, error = function(e) {
        cat("❌ Failed to install", pkg, ":", e$message, "\n")
        failed_packages <<- c(failed_packages, pkg)
      })
    }
    
    if (length(failed_packages) > 0) {
      cat("\n⚠️  The following packages failed to install:\n")
      cat(paste(failed_packages, collapse = ", "), "\n")
      cat("You may need to install them manually.\n")
      return(FALSE)
    } else {
      cat("\n✅ All packages installed successfully using fallback method!\n")
      return(TRUE)
    }
  })
}

# Function to check if all packages are available
check_packages <- function() {
  required_packages <- c(
    "shiny", "shinydashboard", "shinyjs", "dplyr", "tidyverse", 
    "ggplot2", "plotly", "DT", "caret", "randomForest", "e1071", 
    "glmnet", "rpart", "rpart.plot", "corrplot", "car", "tidyr", "readr"
  )
  
  missing_packages <- c()
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if (length(missing_packages) > 0) {
    cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n")
    return(FALSE)
  } else {
    cat("✅ All required packages are available!\n")
    return(TRUE)
  }
}

# Main execution
if (!interactive()) {
  # If script is run non-interactively, install packages
  install_required_packages()
} else {
  # If run interactively, give user options
  cat("ML Model Builder Package Manager\n")
  cat("================================\n")
  cat("1. Check if packages are installed\n")
  cat("2. Install all required packages\n")
  cat("3. Exit\n\n")
  
  choice <- readline(prompt = "Enter your choice (1-3): ")
  
  if (choice == "1") {
    check_packages()
  } else if (choice == "2") {
    install_required_packages()
  } else {
    cat("Goodbye!\n")
  }
}

# ML Model Builder - Main Application Entry Point
# This file ensures all dependencies are loaded and starts the Shiny application

# Display startup message
cat("Starting ML Model Builder...\n")

# Source global configuration and package management
tryCatch({
  source("global.R")
  cat("✅ Global configuration loaded successfully\n")
}, error = function(e) {
  cat("❌ Error loading global.R:", e$message, "\n")
  cat("Please ensure all required packages are installed.\n")
  cat("Run install_packages.R first or use run_app.R for automatic setup.\n")
  stop("Cannot start application due to missing dependencies.")
})

# Source UI and server components
tryCatch({
  source("ui.R")
  source("server.R")
  cat("✅ UI and server components loaded successfully\n")
}, error = function(e) {
  cat("❌ Error loading UI/Server components:", e$message, "\n")
  stop("Cannot start application due to component loading error.")
})

# Create and run the Shiny application
cat("🚀 Launching ML Model Builder application...\n")
cat("The application will open in your default web browser.\n")
cat("If it doesn't open automatically, navigate to the URL shown below.\n\n")

shinyApp(ui = ui, server = server)
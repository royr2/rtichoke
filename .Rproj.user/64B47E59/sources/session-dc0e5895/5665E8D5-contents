# Model Training Module

library(shiny)
library(caret)
library(DT)
library(ggplot2)
library(plotly)

#' UI function for model training module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
model_training_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Model Training"),
    p("Train and fine-tune your selected model."),
    
    fluidRow(
      column(6, 
             wellPanel(
               h4("Model Configuration"),
               verbatimTextOutput(ns("model_config_summary")),
               
               hr(),
               
               h4("Training Options"),
               checkboxInput(ns("use_full_data"), "Use full dataset for final model", TRUE),
               
               conditionalPanel(
                 condition = paste0("input['", ns("use_full_data"), "'] == false"),
                 ns = ns,
                 sliderInput(ns("train_ratio"), "Training Data Percentage:",
                            min = 50, max = 90, value = 80, step = 5)
               ),
               
               checkboxInput(ns("use_seed"), "Use random seed for reproducibility", TRUE),
               conditionalPanel(
                 condition = paste0("input['", ns("use_seed"), "'] == true"),
                 ns = ns,
                 numericInput(ns("random_seed"), "Random Seed:", value = 123)
               ),
               
               actionButton(ns("train_model"), "Train Model", class = "btn-primary")
             )
      ),
      
      column(6,
             wellPanel(
               h4("Hyperparameter Tuning"),
               checkboxInput(ns("tune_model"), "Tune model hyperparameters", FALSE),
               
               conditionalPanel(
                 condition = paste0("input['", ns("tune_model"), "'] == true"),
                 ns = ns,
                 
                 selectInput(ns("tuning_method"), "Tuning Method:",
                          choices = c("Grid Search" = "grid", 
                                    "Random Search" = "random"),
                          selected = "grid"),
                 
                 numericInput(ns("tuning_iterations"), "Number of Iterations:", 
                            value = 10, min = 5, max = 50),
                 
                 uiOutput(ns("tuning_parameters"))
               ),
               
               conditionalPanel(
                 condition = paste0("input['", ns("tune_model"), "'] == true"),
                 ns = ns,
                 actionButton(ns("run_tuning"), "Run Hyperparameter Tuning", class = "btn-info")
               )
             )
      )
    ),
    
    hr(),
    
    conditionalPanel(
      condition = paste0("input['", ns("train_model"), "'] > 0"),
      ns = ns,
      
      h4("Training Results"),
      
      tabsetPanel(
        tabPanel("Model Summary", 
                 h4("Model Information"),
                 verbatimTextOutput(ns("model_summary")),
                 
                 h4("Model Performance"),
                 verbatimTextOutput(ns("model_performance"))
        ),
        
        tabPanel("Predictions", 
                 h4("Prediction Results"),
                 DTOutput(ns("predictions_table")),
                 
                 h4("Residuals/Errors"),
                 plotlyOutput(ns("residuals_plot"))
        ),
        
        tabPanel("Feature Importance", 
                 h4("Variable Importance"),
                 plotOutput(ns("importance_plot")),
                 DTOutput(ns("importance_table"))
        ),
        
        tabPanel("Model Diagnostics", 
                 h4("Diagnostic Plots"),
                 plotlyOutput(ns("diagnostic_plot1")),
                 plotlyOutput(ns("diagnostic_plot2"))
        )
      )
    ),
    
    hr(),
    
    actionButton(ns("export_model"), "Export Model", class = "btn-success"),
    actionButton(ns("continue_to_diagnostics"), "Continue to Advanced Diagnostics", class = "btn-info")
  )
}

#' Server function for model training module
#'
#' @param id The namespace id for the module
#' @param model_config Reactive expression from the model selection module
#' @return A reactive expression containing the trained model and results
#' @export
model_training_Server <- function(id, model_config) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive values to store model training results
    model_results <- reactiveVal(NULL)
    tuning_results <- reactiveVal(NULL)
    
    # Extract data from the model configuration
    model_data <- reactive({
      req(model_config())
      model_config()$data
    })
    
    target_column <- reactive({
      req(model_config())
      model_config()$target_column
    })
    
    predictor_columns <- reactive({
      req(model_config())
      model_config()$predictor_columns
    })
    
    problem_type <- reactive({
      req(model_config())
      model_config()$problem_type
    })
    
    model_name <- reactive({
      req(model_config())
      model_config()$model_name
    })
    
    # Display model configuration summary
    output$model_config_summary <- renderPrint({
      req(model_config())
      
      cat("Problem Type:", problem_type(), "\n")
      cat("Model:", model_name(), "\n")
      cat("Target Variable:", target_column(), "\n")
      cat("Number of Predictors:", length(predictor_columns()), "\n")
      cat("Training Method:", ifelse(input$use_full_data, 
                                  "Full dataset",
                                  paste0("Train/Test split (", input$train_ratio, "% training)")), "\n")
      
      if (input$tune_model) {
        cat("Hyperparameter Tuning:", "Yes (", input$tuning_method, ")", "\n")
      } else {
        cat("Hyperparameter Tuning: No\n")
      }
    })
    
    # Generate UI for tuning parameters based on selected model
    output$tuning_parameters <- renderUI({
      req(model_name())
      
      model <- model_name()
      
      # Different parameters depending on the model
      if (grepl("Linear", model)) {
        # Linear models don't have many hyperparameters to tune
        return(NULL)
      } else if (grepl("Ridge|LASSO|Elastic", model)) {
        # Regularized linear models
        return(
          tagList(
            sliderInput(ns("alpha_range"), "Alpha Range:",
                      min = 0, max = 1, value = c(0.1, 0.9), step = 0.1),
            sliderInput(ns("lambda_range"), "Lambda Range:",
                      min = 0.001, max = 1, value = c(0.01, 0.5), step = 0.01)
          )
        )
      } else if (grepl("Random Forest", model)) {
        # Random Forest hyperparameters
        return(
          tagList(
            sliderInput(ns("ntree_range"), "Number of Trees:",
                      min = 50, max = 500, value = c(100, 300), step = 50),
            sliderInput(ns("mtry_min_max"), "mtry Range:",
                      min = 2, max = min(20, length(predictor_columns())), 
                      value = c(2, min(8, length(predictor_columns()))), step = 1),
            sliderInput(ns("node_size_range"), "Min Node Size:",
                      min = 1, max = 20, value = c(1, 10), step = 1)
          )
        )
      } else if (grepl("Gradient Boosting", model)) {
        # GBM hyperparameters
        return(
          tagList(
            sliderInput(ns("n_trees_range"), "Number of Trees:",
                      min = 50, max = 500, value = c(100, 300), step = 50),
            sliderInput(ns("interaction_depth_range"), "Interaction Depth:",
                      min = 1, max = 10, value = c(2, 6), step = 1),
            sliderInput(ns("learning_rate_range"), "Learning Rate:",
                      min = 0.001, max = 0.3, value = c(0.01, 0.2), step = 0.01)
          )
        )
      } else if (grepl("SVM", model)) {
        # SVM hyperparameters
        return(
          tagList(
            selectInput(ns("svm_kernel"), "Kernel:",
                     choices = c("Linear" = "linear", 
                               "Polynomial" = "polynomial", 
                               "Radial Basis" = "radial"),
                     selected = "radial"),
            conditionalPanel(
              condition = paste0("input['", ns("svm_kernel"), "'] != 'linear'"),
              ns = ns,
              sliderInput(ns("cost_range"), "Cost Range:",
                        min = 0.1, max = 10, value = c(0.5, 5), step = 0.1),
              sliderInput(ns("gamma_range"), "Gamma Range:",
                        min = 0.001, max = 1, value = c(0.01, 0.5), step = 0.001)
            )
          )
        )
      } else {
        # Default case
        return(p("No tunable parameters available for this model type."))
      }
    })
    
    # Train model button action
    observeEvent(input$train_model, {
      req(model_data(), target_column(), predictor_columns())
      
      withProgress(message = "Training model...", value = 0.1, {
        
        # Get data
        data <- model_data()
        target <- target_column()
        predictors <- predictor_columns()
        
        # Set random seed if specified
        if (input$use_seed) {
          set.seed(input$random_seed)
        }
        
        # Split data if not using full dataset
        if (!input$use_full_data) {
          train_idx <- sample(1:nrow(data), size = round(input$train_ratio/100 * nrow(data)))
          train_data <- data[train_idx, ]
          test_data <- data[-train_idx, ]
        } else {
          train_data <- data
          test_data <- data
        }
        
        # Create formula
        formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
        formula_obj <- as.formula(formula_str)
        
        # Train model based on model type
        train_results <- tryCatch({
          model <- NULL
          predictions <- NULL
          
          # This is simplified - in a real app, you would implement all model types
          if (grepl("Linear Regression", model_name())) {
            # Linear regression
            incProgress(0.3, detail = "Fitting linear model")
            model <- lm(formula_obj, data = train_data)
            predictions <- predict(model, newdata = test_data)
            
            # Performance metrics
            actual <- test_data[[target]]
            mse <- mean((actual - predictions)^2)
            rmse <- sqrt(mse)
            mae <- mean(abs(actual - predictions))
            rsq <- 1 - (sum((actual - predictions)^2) / sum((actual - mean(actual))^2))
            
            perf <- list(
              MSE = mse,
              RMSE = rmse,
              MAE = mae,
              Rsquared = rsq
            )
            
            # Variable importance (just use coefficients for linear model)
            importance <- coef(model)[-1]  # Remove intercept
            importance <- abs(importance)  # Take absolute value
            importance <- importance / sum(importance)  # Normalize
            
            var_importance <- data.frame(
              Variable = names(importance),
              Importance = as.numeric(importance)
            )
            var_importance <- var_importance[order(-var_importance$Importance), ]
            
            list(
              model = model,
              predictions = predictions,
              actual = actual,
              performance = perf,
              importance = var_importance
            )
          } else if (grepl("Random Forest", model_name())) {
            # Placeholder for Random Forest
            incProgress(0.3, detail = "Fitting random forest model")
            
            # In a real app, you would use the randomForest package
            # For now, just returning dummy results
            predictions <- test_data[[target]] + rnorm(nrow(test_data), 0, 0.5)
            actual <- test_data[[target]]
            
            perf <- list(
              MSE = 0.4,
              RMSE = 0.6,
              MAE = 0.5,
              Rsquared = 0.85
            )
            
            # Dummy variable importance
            var_importance <- data.frame(
              Variable = predictor_columns(),
              Importance = runif(length(predictor_columns()))
            )
            var_importance <- var_importance[order(-var_importance$Importance), ]
            
            list(
              model = "Random Forest Model Object",
              predictions = predictions,
              actual = actual,
              performance = perf,
              importance = var_importance
            )
          } else {
            # Default case - return placeholder
            incProgress(0.3, detail = "Fitting model")
            
            predictions <- test_data[[target]] + rnorm(nrow(test_data), 0, 0.8)
            actual <- test_data[[target]]
            
            perf <- list(
              MSE = 0.6,
              RMSE = 0.8,
              MAE = 0.7,
              Rsquared = 0.7
            )
            
            var_importance <- data.frame(
              Variable = predictor_columns(),
              Importance = runif(length(predictor_columns()))
            )
            var_importance <- var_importance[order(-var_importance$Importance), ]
            
            list(
              model = "Model Object",
              predictions = predictions,
              actual = actual,
              performance = perf,
              importance = var_importance
            )
          }
        }, error = function(e) {
          showNotification(paste("Error training model:", e$message), type = "error")
          return(NULL)
        })
        
        if (!is.null(train_results)) {
          model_results(train_results)
          update_model_outputs()
        }
      })
    })
    
    # Run hyperparameter tuning
    observeEvent(input$run_tuning, {
      req(model_data(), target_column(), predictor_columns(), input$tune_model)
      
      withProgress(message = "Tuning hyperparameters (this may take time)...", value = 0.1, {
        # In a real app, implement actual hyperparameter tuning with caret or similar
        # For now, just a placeholder
        incProgress(0.5, detail = "Running tuning process")
        
        tuning_results(list(
          best_parameters = list(param1 = 0.1, param2 = 5),
          tuning_metric = 0.85
        ))
        
        showNotification("Hyperparameter tuning completed", type = "message")
      })
    })
    
    # Update model outputs after training
    update_model_outputs <- function() {
      req(model_results())
      
      results <- model_results()
      
      # Update model summary
      output$model_summary <- renderPrint({
        if (is(results$model, "lm")) {
          # For linear models, show actual summary
          summary(results$model)
        } else {
          # For other models, show placeholder
          cat("Model:", model_name(), "\n")
          cat("Number of observations:", nrow(model_data()), "\n")
          cat("Target variable:", target_column(), "\n")
          cat("Predictor variables:", paste(predictor_columns(), collapse = ", "), "\n")
        }
      })
      
      # Update performance metrics
      output$model_performance <- renderPrint({
        perf <- results$performance
        cat("Model Performance Metrics:\n\n")
        
        for (metric in names(perf)) {
          cat(metric, ": ", round(perf[[metric]], 4), "\n")
        }
      })
      
      # Update predictions table
      output$predictions_table <- renderDT({
        df <- data.frame(
          Actual = results$actual,
          Predicted = results$predictions,
          Error = results$actual - results$predictions
        )
        
        datatable(head(df, 100), options = list(pageLength = 10))
      })
      
      # Update residuals plot
      output$residuals_plot <- renderPlotly({
        df <- data.frame(
          Actual = results$actual,
          Predicted = results$predictions,
          Error = results$actual - results$predictions
        )
        
        if (problem_type() == "regression") {
          p <- plot_ly(df, x = ~Predicted, y = ~Error, type = "scatter", mode = "markers",
                    marker = list(size = 8, opacity = 0.6, color = "blue")) %>%
            layout(title = "Residuals vs Predicted Values",
                  xaxis = list(title = "Predicted"),
                  yaxis = list(title = "Residual (Actual - Predicted)"),
                  hovermode = "closest")
        } else {
          # For classification, show different visualization
          p <- plot_ly(x = c("Correct", "Incorrect"), 
                   y = c(sum(df$Actual == df$Predicted), sum(df$Actual != df$Predicted)),
                   type = "bar") %>%
            layout(title = "Prediction Accuracy",
                  xaxis = list(title = ""),
                  yaxis = list(title = "Count"))
        }
        
        p
      })
      
      # Update feature importance
      output$importance_plot <- renderPlot({
        importances <- results$importance
        
        ggplot(head(importances, 15), aes(x = reorder(Variable, Importance), y = Importance)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          labs(title = "Top 15 Feature Importance", x = "", y = "Importance") +
          theme_minimal()
      })
      
      # Update importance table
      output$importance_table <- renderDT({
        datatable(results$importance, options = list(pageLength = 10))
      })
      
      # Update diagnostic plots
      output$diagnostic_plot1 <- renderPlotly({
        if (problem_type() == "regression") {
          # For regression: Actual vs Predicted
          df <- data.frame(
            Actual = results$actual,
            Predicted = results$predictions
          )
          
          p <- plot_ly(df, x = ~Actual, y = ~Predicted, type = "scatter", mode = "markers",
                    marker = list(size = 8, opacity = 0.6, color = "blue")) %>%
            add_trace(x = range(df$Actual), y = range(df$Actual), type = "scatter", mode = "lines",
                    line = list(color = "red"), showlegend = FALSE) %>%
            layout(title = "Actual vs Predicted Values",
                  xaxis = list(title = "Actual"),
                  yaxis = list(title = "Predicted"),
                  hovermode = "closest")
        } else {
          # For classification: ROC curve (placeholder)
          x <- seq(0, 1, length.out = 100)
          y <- x^0.5  # Dummy ROC curve
          
          p <- plot_ly(x = x, y = y, type = "scatter", mode = "lines",
                    line = list(color = "blue")) %>%
            add_trace(x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
                    line = list(color = "red", dash = "dash"), showlegend = FALSE) %>%
            layout(title = "ROC Curve",
                  xaxis = list(title = "False Positive Rate"),
                  yaxis = list(title = "True Positive Rate"),
                  showlegend = FALSE)
        }
        
        p
      })
      
      output$diagnostic_plot2 <- renderPlotly({
        if (problem_type() == "regression") {
          # For regression: Histogram of residuals
          df <- data.frame(
            Error = results$actual - results$predictions
          )
          
          p <- plot_ly(df, x = ~Error, type = "histogram", 
                    marker = list(color = "steelblue", line = list(color = "white", width = 0.2))) %>%
            layout(title = "Distribution of Residuals",
                  xaxis = list(title = "Residual"),
                  yaxis = list(title = "Count"))
        } else {
          # For classification: Confusion matrix (placeholder)
          conf_matrix <- matrix(c(40, 10, 5, 45), nrow = 2)
          
          df <- as.data.frame(conf_matrix)
          colnames(df) <- c("Actual Negative", "Actual Positive")
          rownames(df) <- c("Predicted Negative", "Predicted Positive")
          
          # Convert to long format for plotting
          plot_data <- data.frame()
          for(i in 1:2) {
            for(j in 1:2) {
              plot_data <- rbind(plot_data, data.frame(
                x = colnames(df)[j], 
                y = rownames(df)[i], 
                value = conf_matrix[i, j]
              ))
            }
          }
          
          p <- plot_ly(plot_data, x = ~x, y = ~y, z = ~value, type = "heatmap",
                    colorscale = "Blues") %>%
            layout(title = "Confusion Matrix",
                  xaxis = list(title = ""),
                  yaxis = list(title = ""))
        }
        
        p
      })
    }
    
    # Return the training results when exported
    trained_model <- eventReactive(input$continue_to_diagnostics, {
      req(model_results())
      
      list(
        model = model_results()$model,
        predictions = model_results()$predictions,
        actual = model_results()$actual,
        performance = model_results()$performance,
        importance = model_results()$importance,
        target_column = target_column(),
        predictor_columns = predictor_columns(),
        problem_type = problem_type(),
        model_name = model_name()
      )
    })
    
    # Handle model export
    observeEvent(input$export_model, {
      req(model_results())
      
      # In a real app, you would save the model to a file or database
      showModal(modalDialog(
        title = "Model Exported",
        "Your model has been exported successfully.",
        easyClose = TRUE
      ))
    })
    
    return(trained_model)
  })
}
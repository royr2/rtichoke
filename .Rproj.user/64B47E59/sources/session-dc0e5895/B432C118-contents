# Column Selection Module for Shiny Application

library(shiny)
library(DT)

#' UI function for column selection module
#'
#' @param id The namespace id for the module
#' @return A UI element
#' @export
column_selection_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Column Selection"),
    p("Select the columns for your analysis model."),
    
    fluidRow(
      column(6, 
             wellPanel(
               h4("Target Column"),
               p("This is the variable you want to predict."),
               uiOutput(ns("target_column_ui")),
               
               conditionalPanel(
                 condition = paste0("input['", ns("target"), "'] != null"),
                 ns = ns,
                 h5("Target Column Type:"),
                 verbatimTextOutput(ns("target_type")),
                 h5("Target Column Statistics:"),
                 verbatimTextOutput(ns("target_stats"))
               )
             )
      ),
      
      column(6,
             wellPanel(
               h4("Predictor Columns"),
               p("These are the variables used to predict the target."),
               actionButton(ns("select_all"), "Select All"),
               actionButton(ns("deselect_all"), "Deselect All"),
               checkboxGroupInput(ns("exclude_types"), "Exclude column types:",
                                  choices = c("Character" = "character",
                                            "Factor" = "factor",
                                            "Numeric" = "numeric",
                                            "Integer" = "integer",
                                            "Date" = "Date"),
                                  selected = NULL),
               uiOutput(ns("predictor_columns_ui"))
             )
      )
    ),
    
    hr(),
    
    h4("Selected Columns Summary"),
    verbatimTextOutput(ns("selection_summary")),
    
    hr(),
    
    h4("Data Preview with Selected Columns"),
    DTOutput(ns("selected_columns_preview")),
    
    actionButton(ns("confirm_selection"), "Confirm Selection and Continue", 
               class = "btn-success", 
               style = "margin-top: 20px;")
  )
}

#' Server function for column selection module
#'
#' @param id The namespace id for the module
#' @param data_input Reactive expression that returns the preprocessed data
#' @return A reactive expression containing the selected data and metadata
#' @export
column_selection_Server <- function(id, data_input) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Reactive value to store available columns
    available_columns <- reactive({
      req(data_input())
      names(data_input())
    })
    
    # Generate UI for target column selection
    output$target_column_ui <- renderUI({
      req(data_input())
      df <- data_input()
      
      # Filter columns based on data type for better target column selection
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      factor_cols <- names(df)[sapply(df, is.factor)]
      
      # Prefer categorical or numeric columns as targets
      all_cols <- c(factor_cols, numeric_cols, setdiff(names(df), c(factor_cols, numeric_cols)))
      
      selectInput(ns("target"), 
                "Select Target Column", 
                choices = all_cols, 
                selected = if(length(all_cols) > 0) all_cols[1] else NULL)
    })
    
    # Generate UI for predictor columns selection
    output$predictor_columns_ui <- renderUI({
      req(data_input(), input$target)
      df <- data_input()
      
      # Get all columns except the target
      all_cols <- setdiff(names(df), input$target)
      
      # Filter based on excluded types
      if (!is.null(input$exclude_types) && length(input$exclude_types) > 0) {
        cols_to_include <- character(0)
        for (col in all_cols) {
          col_type <- class(df[[col]])[1]
          if (!(col_type %in% input$exclude_types)) {
            cols_to_include <- c(cols_to_include, col)
          }
        }
        all_cols <- cols_to_include
      }
      
      checkboxGroupInput(ns("predictors"), 
                       "Select Predictor Columns:", 
                       choices = all_cols,
                       selected = all_cols)
    })
    
    # Select all predictors
    observeEvent(input$select_all, {
      req(data_input(), input$target)
      df <- data_input()
      
      all_cols <- setdiff(names(df), input$target)
      
      # Filter based on excluded types
      if (!is.null(input$exclude_types) && length(input$exclude_types) > 0) {
        cols_to_include <- character(0)
        for (col in all_cols) {
          col_type <- class(df[[col]])[1]
          if (!(col_type %in% input$exclude_types)) {
            cols_to_include <- c(cols_to_include, col)
          }
        }
        all_cols <- cols_to_include
      }
      
      updateCheckboxGroupInput(session, "predictors", selected = all_cols)
    })
    
    # Deselect all predictors
    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(session, "predictors", selected = character(0))
    })
    
    # Detect the type of problem (classification or regression)
    problem_type <- reactive({
      req(data_input(), input$target)
      df <- data_input()
      
      target_col <- df[[input$target]]
      
      if (is.factor(target_col) || is.character(target_col)) {
        # If target is categorical, it's a classification problem
        return("classification")
      } else if (is.numeric(target_col) || is.integer(target_col)) {
        # If target is numeric, it's a regression problem
        return("regression")
      } else {
        # Fallback
        return("unknown")
      }
    })
    
    # Show target column type and stats
    output$target_type <- renderText({
      req(data_input(), input$target)
      df <- data_input()
      
      target_col <- df[[input$target]]
      col_type <- class(target_col)[1]
      
      paste("Data type:", col_type, "\nProblem type:", problem_type())
    })
    
    output$target_stats <- renderText({
      req(data_input(), input$target)
      df <- data_input()
      
      target_col <- df[[input$target]]
      
      if (is.factor(target_col) || is.character(target_col)) {
        # For categorical target
        freq_table <- table(target_col)
        paste("Levels:", paste(names(freq_table), collapse = ", "), 
              "\nCounts:", paste(as.integer(freq_table), collapse = ", "),
              "\nDistribution (%):", paste0(round(100 * as.integer(freq_table) / sum(freq_table), 1), "%", collapse = ", "))
      } else if (is.numeric(target_col) || is.integer(target_col)) {
        # For numeric target
        paste("Min:", min(target_col, na.rm = TRUE),
              "\nMax:", max(target_col, na.rm = TRUE),
              "\nMean:", round(mean(target_col, na.rm = TRUE), 4),
              "\nMedian:", median(target_col, na.rm = TRUE),
              "\nStd Dev:", round(sd(target_col, na.rm = TRUE), 4))
      } else {
        "Cannot compute statistics for this data type."
      }
    })
    
    # Show summary of selected columns
    output$selection_summary <- renderText({
      req(input$target, input$predictors)
      
      paste("Target column:", input$target,
            "\nNumber of predictor columns:", length(input$predictors),
            "\nProblem type:", problem_type())
    })
    
    # Preview of selected columns
    output$selected_columns_preview <- renderDT({
      req(data_input(), input$target, input$predictors)
      df <- data_input()
      
      selected_cols <- c(input$target, input$predictors)
      filtered_df <- df[, selected_cols, drop = FALSE]
      
      datatable(head(filtered_df, 10), 
               options = list(scrollX = TRUE, 
                              dom = 'ftip',
                              pageLength = 5),
               rownames = FALSE)
    })
    
    # Reactive to store final selected data
    selected_data <- eventReactive(input$confirm_selection, {
      req(data_input(), input$target, input$predictors)
      df <- data_input()
      
      selected_cols <- c(input$target, input$predictors)
      selected_df <- df[, selected_cols, drop = FALSE]
      
      list(
        data = selected_df,
        target_column = input$target,
        predictor_columns = input$predictors,
        problem_type = problem_type()
      )
    })
    
    return(selected_data)
  })
}
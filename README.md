# MATH-3210-Final-Project
## Description
Final Project for Math 3210

## Live App
Access the live Shiny application here: https://kalyiapugh.shinyapps.io/Math3210_FinalApp/

## Repository Contents
- `app.R`: # Palmer Penguins Comprehensive Analysis App
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(palmerpenguins)
library(caret)
library(broom)
library(car)
library(MASS)
library(corrplot)
library(ggplot2)
library(plotly)
library(moments)
library(reshape2)  # Added for melt function
library(lmtest)    # Added for bptest function

# Data Transformation Functions
apply_transformation <- function(data, variable, method = "log") {
  transformed_data <- data
  
  switch(method,
         "log" = {
           min_val <- min(data[[variable]], na.rm = TRUE)
           if (min_val <= 0) {
             transformed_data[[variable]] <- log(data[[variable]] - min_val + 1)
           } else {
             transformed_data[[variable]] <- log(data[[variable]])
           }
         },
         "sqrt" = {
           transformed_data[[variable]] <- sqrt(data[[variable]])
         },
         "box_cox" = {
           transformed_data[[variable]] <- powerTransform(data[[variable]])$transformed
         }
  )
  
  return(transformed_data)
}

recommend_transformation <- function(data, variable) {
  skewness_val <- moments::skewness(data[[variable]], na.rm = TRUE)
  
  if (abs(skewness_val) > 1) {
    if (skewness_val > 0) {
      return("log")  # Right-skewed
    } else {
      return("sqrt")  # Left-skewed
    }
  } else {
    return(NULL)  # No transformation needed
  }
}

# Statistical Modeling Functions
run_linear_regression <- function(data, response_var, explanatory_vars) {
  formula_str <- paste(response_var, "~", 
                       paste(explanatory_vars, collapse = " + "))
  model <- lm(as.formula(formula_str), data = data)
  return(model)
}

run_robust_regression <- function(data, response_var, explanatory_vars) {
  formula_str <- paste(response_var, "~", 
                       paste(explanatory_vars, collapse = " + "))
  model <- rlm(as.formula(formula_str), data = data)
  return(model)
}

run_polynomial_regression <- function(data, response_var, explanatory_vars, degree = 2) {
  poly_terms <- sapply(explanatory_vars, function(var) {
    paste0("poly(", var, ", ", degree, ")")
  })
  
  formula_str <- paste(response_var, "~", 
                       paste(poly_terms, collapse = " + "))
  model <- lm(as.formula(formula_str), data = data)
  return(model)
}

# Model Assumption Checks
check_model_assumptions <- function(model) {
  normality_test <- shapiro.test(residuals(model))
  homoscedasticity_test <- lmtest::bptest(model)
  
  vif_values <- tryCatch({
    vif(model)
  }, error = function(e) NULL)
  
  outliers <- which(abs(rstudent(model)) > 3)
  
  list(
    normality_test = normality_test,
    homoscedasticity_test = homoscedasticity_test,
    vif_values = vif_values,
    outliers = outliers
  )
}

# Model Comparison Function
compare_models <- function(models) {
  comparison_df <- data.frame(
    Model = names(models),
    R_Squared = sapply(models, function(m) summary(m)$r.squared),
    Adj_R_Squared = sapply(models, function(m) summary(m)$adj.r.squared),
    AIC = sapply(models, AIC),
    BIC = sapply(models, BIC)
  )
  
  return(comparison_df)
}

# Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Palmer Penguins Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("Exploratory Analysis", tabName = "exploratory", icon = icon("chart-bar")),
      menuItem("Statistical Modeling", tabName = "modeling", icon = icon("calculator")),
      menuItem("Model Comparison", tabName = "model_compare", icon = icon("chart-line")),
      menuItem("Data Transformation", tabName = "transformation", icon = icon("refresh"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Overview Tab
      tabItem(tabName = "data_overview",
              fluidRow(
                box(width = 12, 
                    title = "Palmer Penguins Dataset",
                    DTOutput("dataset_table") %>% withSpinner()
                )
              ),
              fluidRow(
                box(width = 6, title = "Dataset Summary", 
                    verbatimTextOutput("dataset_summary")),
                box(width = 6, title = "Missing Values", 
                    plotOutput("missing_values_plot"))
              )
      ),
      
      # Exploratory Analysis Tab
      tabItem(tabName = "exploratory",
              fluidRow(
                box(width = 4,
                    title = "Visualization Controls",
                    selectInput("x_var", "X Variable", 
                                choices = c("bill_length_mm", "bill_depth_mm", 
                                            "flipper_length_mm", "body_mass_g")),
                    selectInput("y_var", "Y Variable", 
                                choices = c("bill_depth_mm", "bill_length_mm", 
                                            "flipper_length_mm", "body_mass_g"),
                                selected = "body_mass_g"),
                    selectInput("color_var", "Color By", 
                                choices = c("species", "island", "sex"))
                ),
                box(width = 8,
                    title = "Scatter Plot",
                    plotlyOutput("scatter_plot") %>% withSpinner()
                )
              ),
              fluidRow(
                box(width = 6, 
                    title = "Box Plot", 
                    plotOutput("box_plot") %>% withSpinner()),
                box(width = 6, 
                    title = "Correlation Matrix", 
                    plotOutput("correlation_plot") %>% withSpinner())
              )
      ),
      
      # Statistical Modeling Tab
      tabItem(tabName = "modeling",
              fluidRow(
                box(width = 4,
                    title = "Model Configuration",
                    selectInput("response_var", "Response Variable", 
                                choices = c("body_mass_g", "bill_length_mm", 
                                            "bill_depth_mm", "flipper_length_mm")),
                    uiOutput("explanatory_vars_ui"),
                    selectInput("model_type", "Model Type",
                                choices = c("Linear Regression", 
                                            "Robust Regression", 
                                            "Polynomial Regression")),
                    actionButton("run_model", "Run Model")
                ),
                box(width = 8,
                    title = "Model Summary",
                    verbatimTextOutput("model_summary") %>% withSpinner()
                )
              ),
              fluidRow(
                box(width = 6,
                    title = "Model Diagnostics",
                    plotOutput("model_diagnostics") %>% withSpinner()),
                box(width = 6,
                    title = "Assumption Checks",
                    verbatimTextOutput("assumption_checks"))
              )
      ),
      
      # Model Comparison Tab
      tabItem(tabName = "model_compare",
              fluidRow(
                box(width = 12,
                    title = "Model Comparison",
                    DTOutput("model_comparison_table") %>% withSpinner())
              )
      ),
      
      # Data Transformation Tab
      tabItem(tabName = "transformation",
              fluidRow(
                box(width = 4,
                    title = "Transformation Controls",
                    selectInput("transform_var", "Variable to Transform", 
                                choices = c("bill_length_mm", "bill_depth_mm", 
                                            "flipper_length_mm", "body_mass_g")),
                    selectInput("transform_method", "Transformation Method",
                                choices = c("Log", "Square Root", "Box-Cox")),
                    actionButton("apply_transform", "Apply Transformation")
                ),
                box(width = 8,
                    title = "Transformation Results",
                    plotOutput("transformation_plot") %>% withSpinner(),
                    verbatimTextOutput("transformation_summary"))
              )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Reactive values for data and models
  rv <- reactiveValues(
    penguins_data = penguins %>% drop_na(),
    transformed_data = NULL,
    models = list()
  )
  
  # Data Overview Tab
  output$dataset_table <- renderDT({
    datatable(rv$penguins_data, 
              options = list(pageLength = 10, 
                             scrollX = TRUE))
  })
  
  output$dataset_summary <- renderPrint({
    summary(rv$penguins_data)
  })
  
  output$missing_values_plot <- renderPlot({
    penguins %>% 
      is.na() %>% 
      melt() %>% 
      ggplot(aes(x = Var2, y = Var1, fill = value)) +
      geom_tile() +
      scale_fill_manual(values = c("white", "red")) +
      theme_minimal() +
      labs(x = "Variables", y = "Observations", 
           title = "Missing Values Heatmap")
  })
  
  # Exploratory Analysis Tab
  output$scatter_plot <- renderPlotly({
    req(input$x_var, input$y_var, input$color_var)
    
    p <- ggplot(rv$penguins_data, 
                aes_string(x = input$x_var, 
                           y = input$y_var, 
                           color = input$color_var)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", input$x_var, "vs", input$y_var))
    
    ggplotly(p)
  })
  
  output$box_plot <- renderPlot({
    ggplot(rv$penguins_data, 
           aes(x = species, y = body_mass_g, fill = sex)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Body Mass by Species and Sex")
  })
  
  output$correlation_plot <- renderPlot({
    cor_matrix <- rv$penguins_data %>% 
      select(bill_length_mm, bill_depth_mm, 
             flipper_length_mm, body_mass_g) %>% 
      cor()
    
    corrplot(cor_matrix, method = "color")
  })
  
  # Modeling Tab Logic
  output$explanatory_vars_ui <- renderUI({
    checkboxGroupInput("explanatory_vars", 
                       "Explanatory Variables", 
                       choices = setdiff(
                         names(rv$penguins_data), 
                         input$response_var
                       ))
  })
  
  # Model Running
  observeEvent(input$run_model, {
    req(input$response_var, input$explanatory_vars)
    
    model <- switch(input$model_type,
                    "Linear Regression" = run_linear_regression(
                      rv$penguins_data, 
                      input$response_var, 
                      input$explanatory_vars
                    ),
                    "Robust Regression" = run_robust_regression(
                      rv$penguins_data, 
                      input$response_var, 
                      input$explanatory_vars
                    ),
                    "Polynomial Regression" = run_polynomial_regression(
                      rv$penguins_data, 
                      input$response_var, 
                      input$explanatory_vars
                    )
    )
    
    # Store model
    rv$models[[input$model_type]] <- model
  })
  
  # Model Summary Output
  output$model_summary <- renderPrint({
    req(input$run_model)
    summary(rv$models[[input$model_type]])
  })
  
  # Model Diagnostics
  output$model_diagnostics <- renderPlot({
    req(input$run_model)
    plot(rv$models[[input$model_type]])
  })
  
  # Assumption Checks
  output$assumption_checks <- renderPrint({
    req(input$run_model)
    check_model_assumptions(rv$models[[input$model_type]])
  })
  
  # Model Comparison Table
  output$model_comparison_table <- renderDT({
    req(length(rv$models) > 0)
    compare_models(rv$models)
  })
  
  # Data Transformation Tab
  observeEvent(input$apply_transform, {
    transform_method <- switch(input$transform_method,
                               "Log" = "log",
                               "Square Root" = "sqrt",
                               "Box-Cox" = "box_cox")
    
    rv$transformed_data <- apply_transformation(
      rv$penguins_data, 
      input$transform_var, 
      transform_method
    )
  })
  
  output$transformation_plot <- renderPlot({
    req(rv$transformed_data)
    
    par(mfrow = c(1, 2))
    # Original Distribution
    hist(rv$penguins_data[[input$transform_var]], 
         main = "Original Distribution", 
         xlab = input$transform_var)
    
    # Transformed Distribution
    hist(rv$transformed_data[[input$transform_var]], 
         main = "Transformed Distribution", 
         xlab = paste("Transformed", input$transform_var))
  })
  
  output$transformation_summary <- renderPrint({
    req(rv$transformed_data)
    
    cat("Transformation Method:", input$transform_method, "\n")
    cat("\nOriginal Variable Summary:\n")
    print(summary(rv$penguins_data[[input$transform_var]]))
    
    cat("\nTransformed Variable Summary:\n")
    print(summary(rv$transformed_data[[input$transform_var]]))
    
    cat("\nSkewness:\n")
    cat("Original:", moments::skewness(rv$penguins_data[[input$transform_var]]), "\n")
    cat("Transformed:", moments::skewness(rv$transformed_data[[input$transform_var]]), "\n")
  })
}

# Run the application 
shinyApp(ui, server)
- `analysis.R`: # Palmer Penguins Data Analysis and Preprocessing

# Required Libraries
library(palmerpenguins)
library(tidyverse)
library(mice)
library(shiny)
library(ggplot2)
library(gridExtra)

# Load the dataset
data(penguins)

# Exploratory Data Analysis: Missing Value Analysis
# Examine the missing values
missing_values <- sapply(penguins, function(x) sum(is.na(x)))
print("Missing Values per Column:")
print(missing_values)

# Numerical Variable Imputation Strategy
# Impute missing numerical values by median, grouped by species. 
# I chose to do it this way because the median is strong to outliers, unlike the mean, making it a better choice for imputation.
# I chose to group by species since penguin species differ in physical traits, using the median within each species allows for more accurate imputation.
impute_numeric <- function(data) {
  # Create a copy of the dataset
  imputed_data <- data
  
  # Numerical columns to impute
  num_cols <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
  
  for(col in num_cols) {
    imputed_data[[col]] <- ifelse(is.na(imputed_data[[col]]), 
                                  ave(imputed_data[[col]], imputed_data$species, 
                                      FUN = function(x) median(x, na.rm = TRUE)),
                                  imputed_data[[col]])
  }
  
  return(imputed_data)
}

# Categorical Variable Imputation Strategy
# I chose to use the mode of the variable to replace missing values because mode is simpler and effective in most cases.Categorical data doesn't have numerical properties, so the mode is a logical choice for replacing missing values with the most frequent category.
impute_categorical <- function(data) {
  # Create a copy of the dataset
  imputed_data <- data
  
  # For sex, we'll use mode by species
  imputed_data$sex <- ifelse(is.na(imputed_data$sex), 
                             ave(imputed_data$sex, imputed_data$species, 
                                 FUN = function(x) names(table(x)[which.max(table(x))])),
                             imputed_data$sex)
  
  return(imputed_data)
}

# Apply imputation
penguins_imputed <- penguins %>%
  impute_numeric() %>%
  impute_categorical()


# Verification of imputation
print("Missing Values After Imputation:")
print(sapply(penguins_imputed, function(x) sum(is.na(x))))



##Plots 
# Save boxplots
ggsave("boxplots.png", 
       plot = do.call(grid.arrange, c(lapply(c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"), 
                                             function(col) {
                                               ggplot(penguins_imputed, aes_string(x = "species", y = col, fill = "species")) +
                                                 geom_boxplot() +
                                                 theme_minimal() +
                                                 labs(title = paste("Boxplot of", col, "by Species"))
                                             }), 
                                      ncol = 2)),
       width = 10, height = 8)

# Save scatterplots
ggsave("scatterplots.png", 
       plot = grid.arrange(
         ggplot(penguins_imputed, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
           geom_point(alpha = 0.7) +
           labs(title = "Bill Length vs Bill Depth") +
           theme_minimal(),
         ggplot(penguins_imputed, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
           geom_point(alpha = 0.7) +
           labs(title = "Flipper Length vs Body Mass") +
           theme_minimal(),
         ncol = 2
       ),
       width = 10, height = 5)


# Exploratory Data Analysis Functions 
create_boxplots <- function(data) {
  # Numeric columns for boxplots
  num_cols <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
  
  # Create boxplots by species
  plots <- lapply(num_cols, function(col) {
    ggplot(data, aes_string(x = "species", y = col, fill = "species")) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boxplot of", col, "by Species"))
  })
  
  # Arrange plots
  grid.arrange(grob = plots, ncol = 2)
}

create_scatterplots <- function(data) {
  # Scatter plots exploring relationships
  p1 <- ggplot(data, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
    geom_point() +
    labs(title = "Bill Length vs Bill Depth")
  
  p2 <- ggplot(data, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
    geom_point() +
    labs(title = "Flipper Length vs Body Mass")
  
  grid.arrange(p1, p2, ncol = 2)
}


## Prerequisites
- R (version X.X.X or later)
- Required R packages:
  - shiny + PLUS MANY MORE (SEE SCRIPT)
  - [list other specific packages]

## Installation
1. Clone the repository:

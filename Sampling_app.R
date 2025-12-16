install.packages("shiny")
install.packages("shinydashboard")
install.packages("DT")

install.packages("shiny")
install.packages("shinydashboard")

#

# Stratified Sampling Calculator - R Shiny App
# Save this as app.R

library(shiny)
library(shinydashboard)
library(DT)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stratified Sampling Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Calculator Tab
      tabItem(tabName = "calculator",
              fluidRow(
                box(
                  title = "Global Parameters", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  column(6,
                         numericInput("precision", "Desired Precision (d):", 5, min = 0.1, step = 0.1)
                  ),
                  column(6,
                         numericInput("z_value", "Z-value (95% CI = 1.96):", 1.96, min = 0.1, step = 0.01)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Stratum 1", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("N1", "Population (N):", 1000, min = 1),
                  numericInput("var1", "Variance (σ²):", 100, min = 0),
                  numericInput("cost1", "Cost per unit:", 10, min = 0),
                  numericInput("time1", "Time per unit:", 5, min = 0)
                ),
                
                box(
                  title = "Stratum 2", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("N2", "Population (N):", 1500, min = 1),
                  numericInput("var2", "Variance (σ²):", 200, min = 0),
                  numericInput("cost2", "Cost per unit:", 15, min = 0),
                  numericInput("time2", "Time per unit:", 8, min = 0)
                ),
                
                box(
                  title = "Stratum 3", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("N3", "Population (N):", 2000, min = 1),
                  numericInput("var3", "Variance (σ²):", 150, min = 0),
                  numericInput("cost3", "Cost per unit:", 12, min = 0),
                  numericInput("time3", "Time per unit:", 6, min = 0)
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  actionButton("calculate", "Calculate Sample Allocations", 
                               class = "btn-primary btn-lg btn-block",
                               icon = icon("calculator"))
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("total_prop", width = 4),
                valueBoxOutput("total_neyman", width = 4),
                valueBoxOutput("total_optimised", width = 4)
              ),
              
              fluidRow(
                box(
                  title = "Proportional Allocation",
                  status = "success",
                  solidHeader = TRUE,
                  width = 4,
                  DTOutput("table_prop")
                ),
                
                box(
                  title = "Neyman Allocation",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  DTOutput("table_neyman")
                ),
                
                box(
                  title = "Optimised Allocation",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 4,
                  DTOutput("table_optimised")
                )
              ),
              
              fluidRow(
                box(
                  title = "Cost & Time Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("comparison_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Summary",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("summary")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  downloadButton("download_results", "Download Results", 
                                 class = "btn-success btn-lg")
                )
              )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
              fluidRow(
                box(
                  title = "About This Application",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Stratified Sampling Calculator"),
                  p("This application calculates required sample sizes for stratified sampling under three allocation methods:"),
                  tags$ul(
                    tags$li(tags$b("Proportional Allocation:"), "Sample size proportional to stratum size"),
                    tags$li(tags$b("Neyman Allocation:"), "Optimal allocation minimizing variance"),
                    tags$li(tags$b("Optimised Allocation:"), "Cost-efficient allocation considering variance and cost")
                  ),
                  h4("How to Use:"),
                  tags$ol(
                    tags$li("Enter desired precision and Z-value in Global Parameters"),
                    tags$li("Input population size, variance, cost, and time for each stratum"),
                    tags$li("Click 'Calculate Sample Allocations'"),
                    tags$li("View results in the Results tab"),
                    tags$li("Download results as needed")
                  ),
                  h4("Formulas Used:"),
                  tags$ul(
                    tags$li(tags$b("Proportional:"), "n_h = n × (N_h / N)"),
                    tags$li(tags$b("Neyman:"), "n_h = n × (N_h × σ_h) / Σ(N_h × σ_h)"),
                    tags$li(tags$b("Optimised:"), "n_h = n × (N_h × σ_h / √c_h) / Σ(N_h × σ_h / √c_h)")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store results
  results <- reactiveValues(
    proportional = NULL,
    neyman = NULL,
    optimised = NULL,
    calculated = FALSE
  )
  
  # Calculate allocations
  observeEvent(input$calculate, {
    # Collect stratum data
    strata <- data.frame(
      Stratum = c("Stratum 1", "Stratum 2", "Stratum 3"),
      N = c(input$N1, input$N2, input$N3),
      Variance = c(input$var1, input$var2, input$var3),
      Cost = c(input$cost1, input$cost2, input$cost3),
      Time = c(input$time1, input$time2, input$time3)
    )
    
    total_N <- sum(strata$N)
    Z <- input$z_value
    d <- input$precision
    
    # Proportional Allocation
    weights_prop <- strata$N / total_N
    var_sum_prop <- sum(weights_prop^2 * strata$Variance)
    n_prop <- (Z^2 * var_sum_prop) / (d^2)
    
    prop_alloc <- ceiling(n_prop * weights_prop)
    total_prop <- sum(prop_alloc)
    cost_prop <- sum(prop_alloc * strata$Cost)
    time_prop <- sum(prop_alloc * strata$Time)
    
    results$proportional <- data.frame(
      Stratum = strata$Stratum,
      Allocation = prop_alloc,
      stringsAsFactors = FALSE
    )
    
    # Neyman Allocation
    neyman_numerator <- sum(strata$N * sqrt(strata$Variance))
    var_sum_neyman <- sum(strata$N * sqrt(strata$Variance))
    n_neyman <- (Z^2 * var_sum_neyman^2) / (d^2 * total_N^2)
    
    neyman_alloc <- ceiling(n_neyman * (strata$N * sqrt(strata$Variance)) / neyman_numerator)
    total_neyman <- sum(neyman_alloc)
    cost_neyman <- sum(neyman_alloc * strata$Cost)
    time_neyman <- sum(neyman_alloc * strata$Time)
    
    results$neyman <- data.frame(
      Stratum = strata$Stratum,
      Allocation = neyman_alloc,
      stringsAsFactors = FALSE
    )
    
    # Optimised Allocation
    optimised_numerator <- sum(strata$N * sqrt(strata$Variance / strata$Cost))
    var_sum_optimised <- sum(strata$N * sqrt(strata$Variance * strata$Cost))
    n_optimised <- (Z^2 * var_sum_optimised * optimised_numerator) / (d^2 * total_N^2)
    
    optimised_alloc <- ceiling(n_optimised * (strata$N * sqrt(strata$Variance / strata$Cost)) / optimised_numerator)
    total_optimised <- sum(optimised_alloc)
    cost_optimised <- sum(optimised_alloc * strata$Cost)
    time_optimised <- sum(optimised_alloc * strata$Time)
    
    results$optimised <- data.frame(
      Stratum = strata$Stratum,
      Allocation = optimised_alloc,
      stringsAsFactors = FALSE
    )
    
    # Store comparison data
    results$comparison <- data.frame(
      Method = c("Proportional", "Neyman", "Optimised"),
      Total_Samples = c(total_prop, total_neyman, total_optimised),
      Total_Cost = c(cost_prop, cost_neyman, cost_optimised),
      Total_Time = c(time_prop, time_neyman, time_optimised),
      stringsAsFactors = FALSE
    )
    
    results$calculated <- TRUE
    
    # Switch to results tab
    updateTabItems(session, "sidebarMenu", "results")
    
    showNotification("Calculations completed successfully!", type = "message")
  })
  
  # Value boxes
  output$total_prop <- renderValueBox({
    if (results$calculated) {
      valueBox(
        sum(results$proportional$Allocation),
        "Proportional Total",
        icon = icon("users"),
        color = "green"
      )
    }
  })
  
  output$total_neyman <- renderValueBox({
    if (results$calculated) {
      valueBox(
        sum(results$neyman$Allocation),
        "Neyman Total",
        icon = icon("chart-line"),
        color = "yellow"
      )
    }
  })
  
  output$total_optimised <- renderValueBox({
    if (results$calculated) {
      valueBox(
        sum(results$optimised$Allocation),
        "Optimised Total",
        icon = icon("dollar-sign"),
        color = "red"
      )
    }
  })
  
  # Tables
  output$table_prop <- renderDT({
    req(results$calculated)
    datatable(results$proportional, options = list(dom = 't', pageLength = 10))
  })
  
  output$table_neyman <- renderDT({
    req(results$calculated)
    datatable(results$neyman, options = list(dom = 't', pageLength = 10))
  })
  
  output$table_optimised <- renderDT({
    req(results$calculated)
    datatable(results$optimised, options = list(dom = 't', pageLength = 10))
  })
  
  output$comparison_table <- renderDT({
    req(results$calculated)
    datatable(results$comparison, 
              options = list(dom = 't', pageLength = 10),
              rownames = FALSE)
  })
  
  # Summary
  output$summary <- renderText({
    req(results$calculated)
    
    comparison <- results$comparison
    
    min_cost_method <- comparison$Method[which.min(comparison$Total_Cost)]
    min_time_method <- comparison$Method[which.min(comparison$Total_Time)]
    min_sample_method <- comparison$Method[which.min(comparison$Total_Samples)]
    
    paste0(
      "SUMMARY:\n",
      "========================================\n",
      "Most Cost-Efficient: ", min_cost_method, "\n",
      "Most Time-Efficient: ", min_time_method, "\n",
      "Smallest Sample Size: ", min_sample_method, "\n\n",
      "Recommendation: ",
      ifelse(min_cost_method == min_time_method && min_cost_method == min_sample_method,
             paste0("Use ", min_cost_method, " allocation as it's optimal across all criteria."),
             "Consider trade-offs between cost, time, and sample size based on your priorities.")
    )
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("stratified_sampling_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(results$calculated)
      
      content <- paste0(
        "STRATIFIED SAMPLING ANALYSIS RESULTS\n",
        "====================================\n\n",
        "Date: ", Sys.Date(), "\n",
        "Desired Precision (d): ", input$precision, "\n",
        "Z-value: ", input$z_value, "\n\n",
        "INPUT DATA:\n",
        "-----------\n"
      )
      
      for (i in 1:3) {
        content <- paste0(content,
                          "\nStratum ", i, ":\n",
                          "  Population (N): ", get(paste0("input$N", i)), "\n",
                          "  Variance (σ²): ", get(paste0("input$var", i)), "\n",
                          "  Cost per unit: ", get(paste0("input$cost", i)), "\n",
                          "  Time per unit: ", get(paste0("input$time", i)), "\n"
        )
      }
      
      content <- paste0(content,
                        "\n\nALLOCATION RESULTS:\n",
                        "-------------------\n\n"
      )
      
      methods <- list(
        list(name = "Proportional", data = results$proportional),
        list(name = "Neyman", data = results$neyman),
        list(name = "Optimised", data = results$optimised)
      )
      
      for (method in methods) {
        content <- paste0(content, method$name, " Allocation:\n")
        for (j in 1:nrow(method$data)) {
          content <- paste0(content,
                            "  ", method$data$Stratum[j], ": ", method$data$Allocation[j], " samples\n"
          )
        }
        
        comp_row <- results$comparison[results$comparison$Method == method$name, ]
        content <- paste0(content,
                          "  Total: ", comp_row$Total_Samples, " samples\n",
                          "  Total Cost: ", comp_row$Total_Cost, "\n",
                          "  Total Time: ", comp_row$Total_Time, "\n\n"
        )
      }
      
      writeLines(content, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)


#
# Stratified Sampling Calculator - R Shiny App
# Save this as app.R

library(shiny)
library(shinydashboard)
library(DT)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Stratified Sampling Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Calculator Tab
      tabItem(tabName = "calculator",
              fluidRow(
                box(
                  title = "Global Parameters", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  column(6,
                         numericInput("precision", "Desired Precision (d):", 5, min = 0.1, step = 0.1)
                  ),
                  column(6,
                         numericInput("z_value", "Z-value (95% CI = 1.96):", 1.96, min = 0.1, step = 0.01)
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "Stratum 1", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("N1", "Population (N):", 1000, min = 1),
                  numericInput("var1", "Variance (σ²):", 100, min = 0),
                  numericInput("cost1", "Cost per unit:", 10, min = 0),
                  numericInput("time1", "Time per unit:", 5, min = 0)
                ),
                
                box(
                  title = "Stratum 2", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("N2", "Population (N):", 1500, min = 1),
                  numericInput("var2", "Variance (σ²):", 200, min = 0),
                  numericInput("cost2", "Cost per unit:", 15, min = 0),
                  numericInput("time2", "Time per unit:", 8, min = 0)
                ),
                
                box(
                  title = "Stratum 3", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  numericInput("N3", "Population (N):", 2000, min = 1),
                  numericInput("var3", "Variance (σ²):", 150, min = 0),
                  numericInput("cost3", "Cost per unit:", 12, min = 0),
                  numericInput("time3", "Time per unit:", 6, min = 0)
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  actionButton("calculate", "Calculate Sample Allocations", 
                               class = "btn-primary btn-lg btn-block",
                               icon = icon("calculator"))
                )
              )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("total_prop", width = 4),
                valueBoxOutput("total_neyman", width = 4),
                valueBoxOutput("total_optimised", width = 4)
              ),
              
              fluidRow(
                box(
                  title = "Proportional Allocation",
                  status = "success",
                  solidHeader = TRUE,
                  width = 4,
                  DTOutput("table_prop")
                ),
                
                box(
                  title = "Neyman Allocation",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  DTOutput("table_neyman")
                ),
                
                box(
                  title = "Optimised Allocation",
                  status = "danger",
                  solidHeader = TRUE,
                  width = 4,
                  DTOutput("table_optimised")
                )
              ),
              
              fluidRow(
                box(
                  title = "Cost & Time Comparison",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("comparison_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Summary",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("summary")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  downloadButton("download_results", "Download Results", 
                                 class = "btn-success btn-lg")
                )
              )
      ),
      
      # Help Tab
      tabItem(tabName = "help",
              fluidRow(
                box(
                  title = "About This Application",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Stratified Sampling Calculator"),
                  p("This application calculates required sample sizes for stratified sampling under three allocation methods:"),
                  tags$ul(
                    tags$li(tags$b("Proportional Allocation:"), "Sample size proportional to stratum size"),
                    tags$li(tags$b("Neyman Allocation:"), "Optimal allocation minimizing variance"),
                    tags$li(tags$b("Optimised Allocation:"), "Cost-efficient allocation considering variance and cost")
                  ),
                  h4("How to Use:"),
                  tags$ol(
                    tags$li("Enter desired precision and Z-value in Global Parameters"),
                    tags$li("Input population size, variance, cost, and time for each stratum"),
                    tags$li("Click 'Calculate Sample Allocations'"),
                    tags$li("View results in the Results tab"),
                    tags$li("Download results as needed")
                  ),
                  h4("Formulas Used:"),
                  tags$ul(
                    tags$li(tags$b("Proportional:"), "n_h = n × (N_h / N)"),
                    tags$li(tags$b("Neyman:"), "n_h = n × (N_h × σ_h) / Σ(N_h × σ_h)"),
                    tags$li(tags$b("Optimised:"), "n_h = n × (N_h × σ_h / √c_h) / Σ(N_h × σ_h / √c_h)")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store results
  results <- reactiveValues(
    proportional = NULL,
    neyman = NULL,
    optimised = NULL,
    calculated = FALSE
  )
  
  # Calculate allocations
  observeEvent(input$calculate, {
    # Collect stratum data
    strata <- data.frame(
      Stratum = c("Stratum 1", "Stratum 2", "Stratum 3"),
      N = c(input$N1, input$N2, input$N3),
      Variance = c(input$var1, input$var2, input$var3),
      Cost = c(input$cost1, input$cost2, input$cost3),
      Time = c(input$time1, input$time2, input$time3)
    )
    
    total_N <- sum(strata$N)
    Z <- input$z_value
    d <- input$precision
    
    # Proportional Allocation
    weights_prop <- strata$N / total_N
    var_sum_prop <- sum(weights_prop^2 * strata$Variance)
    n_prop <- (Z^2 * var_sum_prop) / (d^2)
    
    prop_alloc <- ceiling(n_prop * weights_prop)
    total_prop <- sum(prop_alloc)
    cost_prop <- sum(prop_alloc * strata$Cost)
    time_prop <- sum(prop_alloc * strata$Time)
    
    results$proportional <- data.frame(
      Stratum = strata$Stratum,
      Allocation = prop_alloc,
      stringsAsFactors = FALSE
    )
    
    # Neyman Allocation
    neyman_numerator <- sum(strata$N * sqrt(strata$Variance))
    var_sum_neyman <- sum(strata$N * sqrt(strata$Variance))
    n_neyman <- (Z^2 * var_sum_neyman^2) / (d^2 * total_N^2)
    
    neyman_alloc <- ceiling(n_neyman * (strata$N * sqrt(strata$Variance)) / neyman_numerator)
    total_neyman <- sum(neyman_alloc)
    cost_neyman <- sum(neyman_alloc * strata$Cost)
    time_neyman <- sum(neyman_alloc * strata$Time)
    
    results$neyman <- data.frame(
      Stratum = strata$Stratum,
      Allocation = neyman_alloc,
      stringsAsFactors = FALSE
    )
    
    # Optimised Allocation
    optimised_numerator <- sum(strata$N * sqrt(strata$Variance / strata$Cost))
    var_sum_optimised <- sum(strata$N * sqrt(strata$Variance * strata$Cost))
    n_optimised <- (Z^2 * var_sum_optimised * optimised_numerator) / (d^2 * total_N^2)
    
    optimised_alloc <- ceiling(n_optimised * (strata$N * sqrt(strata$Variance / strata$Cost)) / optimised_numerator)
    total_optimised <- sum(optimised_alloc)
    cost_optimised <- sum(optimised_alloc * strata$Cost)
    time_optimised <- sum(optimised_alloc * strata$Time)
    
    results$optimised <- data.frame(
      Stratum = strata$Stratum,
      Allocation = optimised_alloc,
      stringsAsFactors = FALSE
    )
    
    # Store comparison data
    results$comparison <- data.frame(
      Method = c("Proportional", "Neyman", "Optimised"),
      Total_Samples = c(total_prop, total_neyman, total_optimised),
      Total_Cost = c(cost_prop, cost_neyman, cost_optimised),
      Total_Time = c(time_prop, time_neyman, time_optimised),
      stringsAsFactors = FALSE
    )
    
    results$calculated <- TRUE
    
    # Switch to results tab
    updateTabItems(session, "tabs", "results")
    
    showNotification("Calculations completed successfully!", type = "message")
  })
  
  # Value boxes
  output$total_prop <- renderValueBox({
    if (results$calculated) {
      valueBox(
        sum(results$proportional$Allocation),
        "Proportional Total",
        icon = icon("users"),
        color = "green"
      )
    }
  })
  
  output$total_neyman <- renderValueBox({
    if (results$calculated) {
      valueBox(
        sum(results$neyman$Allocation),
        "Neyman Total",
        icon = icon("chart-line"),
        color = "yellow"
      )
    }
  })
  
  output$total_optimised <- renderValueBox({
    if (results$calculated) {
      valueBox(
        sum(results$optimised$Allocation),
        "Optimised Total",
        icon = icon("dollar-sign"),
        color = "red"
      )
    }
  })
  
  # Tables
  output$table_prop <- renderDT({
    req(results$calculated)
    datatable(results$proportional, options = list(dom = 't', pageLength = 10))
  })
  
  output$table_neyman <- renderDT({
    req(results$calculated)
    datatable(results$neyman, options = list(dom = 't', pageLength = 10))
  })
  
  output$table_optimised <- renderDT({
    req(results$calculated)
    datatable(results$optimised, options = list(dom = 't', pageLength = 10))
  })
  
  output$comparison_table <- renderDT({
    req(results$calculated)
    datatable(results$comparison, 
              options = list(dom = 't', pageLength = 10),
              rownames = FALSE)
  })
  
  # Summary
  output$summary <- renderText({
    req(results$calculated)
    
    comparison <- results$comparison
    
    min_cost_method <- comparison$Method[which.min(comparison$Total_Cost)]
    min_time_method <- comparison$Method[which.min(comparison$Total_Time)]
    min_sample_method <- comparison$Method[which.min(comparison$Total_Samples)]
    
    paste0(
      "SUMMARY:\n",
      "========================================\n",
      "Most Cost-Efficient: ", min_cost_method, "\n",
      "Most Time-Efficient: ", min_time_method, "\n",
      "Smallest Sample Size: ", min_sample_method, "\n\n",
      "Recommendation: ",
      ifelse(min_cost_method == min_time_method && min_cost_method == min_sample_method,
             paste0("Use ", min_cost_method, " allocation as it's optimal across all criteria."),
             "Consider trade-offs between cost, time, and sample size based on your priorities.")
    )
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("stratified_sampling_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(results$calculated)
      
      content <- paste0(
        "STRATIFIED SAMPLING ANALYSIS RESULTS\n",
        "====================================\n\n",
        "Date: ", Sys.Date(), "\n",
        "Desired Precision (d): ", input$precision, "\n",
        "Z-value: ", input$z_value, "\n\n",
        "INPUT DATA:\n",
        "-----------\n"
      )
      
      for (i in 1:3) {
        n_val <- switch(i, input$N1, input$N2, input$N3)
        var_val <- switch(i, input$var1, input$var2, input$var3)
        cost_val <- switch(i, input$cost1, input$cost2, input$cost3)
        time_val <- switch(i, input$time1, input$time2, input$time3)
        
        content <- paste0(content,
                          "\nStratum ", i, ":\n",
                          "  Population (N): ", n_val, "\n",
                          "  Variance (σ²): ", var_val, "\n",
                          "  Cost per unit: ", cost_val, "\n",
                          "  Time per unit: ", time_val, "\n"
        )
      }
      
      content <- paste0(content,
                        "\n\nALLOCATION RESULTS:\n",
                        "-------------------\n\n"
      )
      
      methods <- list(
        list(name = "Proportional", data = results$proportional),
        list(name = "Neyman", data = results$neyman),
        list(name = "Optimised", data = results$optimised)
      )
      
      for (method in methods) {
        content <- paste0(content, method$name, " Allocation:\n")
        for (j in 1:nrow(method$data)) {
          content <- paste0(content,
                            "  ", method$data$Stratum[j], ": ", method$data$Allocation[j], " samples\n"
          )
        }
        
        comp_row <- results$comparison[results$comparison$Method == method$name, ]
        content <- paste0(content,
                          "  Total: ", comp_row$Total_Samples, " samples\n",
                          "  Total Cost: ", comp_row$Total_Cost, "\n",
                          "  Total Time: ", comp_row$Total_Time, "\n\n"
        )
      }
      
      writeLines(content, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)


remove.packages("shiny")
install.packages("shiny", dependencies = TRUE)
library(shiny)

install.packages("shinydashboard", dependencies = TRUE)
# Type 'no' when asked

install.packages("DT", dependencies = TRUE)
# Type 'no' when asked

r

rno
Do you want to install from sources the packages which need compilation? (Yes/no/cancel)


install.packages("shinydashboard", dependencies = TRUE)
# Type 'no' when asked

install.packages("DT", dependencies = TRUE)
# Type 'no' when asked

install.packages("shinydashboard", dependencies = TRUE)
# Type 'no' when asked

install.packages("DT", dependencies = TRUE)
# Type 'no' when asked
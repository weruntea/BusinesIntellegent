# Load required libraries
library(shiny)
library(ggplot2)

# Sample data
ad_data <- data.frame(
  Day = 1:10,
  Left_Sidebar = c(2.5, 2.7, 2.8, 2.6, 3, 2.4, 2.9, 2.5, 2.6, 2.7),
  Center_Page = c(3.8, 3.5, 4, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9),
  Right_Sidebar = c(3.1, 2.9, 3, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
)

# UI
ui <- fluidPage(
  titlePanel("Product Placement Effectiveness Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input Data"),
      tableOutput("data_table"),
      
      checkboxGroupInput("placement_var", "Select Ad Placement Location(s)",
                         choices = colnames(ad_data)[-1]),
      
      selectInput("ctr_var", "Select Click Through Rate Variable",
                  choices = colnames(ad_data)[-1]),
      
      actionButton("analyze_btn", "Analyze"),
      hr(),
      helpText("Note: The dataset contains CTR data for different ad placement locations.")
    ),
    
    mainPanel(
      plotOutput("scatterplot"),
      verbatimTextOutput("summary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Display the sample data table
  output$data_table <- renderTable({
    ad_data
  })
  
  # Analyze button action
  observeEvent(input$analyze_btn, {
    req(input$placement_var, input$ctr_var)
    
    selected_vars <- c(input$ctr_var, input$placement_var)
    selected_data <- ad_data[, selected_vars]
    
    # Fit linear regression model
    formula <- as.formula(paste(selected_vars[1], paste(selected_vars[-1], collapse = " + "), sep = " ~ "))
    fit <- lm(formula, data = selected_data)
    
    # Output Scatterplot
    output$scatterplot <- renderPlot({
      ggplot(selected_data, aes_string(x = input$placement_var, y = input$ctr_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = "Product Placement Effectiveness Analysis",
             x = input$placement_var,
             y = input$ctr_var)
    })
    
    # Output Regression Summary
    output$summary <- renderPrint({
      summary(fit)
    })
  })
}

# Run the application
shinyApp(ui, server)

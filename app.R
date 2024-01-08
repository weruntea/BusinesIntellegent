library(shiny)
library(markdown)
library(ggplot2)
library(shinythemes)
library(rsconnect)
library(shinyalert)

# Define UI for application that draws a histogram
serverUI<-fluidPage(theme = shinytheme("united"),
                    navbarPage("Sales Prediction",
                               tabPanel("Datasets",
                                        sidebarLayout(
                                          sidebarPanel(
                                            fileInput("file1", "Choose CSV File",
                                                      multiple = FALSE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv")),
                                            tags$hr(),
                                            checkboxInput("header", "Header", TRUE),
                                            radioButtons("sep", "separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";",
                                                                     Tab = "\t"),
                                                         selected = ","),
                                            radioButtons("quote", "Quote",
                                                         choices= c(none = "",
                                                                    "Double Quote" = '"',
                                                                    "Single Quote" = "'"),
                                                         selected = '"'),
                                            tags$hr(),
                                            radioButtons("disp", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")
                                            
                                          ),
                                          mainPanel(
                                            tags$h1("Tabel Output"),
                                            #Output tabel
                                            tableOutput("contents"),
                                            tags$h1("Statistika Deskriptif"),
                                            #Output Statistika deskriptif
                                            verbatimTextOutput("summ")
                                          ),
                                        )
                                        
                               ),
                               tabPanel("Correlation Matrix", plotOutput("correlation_matrix")),
                               tabPanel("Regresi",
                                        sidebarLayout(
                                          sidebarPanel(
                                            textInput("Y", "Enter Variabel Dependen"),
                                            textInput("X1", "Enter Variabel Independen"),
                                            textInput("X2", "Enter Variabe; Independen")
                                          ),
                                          mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Model Summary", verbatimTextOutput("Summary")),
                                                        tabPanel("Normalitas Residual", verbatimTextOutput("norm")),
                                                        tabPanel("Multikolinearitas", verbatimTextOutput("multi")),
                                                        tabPanel("Autokorelasi", verbatimTextOutput("auto")),
                                                        tabPanel("Heterokedastisitas", verbatimTextOutput("hete")),
                                                        tabPanel("Distribution",
                                                                 fluidRow(
                                                                   column(8, plotOutput("distribution1")),
                                                                   column(8, plotOutput("distribution2")),
                                                                   column(8, plotOutput("distribution3"))
                                                                 ))
                                            )
                                          )
                                        )
                               ),
                               tabPanel("Prediction",
                                        fluidPage(
                                          titlePanel(title = div("Monthly Sales Volume", style = "color: #333333; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
                                          sidebarLayout(
                                            sidebarPanel(
                                              numericInput("x1.input", "x1 Value:", value = 150000),
                                              numericInput("x2.input", "x2 Value:", value = 8000),
                                              numericInput("x3.input", "x3 Value:", value = 5),
                                              numericInput("x4.input", "x4 Value:", value = 8.5),
                                              numericInput("x5.input", "x5 Value:", value = 20000),
                                              actionButton("predict_button", "Generate Predict", class = "btn-primary")
                                            ),
                                            
                                            mainPanel(
                                              plotOutput("Monthly_Sales_Volume"),
                                              verbatimTextOutput("prediction_output")
                                            )
                                          )
                                        )
                               )
                               
                    )
)

library(shiny)
library(psych)
library(GGally)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #Input Data
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  #Statistika Deskriptif
  output$summ<-renderPrint({
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    describe(df)
  })
  
  output$Caption <- renderText({
    req(input$caption)
  })
  
  # Render correlation matrix
  output$correlation_matrix <- renderPlot({
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    tryCatch({
      # Calculate the correlation matrix
      correlation_matrix <- round(cor(df), 2)
      
      # Plot the correlation matrix as a heatmap
      heatmap(correlation_matrix,
              col = colorRampPalette(c("red", "white", "blue"))(20),
              main = "Correlation Matrix",
              xlab = "Variables",
              ylab = "Variables")
    }, error = function(e) {
      # Display an error message if there's an issue
      errorMessage <- paste("Error in rendering correlation matrix:", e$message)
      shinyalert::shinyalert(
        title = "Error",
        text = errorMessage,
        type = "error"
      )
    })
  })
  
  #Model SUmmary
  output$Summary <- renderPrint({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        fit = lm(df[,input$Y]~df[, input$X1]+df[, input$X2], data = df)
        names(fit$coefficient) <- c("Intercept", input$X1, input$X2)
        summary(fit)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        print("Masukkan Variabel terlebih dahulu")
      }
    )
  })
  
  #Normalitas Residual
  output$norm <- renderPrint({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) 
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        res = residuals(lm(df[,input$Y]~df[,input$X1]+df[,input$X2]))
        shapiro.test(res)  
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  #Uji Multikolinearitas
  output$multi <- renderPrint({
    require(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)  
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        model = (lm(df[,input$Y]~df[,input$X1]+df[,input$X2]))
        vif(model)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  #Uji Autokorelasi
  output$auto <- renderPrint({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        model = (lm(df[,input$Y]~df[,input$X1]+df[,input$X2]))
        bgtest(model)  
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  #Uji Heterokedastisitas
  output$hete <- renderPrint({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        model = (lm(df[,input$Y]~df[,input$X1]+df[,input$X2]))
        bptest(model) 
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  #Distribution of Data
  output$distribution1 <- renderPlot({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) 
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        ggplot(df, aes(df[,input$Y], fill=df[,input$Y]))+
          geom_bar(bins = 10, col="blue",aes(fill=..count..))+
          labs(title = "Grafik Histogram")+labs(x="df[,input$Y]", y="Jumlah")+
          scale_fill_gradient("df[,input$Y]", low = "yellow", high = "red")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$distribution2 <- renderPlot({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) 
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        ggplot(df, aes(df[,input$X1], fill=df[,input$X1]))+
          geom_histogram(bins = 10, col="blue",aes(fill=..count..))+
          labs(title = "Grafik Histogram")+labs(x="df[,input$Y]", y="Jumlah")+
          scale_fill_gradient("df[,input$X1]", low = "yellow", high = "red")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$distribution3 <- renderPlot({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote) 
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    tryCatch(
      {
        ggplot(df, aes(df[,input$X2], fill=df[,input$X2]))+
          geom_histogram(bins = 10, col="blue",aes(fill=..count..))+
          labs(title = "Grafik Histogram")+labs(x="df[,input$Y]", y="Jumlah")+
          scale_fill_gradient("df[,input$X2]", low = "yellow", high = "red")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  # Prediction 
  observeEvent(input$predict_button, {
    new_data <- data.frame(
      x1 = input$x1.input,
      x2 = input$x2.input,
      x3 = input$x3.input,
      x4 = input$x4.input,
      x5 = input$x5.input
    )
    
    # Predicted predicted_Monthly_Sales_Volume
    predicted_Monthly_Sales_Volume <- predict(y_Monthly_Sales_Volume, newdata = new_data)
    predicted_Monthly_Sales_Volume <- abs(predicted_Monthly_Sales_Volume)
    predicted_Monthly_Sales_Volume <- round(predicted_Monthly_Sales_Volume)
    
    output$Monthly_Sales_Volume <- renderPlot({
      plot(data$x1, data$y, col = "#3498DB", xlab = "number of website visitors per month", ylab = "Sales", main = "Scatterplot of Predicted Monthly Sales Volume")
      points(new_data$x1, predicted_Monthly_Sales_Volume, col = "red", pch = 16)
      legend("topright", legend = c("Actual Data", "Predicted Data"), col = c("#3498DB", "red"), pch = c(1, 16))
    })
    
    output$prediction_output <- renderPrint({
      cat("Prediction of Monthly Sales Volume: ", predicted_Monthly_Sales_Volume,"\n")
    })
  })
}
# Run the application
shinyApp(serverUI, server)

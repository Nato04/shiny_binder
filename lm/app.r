#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a scatterplot/linear model of data
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling - Shiny"),

    # Sidebar with CSV definitions/options and an action button for the linear model. 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
        
            # Horizontal line ----
            tags$hr(),
            
            #Action Button - Linear Modeling
            actionButton("lmRun","Linear Model Calculation")  
        
        ),

        # Show a plot of the scatterplot data, linear model, and datainput table
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        
        )   
    )
)

# Define server logic for Linear Regression Modeling - Scatterplot, Linear Model graph
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)        
    })
    
    
    #Scatter plot with ggplot
    
    output$distPlot <- renderPlot({
         ggplot()+
                geom_point(aes(x = dataInput()$x, y = dataInput()$y), 
                       colour = 'red')+
                ggtitle('Y versus X')+
                xlab("X") +
                ylab("Y")
        
                     
    })
    
    #Linear Model Plot, (unsure why the coefficient data is overlapping in the graph at all).
    
    output$lmPlot <- renderPlot({
        coefs <- coef(model(), 2)
        intercept <- round(coefs[1], 2)
        slope <- round(coefs[2], 2)
        r2 <- round (summary(model())$r.squared, 2)

            ggplot()+
                geom_point(aes(x = dataInput()$x, y = dataInput()$y), 
                       colour = 'red') +
                geom_line(aes(x = dataInput()$x, y = predict(model(), newdata = dataInput())), 
                      colour = 'blue') +
                ggtitle('Y Versus X') +
                xlab("X") +
                ylab("Y") +
                geom_text(aes(x=10, y=11, label = paste("Intercept: ", intercept))) +
                geom_text(aes(x=10, y=12, label = paste("Slope: ", slope))) +
                geom_text(aes(x=10, y=13, label = paste("Coefficient: ", coefs)))+
                geom_text(aes(x=10, y=14, label = paste("R Squared: ", r2)))      
    })                                    
    
    #Linear Model calculation with reactive element
    
    model <- eventReactive(input$lmRun, {
        lm(formula = y ~ x, 
         data = dataInput())
        
    })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
           
     })      
        
}

# Run the application 
shinyApp(ui = ui, server = server)

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling - Shiny"),

    # Sidebar with a slider input for number of bins 
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
            actionButton("lmRun","Linear Model Calculation"),

            # Horizontal line ----
            tags$hr(),
            
            #Action Button - Show LM
            actionButton("lmShow","View Linear Model")
            
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
            tableOutput("contents"),
            textOutput("lmMod")
        
        )   
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    
        lmTrig <- eventReactive(input$lmRun, {
            model = lm(formula = dataInput()$x ~ dataInput()$y, data = dataset)  
    })        
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    output$lmMod <- renderPrint({
        summary(model)
    })    
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
        observeEvent(input$lmShow, {
            ggplot()+
            geom_line(aes(x = dataInput()$x, y = predict(model, newdata = dataset)), colour = 'blue')
    })                                    
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
    
    output$lmMod <- renderPrint({
        if(is.null(model)) {
            return(print("Run linear model calculation to see data"))
        }
        else{
            return(summary(model))
        }
           
     })      
        
}

# Run the application 
shinyApp(ui = ui, server = server)

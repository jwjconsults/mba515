library(shiny)
library(shinythemes)
library(xlsx)
library(dplyr)
library(ggplot2)
library(scales)

# ui <- fluidPage(
#   h1("Bar Plot using GGPlot"),
#   plotOutput("plot")
# )
# 
# server <- function(input, output, session) {
#   
#   output$plot <- renderPlot({
#     df1 <- read.xlsx("Offenses_Completed_and_Attempted.xlsx", sheetIndex = 1)
#     df = df1[3:28 ,]
#     #Plot
#     ggplot(df) + geom_col(aes(x = Total.Offenses, y = Offense.Category), stat = "identity")
#   })
# }

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("MBA 515",
                           
                           tabPanel("Offenses Attempted and Completed",
                                    
                                    sidebarPanel(
                                      selectInput(inputId = "sel_Offenses",
                                                  label = "Choose Attempted or Completed",
                                                  "Names")
                                    ), #Sidebar Panel
                                    
                                    mainPanel(
                                      plotOutput("plot")
                                    ) #Main End
                           ), #  Tab 1 End
                           tabPanel("Navbar 2", "This panel is intentionally left blank"),
                           tabPanel("Navbar 3", "This panel is intentionally left blank")
                           
                ) #NavBar Page End
) #UI End

server <- function(input, output, session) {
  
  
  df1 <- read.xlsx("Offenses_Completed_and_Attempted.xlsx", sheetIndex = 1)
  # Summarize Data and Plot Chart - 
  #wrap in reactive to allow selection of data
  dataSelected <- reactive({
    req(input$sel_Offenses)
    
    
  })
  
  # Update SelectInput Dynamically
  observe({
    updateSelectInput(session, "sel_Offenses", choices = colnames(df1[2:4]))
  })
  #Plot ggplot2
  # Wrap in render cmd to make interactive
  
  output$plot <- renderPlot({
    df = df1[3:28 ,]
    ggplot(df)+ geom_col(aes_string(x = input$sel_Offenses, y = "Offense.Category"))+ scale_x_continuous(labels = comma)
    
  })
  
  
}

shinyApp(ui = ui, server = server)


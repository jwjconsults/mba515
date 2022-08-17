library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(scales)
library(broom)
#library(ggpubr)
library(tidyverse)

ui <- fluidPage(
  theme = shinytheme(theme = "superhero"),
  navbarPage("MBA 515",
             
             tabPanel("Offenses Attempted and Completed",
                      
                      sidebarPanel(
                        selectInput(inputId = "sel_Offenses",
                                    label = "Choose Attempted or Completed",
                                    list("Total.Offenses", "Number.of.Offenses.Completed", "Number.of.Offenses.Attempted"),
                                    selected = "Total.Offenses")
                        
                      ), #Sidebar Panel
                      
                      mainPanel(
                        plotOutput("plot1"),
                        tableOutput("crimeData")
                      ) #Main End
             ), # End Tab 1
             tabPanel("Agencies", 
                      mainPanel(
                        plotOutput("plot2")
                      )
             ), # End Tab 2
             tabPanel("Times",
                      mainPanel(
                        plotOutput("plot3")
                      )
             ), #End Tab 3
             tabPanel("Crimes in Texas",
                      mainPanel(
                        plotOutput("plot4")
                      )   
             ), # End Tab 4
             tabPanel("Crimes in Texas: Categories",
                      mainPanel(
                        plotOutput("plot5")
                      )   
             ), # End Tab 5
             tabPanel("Population Covered",
                      sidebarPanel(
                        radioButtons("Button2", 
                                     label = "Choose:",
                                     c("Persons" = "persons2",
                                       "Property" = "property2",
                                       "Society" = "society2"),
                                     selected = "persons2"        
                        )
                        
                      ), #Sidebar Panel
                      mainPanel(
                        plotOutput("plot6")
                      )   
             ), # End Tab 6
             tabPanel("Crimes by Target",
                      sidebarPanel(
                        
                        radioButtons("Button", 
                                     label = "Choose:",
                                     c("Persons", "Property", "Society"),
                                     selected = "Persons"        
                        )
                        
                      ), #Sidebar Panel
                      mainPanel(
                        plotOutput("plot7")
                      )   
             ), # End Tab 7
             
  ) #NavBar Page End
) #UI End

#-----------------------------------------------------------------------------

server <- function(input, output, session) {
  # Tab 1 data
  
  df1 <- read.csv("Offenses_Completed_and_Attempted.csv", header = T, sep = ",")
  #wrap in reactive to allow selection of data
  dataSelected <- reactive({
    req(input$sel_Offenses)
  })
  
  # Update SelectInput Dynamically
  updateSelectInput(session, "sel_Offenses", choices = colnames(df1[2:4]))
  
  output$plot1 <- renderPlot({
    df = df1[3:28 ,]
    ggplot(df)+ 
      geom_col(aes_string(x = input$sel_Offenses, y = "Offense.Category"))+ 
      scale_x_continuous(labels = comma)
    
  })
  output$crimeData <- renderTable({
    df1 %>% select("Offense.Category",input$sel_Offenses)
  })
  
  # Tab 2 data
  df2 <- read.csv("Crimes_Against_Persons_State.csv", header = T, sep = ",")
  
  output$plot2 <- renderPlot({
    ggplot(filter(df2, df2$State != "Total"), 
           aes(x=Population.Covered, y=Number.of.Participating.Agencies))+
      geom_point()+ 
      geom_smooth(method="lm", col="black")+
      #stat_regline_equation()+
      theme(axis.text.x = element_text(angle = 90))+
      labs(x = "Population Covered", y = "# of Participating Agencies", 
           title = "Regression Model Population Covered vs Participating Agencies in 2020")
  })
  
  # Tab 3 data
  df3 <- read.csv("Crimes_Incidents_Time_of_Day.csv", header = T, sep = ",")
  
  output$plot3 <- renderPlot({
    TOD <- df3 %>% 
      select("Time.of.Day","Assault.Offenses", "Larceny.Theft.Offenses", "Destruction.Damage.Vandalism", "Drug.Narcotic.Offenses", "Burglary.Breaking.and.Entering") %>% 
      pivot_longer(-"Time.of.Day", names_to = "variable", values_to = "value")
    
    
    
    ggplot(TOD, aes(Time.of.Day, value, colour = variable)) + geom_line() +
      labs(x = "Time of Day", y = "Total Offenses", 
           title = "Top 5 Crimes - Texas - Day Timeframe", colour ="Crimes Commited")
  })
  
  # Tab 4 data
  df4 <- read.csv("CLEAN_Crimes_Texas.csv")
  
  output$plot4 <- renderPlot({
    ggplot(df4, aes(x="", y=Total.Offenses, fill=Texas)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      geom_text(aes(label = paste0(Total.Offenses, "%")), position =  position_stack(vjust=0.5)) +
      labs(x = NULL, y = NULL, fill = NULL, title = "Crimes Offenses In Texas 2020") +
      theme_classic() +
      theme() +
      scale_fill_brewer(palette="PuBu")
  })
  
  # Tab 5 data
  df5 <- read.csv("CLEAN_Crimes_Texas_Categories.csv")
  
  output$plot5 <- renderPlot({
    ggplot(df5, aes(Categories, Percentage)) + 
      geom_bar(stat="identity", width = 0.8, fill="tomato2") + 
      theme(axis.text.x = element_text(angle=90)) +
      labs(x = "Types of Offenses", y = "Percentage", 
           title = "Types of Texas Crimes by 2020")
  })
  
  # Tab 6 Data
  df6a <- read.csv("Crimes_Against_Society_State.csv")
  df6b <- read.csv("Crimes_Against_Property_State.csv")
  df6c <- read.csv("Crimes_Against_Society_State.csv")
  
  output$plot6 <- renderPlot({
    
    button2 <- switch(input$Button2,
                      persons2 =  ggplot(filter(df6a, df6a$State != "Total"), aes(x=Population.Covered, y=Total.Offenses))+
                        geom_point()+
                        geom_smooth(method="lm", col="black")+
                        #stat_regline_equation()+
                        theme(axis.text.x = element_text(angle = 90))+
                        labs(x = "Population Covered", y = "Total Offenses",
                             title = "Regression Model of Population Covered vs Total for Crimes Against Persons 2020"),
                      property2 = ggplot(filter(df6b, df6b$State != "Total"), aes(x=Population.Covered, y=Total.Offenses))+
                        geom_point()+
                        geom_smooth(method="lm", col="black")+
                        #stat_regline_equation()+
                        theme(axis.text.x = element_text(angle = 90))+
                        labs(x = "Population Covered", y = "Total Offenses",
                             title = "Regression Model of Population Covered vs Total for Crimes Against Property 2020"),
                      society2 = ggplot(filter(df6c, df6c$State != "Total"), aes(x=Population.Covered, y=Total.Offenses))+
                        geom_point()+
                        geom_smooth(method="lm", col="black")+
                        #stat_regline_equation()+
                        theme(axis.text.x = element_text(angle = 90))+
                        labs(x = "Population Covered", y = "Total Offenses",
                             title = "Regression Model of Population Covered vs Total for Crimes Against Society 2020")
                      
    )
    button2 
  })
  
  #Tab 7 Data
  df7a <- read.csv("Crimes_Against_Persons_State.csv")
  df7b <- read.csv("Crimes_Against_Property_State.csv")
  df7c <- read.csv("Crimes_Against_Society_State.csv")
  
  output$plot7 <- renderPlot({
    
    button <- switch(input$Button,
                     Persons =  ggplot(df7a)+
                       geom_bar(aes(State,Total.Offenses),stat="summary",fun="mean")+
                       scale_y_continuous(labels = comma)+
                       theme(axis.text.x = element_text(angle = 90))+
                       labs(x = "State", y = "Total Offenses", 
                            title = "Crimes Against Persons Offenses by State 2020"),
                     Property = ggplot(df7b)+
                       geom_bar(aes(State,Total.Offenses),stat="summary",fun="mean",)+
                       scale_y_continuous(labels = comma)+
                       theme(axis.text.x = element_text(angle = 90))+
                       labs(x = "State", y = "Total Offenses", 
                            title = "Crimes Against Property Offenses by State 2020"),
                     Society = ggplot(df7c)+
                       geom_bar(aes(State,Total.Offenses),stat="summary",fun="mean")+
                       scale_y_continuous(labels = comma)+
                       theme(axis.text.x = element_text(angle = 90))+
                       labs(x = "State", y = "Total Offenses", 
                            title = "Crimes Against Society Offenses by State 2020")
                     
    )
    button  
  })
  
  
} #Server End

shinyApp(ui = ui, server = server)

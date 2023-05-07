#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vembedr)
library(readxl)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(disable=T),
  dashboardSidebar(disable=T),
  
  dashboardBody(
    # set background to white
    setBackgroundColor(color = "#FFFFFF", shinydashboard = FALSE),
    setBackgroundColor(color = "#FFFFFF", shinydashboard = TRUE),
    tags$head(
      tags$link(rel = "shortcut icon", href = "Good-logo.ico")
    ),
    navbarPage(title=img(id="logo", src="Good logo.png", style="width:100%;height:150%"),
               theme = shinytheme("lumen"),
               windowTitle = "Employee report",
               tabPanel("Service description",
                        column(width = 12,align = "center",
                               h3("Employee report"),
                               p("This service is a gift from me for employees to help you track your progress in transactions processing in a simple and easy way using 7 indicators. To learn more about this tool, you can check the video below and download the sample data excel file to start using it yourself."),
                               br(),
                               fluidRow(downloadButton("id1","Download Excel file")
                                        ),
                               br(),
                               fluidRow(
                                 column(width = 12,
                                        embed_url("https://www.youtube.com/watch?v=-IBxIXBaei8") %>%
                                          use_bs_responsive()
                                        )
                                 )
                               )
                        ),
               tabPanel("Dashboard",
                        column(width = 12,align = "center",
                               h3("Upload File"),
                               column(width = 12,align = "left",
                                      p("The objective of this application is to generate 7 indicators using the employee's transactions history database in the form of a dashboard. The indicators included in the dashboard are:"),
                                      tags$ol(
                                        tags$li("Total number of transactions"),
                                        tags$li("Average time period to complete transactions"),
                                        tags$li("Most finished transaction type"),
                                        tags$li("Total count of transactions by status"),
                                        tags$li("Total count of transaction type by status"),
                                        tags$li("Average process time period by transaction"),
                                        tags$li("Count of finished transactions by date")
                                        
                                      )
                                      ),
                               br(),
                               fluidRow(fileInput("id2", "",accept = ".xlsm")
                                        ),
                               br(),
                               fluidRow(
                                 column(width = 4,valueBoxOutput("box1", 12),
                                        valueBoxOutput("box2", 12),
                                        valueBoxOutput("box3", 12),
                                        
                                        ),
                                 column(width = 4, plotlyOutput("plot1")
                                        ),
                                 column(width = 4, plotlyOutput("plot2")
                                        )
                                 ),
                               fluidRow(
                                 column(width = 6,plotlyOutput("plot3")
                                        ),
                                 column(width = 6,plotlyOutput("plot4")
                                        )
                                 )
                               )
                        )
               )
    )
  )
# Define server logic required to draw a histogram
server <- function(input, output) {
  df <- reactiveVal("AN")
  output$id1 <- downloadHandler(
    filename = function() {
      paste0("My data ", Sys.Date(), ".xlsm")},
    content = function(My_data){
      file.copy("database.xlsm",My_data)}
    )
  observeEvent(input$id2,{
    print(input$id2)
    datae <- read_excel(input$id2$datapath,range = "B12:C212")
    data2 <- read_excel(input$id2$datapath,range = "XFA12:XFB212")
    print(datae)
    print(data2)
    print(cbind(datae,data2))
    df(cbind(datae,data2))
    print(names(df()))
    output$plot1 <- renderPlotly({
      plot1 <- df() %>%
        filter(!is.na(situation)) %>%
        group_by(situation) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = situation, y = count, fill = situation)) +
        geom_bar(stat = "identity") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Total count of transactions by status") +
        xlab("") +
        ylab("") +
        scale_fill_manual(values = c("#BFBFBF", "#417FA6"))
        # scale_fill_brewer(palette="Dark2")
      ggplotly(plot1,tooltip = c("y")
               )
                })
    output$plot2 <- renderPlotly({
      plot2 <- df() %>%
        filter(!is.na(situation)) %>%
        group_by(`type of process`,situation) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = `type of process`, y = count, fill = situation)) +
        geom_bar(stat = "identity") +
        ggtitle("Count of each transaction type by status") +
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#BFBFBF", "#417FA6"))
        # scale_fill_brewer(palette="Dark2")
      ggplotly(plot2,tooltip = c("y","fill")
               )
      })
    output$plot3 <- renderPlotly({
      plot3 <- df() %>%
        filter(situation == "finished") %>%
        mutate(`last process` = as.Date(`last process`,format = "%y-%m-%d"),
               `first process` = as.Date(`first process`,format = "%y-%m-%d"),
               Average = `last process` - `first process`) %>%
        group_by(`type of process`) %>%
        summarise(Average = round(mean(Average))) %>%
        ggplot(aes(x = `type of process`, y = Average, fill = `type of process`)) +
        geom_bar(stat = "identity") +
        ggtitle("Average process time by transaction in days") +
        xlab("") +
        ylab("Days") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = c("#A2A6F2","#BFBFBF", "#417FA6","#3F89A6","#0D0D0D"))
      ggplotly(plot3,tooltip = c("y")
               )
      })
    output$plot4 <- renderPlotly({
      plot4 <- df() %>%
        filter(situation == "finished") %>%
        mutate(`last process` = as.Date(`last process`,format = "%y-%m-%d")) %>%
        group_by(`last process`) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = `last process`, y = count)) +
        geom_line(color = "#3F89A6") +
        geom_point() +
        ggtitle("Count of finished transactions by date") +
        xlab("End of transaction") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))
      ggplotly(plot4,tooltip = c("y")
               )
      })
    output$box1 <- renderValueBox({
      boxA = df() %>%
        filter(!is.na(`type of process`)) %>%
        summarise(count = n()
                  )
      valueBox(boxA$count,"Number of transactions")
      })
    output$box2 <- renderValueBox({
      boxB = df() %>%
        filter(situation == "finished") %>%
        mutate(`last process` = as.Date(`last process`,format = "%y-%m-%d"),
               `first process` = as.Date(`first process`,format = "%y-%m-%d"),
               Average = `last process` - `first process`) %>%
        summarise(Average = round(mean(Average)))
      valueBox(boxB$Average,"Average process time in days")
    })
    output$box3 <- renderValueBox({
      boxC = df() %>%
        filter(situation == "finished") %>%
        group_by(`type of process`) %>%
        summarise(count = n())
      
      boxC <- boxC[boxC$count == max(boxC$count),"type of process"]
      
      valueBox(boxC$`type of process`,"Most finished transaction type")
    })
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

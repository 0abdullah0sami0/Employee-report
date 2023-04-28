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
                               p("Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
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
                               p("Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
                               br(),
                               fluidRow(fileInput("id2", "",accept = ".xlsm")
                                        ),
                               br(),
                               fluidRow(
                                 column(width = 2, valueBoxOutput("box1", 12)
                                        ),
                                 column(width = 2, valueBoxOutput("box2", 12)
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
      file.copy("My data.xlsm",My_data)}
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
        filter(!is.na(`situation‏`)) %>%
        group_by(`situation‏`) %>%
        summarise(count = n()) %>%
        ggplot(aes(x = `situation‏`, y = count, fill = `situation‏`)) +
                    geom_bar(stat = "identity") +
                    theme_classic() +
                    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
                    ggtitle("Total count by situation") +
                    xlab("") +
                    ylab("") +
                    scale_fill_brewer(palette="Dark2")
                  
                   ggplotly(plot1,tooltip = c("y"))
                })
                
                output$plot2 <- renderPlotly({
                plot2 <- df() %>%
                    filter(!is.na(`situation‏`)) %>%
                    group_by(`type of process`,`situation‏`) %>%
                    summarise(count = n()) %>%
                    
                    ggplot(aes(x = `type of process`, y = count, fill = `situation‏`)) +
                    geom_bar(stat = "identity") +
                    ggtitle("Count of transactions by situation") +
                    xlab("") +
                    ylab("") +
                    theme_classic() +
                    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
                    scale_fill_brewer(palette="Dark2")
                
                    ggplotly(plot2,tooltip = c("y","fill"))
                  
                })
                
                output$plot3 <- renderPlotly({
                  plot3 <- df() %>%
                           filter(`situation‏` == "finished") %>%
                           mutate(Avrage = `last process` - `first process`) %>%
                           group_by(`type of process`) %>%
                           summarise(Avrage = mean(Avrage)) %>%
                           
                           ggplot(aes(x = `type of process`, y = Avrage, fill = `type of process`)) +
                           geom_bar(stat = "identity") +
                           ggtitle("Average process time by transaction") +
                           xlab("") +
                           ylab("") +
                           theme_classic() +
                           theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5)) +
                           scale_fill_brewer(palette="Dark2")
                          
                  ggplotly(plot3,tooltip = c("y"))
                })
                
                output$plot4 <- renderPlotly({
                plot4 <- df() %>%
                         filter(`situation‏` == "finished") %>%
                         group_by(`last process`) %>%
                         summarise(count = n()) %>%
                  
                  ggplot(aes(x = `last process`, y = count)) +
                  geom_line(color = "#1C9E77") +
                  ggtitle("Number of finished transactions by date") +
                  xlab("End of transaction") +
                  ylab("") +
                  theme_classic() +
                  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))
                  
                
                ggplotly(plot4,tooltip = c("y"))
                  
                })
                
                output$box1 <- renderValueBox({
                  boxA = df() %>%
                    filter(!is.na(`type of process`)) %>%
                    summarise(count = n())
                  
                  valueBox(boxA$count,"Number of transactions")
                  
                })
})}

# Run the application 
shinyApp(ui = ui, server = server)

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

# Define UI for application that draws a histogram
ui <- navbarPage("Employee report",
                 tabPanel("Service description",
                          
                 column(width = 12,align = "center",
                        
                 fluidRow(downloadButton("id1","Download Excel file")),
                 br(),
                 fluidRow(embed_url("https://www.youtube.com/watch?v=-IBxIXBaei8")))),
                 
                 tabPanel("Dashboard",
                 
                 column(width = 12,align = "center",
                 
                 fluidRow(fileInput("id2", "Upload",accept = ".xlsm")),
                 br(),
                 fluidRow(
                 column(width = 4,plotOutput("plot1")),
                 column(width = 4,plotOutput("plot2")),
                 column(width = 4,plotOutput("plot3"))),
                 
                 fluidRow(
                 column(width = 6,plotOutput("plot4")),
                 column(width = 6,plotOutput("plot5")))
                 
                 )))
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
                
                output$plot1 <- renderPlot({
                    df() %>%
                    filter(!is.na(`situation‏`)) %>%
                    group_by(`situation‏`) %>%
                    summarise(countAF = n()) %>%
                    
                    ggplot(aes(x = `situation‏`, y = countAF, fill = `situation‏`)) +
                    geom_bar(stat = "identity")
                })
                
                output$plot2 <- renderPlot({
                    df() %>%
                    filter(!is.na(`situation‏`)) %>%
                    group_by(`type of process`,`situation‏`) %>%
                    summarise(countALL = n()) %>%
                    
                    ggplot(aes(x = `type of process`, y = countALL, fill = `situation‏`)) +
                    geom_bar(stat = "identity")
                })
                
                output$plot3 <- renderPlot({
                  df() %>%
                    filter(!is.na(`situation‏`)) %>%
                    group_by(`type of process`,`situation‏`) %>%
                    summarise(countALL = n()) %>%
                    
                    ggplot(aes(x = `type of process`, y = countALL, fill = `situation‏`)) +
                    geom_bar(stat = "identity")
                })
                
                output$plot4 <- renderPlot({
                  df() %>%
                    filter(!is.na(`situation‏`)) %>%
                    group_by(`type of process`,`situation‏`) %>%
                    summarise(countALL = n()) %>%
                    
                    ggplot(aes(x = `type of process`, y = countALL, fill = `situation‏`)) +
                    geom_bar(stat = "identity")
                })
                
                output$plot5 <- renderPlot({
                  df() %>%
                    filter(!is.na(`situation‏`)) %>%
                    group_by(`type of process`,`situation‏`) %>%
                    summarise(countALL = n()) %>%
                    
                    ggplot(aes(x = `type of process`, y = countALL, fill = `situation‏`)) +
                    geom_bar(stat = "identity")
                })
                  
                })
}

# Run the application 
shinyApp(ui = ui, server = server)

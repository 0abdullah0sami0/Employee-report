dashboardPage(
  dashboardHeader(disable=T),
  dashboardSidebar(disable=T),
  
  dashboardBody(
    # set background to white
    setBackgroundColor(color = "#FFFFFF", shinydashboard = FALSE),
    setBackgroundColor(color = "#FFFFFF", shinydashboard = TRUE),
    tags$head(
      tags$link(rel = "shortcut icon", href = "Good-logo.ico")
    ),
    navbarPage(title=div(img(src="Good logo.png", style = "width:8%;height:10%; position:relative;display:right-align;margin-top:-10px;")),
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
                               hr(),
                               fluidRow(
                                 column(width = 12,
                                        img(src = "application.png", style = "width:70%;height:70%;")
                                        # embed_url("https://www.youtube.com/watch?v=-IBxIXBaei8") %>%
                                        #   use_bs_responsive()
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
                                 column(width = 4,withSpinner(valueBoxOutput("box1", 12)),
                                        valueBoxOutput("box2", 12),
                                        valueBoxOutput("box3", 12)
                                        
                                 ),
                                 column(width = 4, withSpinner(plotlyOutput("plot1"))
                                 ),
                                 column(width = 4, withSpinner(plotlyOutput("plot2"))
                                 )
                               ),
                               fluidRow(
                                 column(width = 6, withSpinner(plotlyOutput("plot3"))
                                 ),
                                 column(width = 6, withSpinner(plotlyOutput("plot4"))
                                 )
                               )
                        )
               )
    ),
    hr(),
    fluidRow(
      column(width = 1,align = "right", offset = 4,
             tags$a(img(src = "GitHub.png",style = "width:30%;height:30%;"),href = "https://github.com/0abdullah0sami0", target="_blank")),
      column(width = 1,align = "center", offset = 0,
             tags$a(img(src = "LinkedIn.png",style = "width:30%;height:30%;"),href = "https://www.linkedin.com/in/abdullahalshalaan/", target="_blank")),
      column(width = 1,align = "left", offset = 0,
             tags$a(img(src = "twittter.jpeg",style = "width:30%;height:30%;"),href = "https://twitter.com/HR02030", target="_blank"))
    )
  )
)
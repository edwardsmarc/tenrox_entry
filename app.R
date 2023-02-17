source("global.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Tenrox entry app"),
  dashboardSidebar(
    fileInput(inputId = "csv_path", label = "Upload calendar.csv", accept = ".csv"),
    tags$br(),
    tags$br(),
    tags$br(),
    div(style="text-align:center",
        tags$h4(" Hours worked this week: ", ),
        tags$h4(textOutput("hours_worked_this_week"))
        )
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        id = "tabs", width = 12,
        tabPanel("Raw", excelOutput(outputId = "full_table", height = "80vh")),
        tabPanel("Project sums", excelOutput(outputId = "weekly_table", height = "80vh"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # generate reactiveValue to hold data
  data <- reactiveValues()
  
  # load timesheet
  observe({
    req(input$csv_path)
    data$timesheet <- timesheet_prep(read_csv(input$csv_path$datapath))
    
    data$full <- full_table(data$timesheet)
    data$weekly <- weekly_sums(data$timesheet)
    
  })
    
  # Render tables
  output$full_table <- renderExcel({
    req(input$csv_path)
    excelTable(data$full, columns = full_columns, rowResize = TRUE, columnResize = TRUE, wordWrap = TRUE, tableHeight="100%", tableWidth = "100%")
  })
  
  output$weekly_table <- renderExcel({
    req(input$csv_path)
    excelTable(data$weekly, columns = weekly_columns, rowResize = TRUE, columnResize = TRUE, wordWrap = TRUE, tableHeight="100%", tableWidth = "100%")
  })
  
  # Calc hours worked this week
  output$hours_worked_this_week <- renderText({
    req(input$csv_path)
    hours_this_week(data$timesheet)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

source("global.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Tenrox entry app"),
  dashboardSidebar(
    fileInput(inputId = "csv_path", label = "Upload calendar.csv", accept = ".csv")
  ),
  dashboardBody(
    tags$style(HTML(
      ".reactiveExcelR {
                    height: calc(100vh - 200px);
                    }
                    "
    )),
    fluidRow(
      tabBox(
        id = "tabs", width = 12,
        tabPanel("Raw", excelOutput(outputId = "full_table", height = "100%", width = "100%")),
        tabPanel("Project sums", excelOutput(outputId = "weekly_table", height = "100%", width = "100%"))
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
    data$weekly <- weekly_sums(data$full)
    
  })
    
  # Render tables
  output$full_table <- renderExcel({
    req(input$csv_path)
    excelTable(data$full, autoWidth=TRUE, tableHeight="100%")
  })
  
  output$weekly_table <- renderExcel({
    req(input$csv_path)
    excelTable(data$weekly, autoWidth=TRUE, tableHeight="100%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

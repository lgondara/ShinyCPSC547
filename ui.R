

dashboardPage(
  dashboardHeader(title = "Exploratory data analysis"),
  dashboardSidebar(
    
    fileInput('file1', 'Choose file to upload',
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )
    ),

    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Two dimensional",
                  plotlyOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Three dimensional",
                  plotlyOutput("packagePlot2", width = "100%", height = 600)
                )
              )
      ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)


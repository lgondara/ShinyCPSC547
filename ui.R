

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
    
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    uiOutput("vars"),
    uiOutput("time"),
    uiOutput("censor"),
    uiOutput("numclust"),
    tags$hr(),
    actionButton("action", "Run"),
  

    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Explore", tabName = "Explore"),
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
                  plotlyOutput("packagePlot", width = "100%", height = 400)
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Three dimensional",
                  plotlyOutput("packagePlot2", width = "100%", height = 400)
                )
              ),
              
              
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "KNN results",
                  plotlyOutput("packagePlot3", width = "100%", height = 400)
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "KM plot",
                  plotOutput("packagePlot4", width = "100%", height = 400)
                )
              )
              
              
      ),
      tabItem("Explore",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      ),
      
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)


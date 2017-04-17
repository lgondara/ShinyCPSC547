

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
      menuItem("Explore", tabName = "explore")
    )
  ),
    
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "tSNE and cluster results",
                  plotlyOutput("packagePlot3", width = "100%", height = 400)
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Number of clusters by MSE",
                  plotlyOutput("packagePlot2", width = "100%", height = 400)
                )
              ),
              
              
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "KM plot-optimal clusters",
                  plotOutput("packagePlot5", width = "100%", height = 400)
                )
                ,
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "KM plot-user selected",
                  plotOutput("packagePlot4", width = "100%", height = 400)
                )
              )
              
              
      ),
      tabItem("explore",
              
              fluidRow(
                box(
                  width = "100%", status = "info", solidHeader = TRUE,
                  title = "Cluster outcome for all variables",
                  parcoordsOutput("parcoords", width = "100%", height = 400)
                ),
                
                uiOutput("exploreclus"),
                actionButton("action2", "Run"),
                box(
                  width = "100%", status = "info", solidHeader = TRUE,
                  title = "Variable by cluster",
                  plotOutput("scatterplot", width = "100%", height = 400)
                )
                
              )
      )
    )
  )
)


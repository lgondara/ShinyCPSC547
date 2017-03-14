dashboardPage(
  dashboardHeader(title = "Exploratory data analysis"),
  dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
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
                  plotOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Three dimensional",
                  plotOutput("packagePlot2", width = "100%", height = 600)
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


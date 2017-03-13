function(input, output) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  
  
  output$packagePlot <- renderPlot({
    rdat=read.table("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/abm.dta",sep="")
    x=rnorm(1000)
    y=rnorm(1000)
    plot(x,y)
  })
  
  output$packageTable <- renderTable({head(attitude,15)})
  
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(attitude, 15))
    options(orig)
  })
}

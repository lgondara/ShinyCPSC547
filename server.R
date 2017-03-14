function(input, output) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  
  
  output$packagePlot <- renderPlotly({
    adata=read.table("https://www.umass.edu/statdata/statdata/data/actg320.dat")
    adata=adata[,-1]
    coll=Rtsne(adata)
    tsned2=as.data.frame(coll$Y)
    plot_ly(data = tsned2, x = ~V1, y = ~V2)
  })
  
  output$packagePlot2 <- renderPlotly({
    coll=Rtsne(adata, dims=3)
    tsned3=as.data.frame(coll$Y)
    plot_ly(data = tsned3, x = ~V1, y = ~V2)
  })
  
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(attitude, 15))
    options(orig)
  })
}



options(shiny.maxRequestSize = 9*1024^2)
function(input, output) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 9MB.
  

  output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
  })

  
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
    plot_ly(data = tsned3, x = ~V1, y = ~V2, z = ~V3)
  })
  
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(attitude, 15))
    options(orig)
  })
}



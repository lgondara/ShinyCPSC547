options(shiny.maxRequestSize = 9*1024^2)
function(input, output) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 9MB.
  
  usevalues=reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    adata=read.csv(inFile$datapath)
    
    adata=adata[,-1]
    coll=Rtsne(adata)
    tsned2=as.data.frame(coll$Y)
    
    coll2=Rtsne(adata, dims=3)
    tsned3=as.data.frame(coll2$Y)
    
    allval=list(a=tsned2,b=tsned3)
    allval
  })
  
  output$packagePlot <- renderPlotly({
    allval=usevalues()
    plot_ly(data =allval$a, x = ~V1, y = ~V2)
  })
  
  output$packagePlot2 <- renderPlotly({
    allval=usevalues()
    plot_ly(data = allval$b, x = ~V1, y = ~V2, z = ~V3)
  })
  
  output$packagePlot3 <- renderPlotly({
    allval=usevalues()
    rtkm=kmeans(allval$a,2)
    newdat=data.frame(allval$a, rtkm$cluster)
    plot_ly(data = newdat, x = ~newdat[,1], y = ~newdat[,2],color=as.factor(newdat[,3]))
  })
  
  
  output$rawtable <- renderPrint({
    allval=usevalues()
    orig <- options(width = 1000)
    print(tail(allval$a, 15))
    options(orig)
  })
}



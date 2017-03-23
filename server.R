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
    
    rtkm=kmeans(tsned2,2)
    newdat=data.frame(tsned2, rtkm$cluster)
    
    kmdata=data.frame(adata, rtkm$cluster)
    fit <- survfit(Surv(T3, D3) ~ rtkm.cluster, data = kmdata)
    
    
    allval=list(a=tsned2,b=tsned3, c=newdat, d=fit)
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
    plot_ly(data = allval$c, x = ~allval$c[,1], y = ~allval$c[,2],color=as.factor(allval$c[,3]))
  })
  
  output$packagePlot4 <- renderPlot({
    allval=usevalues()
    ggsurvplot(allval$d, risk.table = TRUE,pval = TRUE)
  })
  
  
  output$rawtable <- renderPrint({
    allval=usevalues()
    orig <- options(width = 1000)
    print(tail(allval$a, 15))
    options(orig)
  })
}



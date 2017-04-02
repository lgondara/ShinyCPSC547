function(input, output) {
  
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  # By default, the file size limit is 5MB. It can be changed by
  # setting this option. Here we'll raise limit to 9MB.
  
  info <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    read.table(infile$datapath, header = input$header, sep = input$sep)
  })
  
  
  output$vars<- renderUI({
    df <- info()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("vars","Select variables",items, multiple = T)
  })
  
  
  output$time<- renderUI({
    df <- info()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("time","Select time",items, multiple = F)
  })
  
  
  output$censor<- renderUI({
    df <- info()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("censor","Select censor",items, multiple = F)
  })
  
  models=eventReactive(input$action,{
    adata=info()
    adatause=adata[input$vars]
    timeout=adata[input$time]
    timeout=as.numeric(unlist(timeout))
    censor=adata[input$censor]
    censor=unlist(censor)
    coll=Rtsne(adatause, check_duplicates = FALSE)
    tsned2=as.data.frame(coll$Y)
    
    rtkm=kmeans(tsned2,2)
    newdat=data.frame(tsned2, rtkm$cluster)
    
    kmdata=data.frame(timeout,censor,rtkm$cluster)
    fit <- survfit(Surv(timeout,censor) ~ rtkm.cluster, data=kmdata)
    
    
    allval=list(a=tsned2, b=kmdata,c=newdat, d=fit)
    allval
  })
  
  output$packagePlot <- renderPlotly({
    allval=models()
    plot_ly(data =allval$a, x = ~V1, y = ~V2)
  })
  
  
  output$packagePlot3 <- renderPlotly({
    allval=models()
    plot_ly(data = allval$c, x = ~allval$c[,1], y = ~allval$c[,2],color=as.factor(allval$c[,3]))
  })
  
  output$packagePlot4 <- renderPlot({
    allval=models()
    ggsurvplot(fit=allval$d, risk.table = TRUE,pval = TRUE,data=allval$b)
  })
  
  
  output$rawtable <- renderPrint({
    adata=models()
    orig <- options(width = 1000)
    print(tail(adata$b, 15))
    options(orig)
  })
}
 





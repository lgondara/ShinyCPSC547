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
  
  output$numclust<- renderUI({
    items=c(1:5)
    names(items)=items
    selectInput("numclust","Select cluster",items, multiple = F)
  })
  

  models=eventReactive(input$action,{
    adata=info()
    adatause=adata[input$vars]
    timeout=adata[input$time]
    timeout=as.numeric(unlist(timeout))
    censor=adata[input$censor]
    censor=unlist(censor)
    cnum=input$numclust
    cnum=as.numeric(unlist(cnum))
    
    coll=Rtsne(adatause, check_duplicates = FALSE)
    tsned2=as.data.frame(coll$Y)
    
    rtkm=kmeans(tsned2,cnum)
    newdat=data.frame(tsned2, rtkm$cluster)
    
    kmdata=data.frame(timeout,censor,rtkm$cluster)
    fit <- survfit(Surv(timeout,censor) ~ rtkm.cluster, data=kmdata)
    
    k.max=10
    ss <- sapply(1:k.max, 
                 function(k){kmeans(tsned2, k, nstart=50,iter.max = 15 )$tot.withinss})
    
    kResults <- data.frame(adatause, cluster = as.factor(rtkm$cluster))
    kResults2 <- data.frame(adatause, cluster = rtkm$cluster)
    
    rl <- as.data.frame(lapply(1:cnum, function(x){ r3 <- kResults[kResults$cluster == x, 
                                                                setdiff(names(kResults), 'cluster')] 
    r4 <- colSums(r3) / nrow(r3)
    r4
    }))
    names(rl) <- paste("cluster",1:cnum)
    
    
    
    allval=list(a=tsned2, b=kmdata,c=newdat, d=fit,e=ss,f=rl,g=kResults,h=kResults2)
    allval
  })
  
  output$packagePlot <- renderPlot({
    allval=models()
    datause=allval$h
    parcoord(datause, col=as.factor(datause$cluster))
  })
  
  output$packagePlot2 <- renderPlotly({
    allval=models()
    duse=as.data.frame(cbind(y=allval$e,x=1:10))
    plot_ly(duse, x = ~x, y = ~y, name = 'Test', type = 'scatter', mode = 'lines+markers')
  })
  
  
  output$packagePlot3 <- renderPlotly({
    allval=models()
    plot_ly(data = allval$c, x = ~allval$c[,1], y = ~allval$c[,2],color=as.factor(allval$c[,3]))
  })
  
  output$packagePlot4 <- renderPlot({
    allval=models()
    ggsurvplot(fit=allval$d, risk.table = TRUE,pval = TRUE,data=allval$b)
  })
  
  
  output$exploreclus<- renderUI({
    allval=models()
    items=names(allval$g[,-ncol(allval$g)])
    names(items)=items
    selectInput("exploreclus","Select variables to explore",items, multiple = T)
  })
  
  models2=eventReactive(input$action2,{
    adata=models()
    clususe=adata$g[input$exploreclus]
  
    kclususe <- data.frame(clususe, cluster = adata$g[,ncol(adata$g)])
    allval2=list(a=kclususe,b=input$exploreclus)
    allval2
  })
  
 output$scatterplot=renderPlot({
   allval=models2()
   kdata=allval$a
   ggpairs(kdata,columns =unlist(allval$b),aes(col=cluster),upper="blank")
 })
  
}
 





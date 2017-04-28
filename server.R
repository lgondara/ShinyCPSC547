function(input, output) {
  # set seed for reproducable tSNE
  set.seed(1000)
  
  # if no variables are selected, disable the run button
  observe({
    if (is.null(input$vars) || input$vars == "") {
      shinyjs::disable("action")
    } else {
      shinyjs::enable("action")
    }
  })
  
  # this piece of code hides and shows parallel coordinate and other plots based on variable selection
  observe({
    if (is.null(input$vars) || input$vars == "") {
      shinyjs::hide("parcoords")
      shinyjs::hide("packagePlot2")
      shinyjs::hide("packagePlot3")
      shinyjs::hide("packagePlot4")
      shinyjs::hide("packagePlot5")
    }
  })
  
  observeEvent(input$action, {
      shinyjs::hide("parcoords2")
      shinyjs::show("parcoords")
      shinyjs::show("packagePlot2")
      shinyjs::show("packagePlot3")
      shinyjs::show("packagePlot4")
      shinyjs::show("packagePlot5")
  })
  
  # read in the dataset, remove any rows with missing data
  info <- reactive({
    infile <- input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    t=read.table(infile$datapath, header = input$header, sep = input$sep)
    t=t[complete.cases(t),]
    return(t)
  })
  
  
  # show the variable names in the dataset for user selection
  output$vars<- renderUI({
    df <- info()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("vars","Select variables",items, multiple = T,selected = NULL)
  })
  
  
  # let user select time
  output$time<- renderUI({
    df <- info()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("time","Select time",items, multiple = F)
  })
  
  # let user select censoring indicator
  output$censor<- renderUI({
    df <- info()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("censor","Select censor",items, multiple = F)
  })
  
  # let user select number of clusters
  output$numclust<- renderUI({
    items=c(1:5)
    names(items)=items
    selectInput("numclust","Select cluster",items, multiple = F)
  })
  

# main part of app, calculates tSNE, k-means, variable significane by cluster and survival models and outputs a list of results to be used by other parts of the app
  models=eventReactive(input$action,{
    adata=info()
    adatause=adata[input$vars]
    timeout=adata[input$time]
    timeout=as.numeric(unlist(timeout))
    censor=adata[input$censor]
    censor=unlist(censor)
    cnum=input$numclust
    cnum=as.numeric(unlist(cnum))
    colors=c("#3366cc","#ff9900", "#109618", "#dc3912", "#990099")
    coloruse=colors[1:cnum]
    
    
    set.seed(1000)
    coll=Rtsne(adatause, check_duplicates = FALSE)
    tsned2=as.data.frame(coll$Y)
    
    rtkm=kmeans(tsned2,cnum)
    newdat=data.frame(tsned2, rtkm$cluster)
    
    cl=clusGap(tsned2, kmeans, 5, B = 100, verbose = F)
    optclus=with(cl,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
    rtkm2=kmeans(tsned2,optclus)

    kmdata=data.frame(timeout,censor,rtkm$cluster)
    fit <- survfit(Surv(timeout,censor==1) ~ rtkm.cluster, data=kmdata)
    
    kmdata2=data.frame(timeout,censor,rtkm2$cluster)
    fit2 <- survfit(Surv(timeout,censor==1) ~ rtkm2.cluster, data=kmdata2)
    
    
    
    k.max=10
    ss <- sapply(1:k.max, 
                 function(k){kmeans(tsned2, k, nstart=50,iter.max = 15 )$tot.withinss})
    
    kResults <- data.frame(adatause, cluster = as.factor(rtkm$cluster))
    kResults2 <- data.frame(adatause, cluster = rtkm$cluster)
    
    nums <- sapply(kResults, is.numeric)
    testcont=kResults[, nums]
    
    pp=lapply(testcont, function(y) kruskal.test(y~kResults$cluster)[3])
    pp=as.data.frame((pp))
    ppt = pp<0.05
    
    sigvar=testcont[,ppt]
    
    sigvarclus=data.frame(sigvar, cluster = as.factor(rtkm$cluster))
    
    
    
    allval=list(a=tsned2, b=kmdata,c=newdat, d=fit,e=ss,g=kResults,h=kResults2, i=fit2, j=kmdata2,k=coloruse, l=colors, m=sigvarclus)
    allval
  })
  
  
  # parallel coordinate plot for clustered data
  output$parcoords = renderParcoords({
    
    allval=models()
    datause=allval$h
    
    
    parcoords(datause, rownames=F, brushMode="1d", color = list(colorBy="cluster",colorScale=htmlwidgets::JS('d3.scale.category10()')))
    
  })
  
  # parallel coordinate plot for all variables
  output$parcoords2 = renderParcoords({
    
    
    data=info()
    if (is.null(data)) return(NULL)
    parcoords(data, rownames=F, brushMode="1d"
    )
  })
  
  
  # plot for MSE
  output$packagePlot2 <- renderPlotly({
    allval=models()
    duse=as.data.frame(cbind(y=allval$e,x=1:10))
    x <- list(
      title = "Number of clusters"
    )
    y <- list(
      title = "Mean Squared Error"
    )
    plot_ly(duse, x = ~x, y = ~y, name = 'Test', type = 'scatter', mode = 'lines+markers')%>%
      layout(xaxis = x, yaxis = y)
  })
  
  # tSNE scatter plot
  output$packagePlot3 <- renderPlotly({
    allval=models()
    
    x <- list(
      title = ""
    )
    y <- list(
      title = ""
    )
    plot_ly(data = allval$c, x = ~allval$c[,1], y = ~allval$c[,2],color=as.factor(allval$c[,3]),colors=allval$k)%>%
      layout(xaxis = x, yaxis = y)
  })
  
  # survival plot 1
  output$packagePlot4 <- renderPlot({
    allval=models()
    ggsurvplot(fit=allval$d, risk.table = F,pval = TRUE,data=allval$b,palette =allval$k)
  })
  
  # survival plot 2
  output$packagePlot5 <- renderPlot({
    allval=models()
    ggsurvplot(fit=allval$i, risk.table = F,pval = TRUE,data=allval$j,palette =allval$l)
  })
  
  
# scatterplot matrix, all variables
  output$scatterplot=renderPairsD3({
    allval=models()
    kdata=allval$g
    cluster=kdata$cluster
    pairsD3(kdata[,-ncol(kdata)],group=cluster,big = T, col=allval$k)
  })  
   
# scatterplot matrix, only statistically significantly different variables among clusters  
 output$scatterplot2=renderPairsD3({
   allval=models()
   kdata=allval$m
   cluster=kdata$cluster
   pairsD3(kdata[,-ncol(kdata)],group=cluster,big = T, col=allval$k)
 })
  
 }
 





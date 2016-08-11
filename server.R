require(lattice)
require(stats)
require(shiny)
require("RColorBrewer")
require("SVGAnnotation")
require("XML")
require('BayesFactor')


shinyServer(function(input,output) {
  
  rText <- reactive({
    logr = input$r
    return(2^logr)
  })
  
  bfAlt <- reactive({
    rscale = 2^input$r
    
    tstat = input$t
    N = input$N
    
    BF = ttest.tstat(tstat, N, rscale = rscale)$bf
    return(BF)
  })
  
  output$rText <- renderText({
    paste("r = ",round(rText(),4),sep="")
  })

  output$nullbfText <- renderText({
    paste("The Bayes factor is ",round(exp(-bfAlt()),4)," in favor of the null,",sep="")
  })

  output$altbfText <- renderText({
    paste("the Bayes factor is ", round(exp(bfAlt()), 4)," in favor of the alternative.",sep="")  
  })
  
  output$svg.grid <- reactive({
    #from lattice package documentation

    priorCol = rgb(0,0,1,.5)
    postCol  = rgb(1,0,0,.5)
    xlims = c(-4,4)
    
    rscale = 2^input$r
    
    tstat = input$t
    N = input$N
    
    BF = bfAlt()
    likeNull = dt(tstat,N-1,log=TRUE)
    norm = BF + likeNull
    
    postFunc = function(delta,tstat,N1,rscale, norm){
      exp(dt(tstat,N1-1,ncp=delta*sqrt(N1), log=TRUE) + dcauchy(delta,scale=rscale,log=TRUE) - norm) 
    }
    postFuncV = Vectorize(postFunc,"delta")
    
    
    xx <- seq(xlims[1]*1.1,xlims[2]*1.1,len=250)
    priory = dcauchy(xx,0,rscale)
    posty = postFuncV(xx,tstat,N,rscale,norm)
    
    atNull = dcauchy(0,0,rscale)
    
    doc = svgPlot( {
      plot(xx,posty/atNull,ty='l',lwd=2, col=postCol, 
           xlab = expression(delta), ylab = "density (relative to prior at 0)", 
           xlim = xlims*.90)
      lines(xx,priory/atNull,col=priorCol,lwd=2)

    abline(v=0,col="gray")
    abline(h=0,col="gray")
    })  
    
    tempsvg <- tempfile(fileext=".svg")
    on.exit(unlink(tempsvg))
    saveXML(doc, tempsvg)
    svgoutput <- readLines(tempsvg, n=-1)
    svgoutput
  })
})

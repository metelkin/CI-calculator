source("auxilary.R")

shinyServer(function(input, output) {
  
  confLevelRes<-reactive({
    switch(input$confLevel, "0.5"=0.5,"0.75"=0.75, "0.9"=0.9, "0.95"=0.95, "0.99"=0.99)
  })
  
  data<-reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)  
    read.delim(inFile$datapath)
  })
   
  Ncol<-reactive({
    ifelse(input$useFirstCol, 1, input$Npar)
  })
  
  stat<-reactive({
    F.0.best<-ifelse(input$auto, 
                     input$F.0.best,
                     min(data()[,"F.0."]))
    parbest<-ifelse(input$auto, 
                    input$parbest,
                    data()[which.min(data()[,"F.0."]),Ncol()]) ####
    F.0.crit<-ifelse(input$logF.0., F.0.best+qchisq(confLevelRes(),1), F.0.best*exp(qchisq(confLevelRes(),1)/input$Nexp))
    linFun<-approxfun(data()[,Ncol()], data()[,"F.0."]-F.0.crit) ####
    lr<-uniroot.all(linFun,c(min(data()[,Ncol()]),max(data()[,Ncol()])), ) ####
    data.frame(Name=c("F.0. critical","Left","Right", "Optimal value of F.0.", paste("Optimal value of", names(data())[Ncol()]), "Confidence level", "Number of experimental points", "Number of free parameters"),
               Value=c(F.0.crit, min(lr), max(lr), F.0.best, parbest, confLevelRes(), input$Nexp, input$Npar),
               stringsAsFactors=F)
  })
  
  stat.print<-function(stat)
  {
    r1<- -floor(log10(abs(stat()[1,2]-stat()[4,2])))+1
    r2<- -floor(log10(abs(stat()[2,2]-stat()[5,2])))+1
    r3<- -floor(log10(abs(stat()[3,2]-stat()[5,2])))+1
    stat.round<-c(round(stat[1,2], digits=r1),
                  round(stat[2,2],digits=r2),
                  round(stat[3,2],digits=r3),
                  round(stat[4,2],digits=r1),
                  round(stat[5,2],digits=max(r2,r3)),
                  stat[6,2], stat[7,2], stat[8,2])
    statToPrint<-data.frame(Name=stat[,1], Value=sprintf("%g",stat.round))
  }
  
  stat2<-reactive({
    F.0.best<-ifelse(input$auto, 
                     input$F.0.best,
                     min(data()[,"F.0."]))
    indexBest<-which.min(data()[,"F.0."])
    parbest<-ifelse(input$auto, 
                    input$parbest,
                    data()[indexBest,Ncol()])
    F.0.crit<-ifelse(input$logF.0., F.0.best+input$Nexp/(input$Nexp-input$Npar)*qchisq(0.999,1), F.0.best*exp(qchisq(0.999,1)/(input$Nexp-input$Npar)))
    linFun<-approxfun(data()[,Ncol()], data()[,"F.0."]-F.0.crit)
    Left<-uniroot.all(linFun,c(min(data()[,Ncol()]), data()[indexBest,Ncol()]))
    Left<-ifelse(length(Left)==0, min(data()[,Ncol()]), min(Left))
    Right<-uniroot.all(linFun,c(data()[indexBest,Ncol()], max(data()[,Ncol()])))
    Right<-ifelse(length(Right)==0, max(data()[,Ncol()]), max(Right))
    data.frame(Name=c("F.0. critical","Left","Right", "Optimal value of F.0.", paste("Optimal value of", names(data())[Ncol()]), "Confidence level", "Number of experimental points", "Number of free parameters"),
               Value=c(F.0.crit, Left, Right, F.0.best, parbest, 0.999, input$Nexp, Ncol()),
               stringsAsFactors=F)
  })
  
  ### auxilary functions
  plotOF<-function(limited=F){
    if (!is.null(data())){
      if (limited){ 
        plot(data()[,Ncol()], data()[,"F.0."], type="b", xlab=names(data())[Ncol()], ylab="F.0.", main="Limited profile", xlim=c(stat2()[2,2],stat2()[3,2]), ylim=c(stat2()[4,2],stat2()[1,2]))
      } else {
        plot(data()[,Ncol()], data()[,"F.0."], type="b", xlab=names(data())[Ncol()], ylab="F.0.", main="Whole profile") }
      # plot auxilary lines and points
      abline(v=stat()[5,2],col="red")
      abline(h=stat()[4,2],col="red",lty=2)
      points(stat()[5,2],stat()[4,2],col="red",pch=16)
      
      abline(h=stat()[1,2],col="blue",lty=2)
      abline(v=stat()[2:3,2],col="blue")
      points(stat()[2,2], stat()[1,2],  col="blue",pch=16)
      points(stat()[3,2], stat()[1,2],  col="blue",pch=16)
    }
  }
  
  plotConf<-function(limited=F){
    if (!is.null(data())){
      cl<-if (input$logF.0.){
          pchisq(q=(stat()[7,2]-stat()[8,2])/stat()[7,2]*(data()[,"F.0."]-stat()[4,2]), df=1)
        } else {
          pchisq(q=(stat()[7,2]-stat()[8,2])*log(data()[,"F.0."]/stat()[4,2]), df=1) }
      #cl<-pchisq(q=(stat()[7,2]-stat()[8,2])*log(data()[,"F.0."]/stat()[4,2]), df=1)
      if (limited){ 
        plot(data()[,Ncol()],cl,type="b",xlab=names(data())[Ncol()],ylab="Confidence level", ylim=c(0,1), main="Limited profile", xlim=c(stat2()[2,2],stat2()[3,2]))
      } else {
        plot(data()[,Ncol()],cl,type="b",xlab=names(data())[Ncol()],ylab="Confidence level", ylim=c(0,1), main="Whole profile") }
      # plot lines and points
      abline(v=stat()[5,2],col="red")
      abline(h=0,col="red",lty=2)
      points(stat()[5,2],0,col="red",pch=16)
      
      abline(h=confLevelRes(),col="blue",lty=2)
      abline(v=stat()[2:3,2],col="blue")
      points(stat()[2,2], confLevelRes(),  col="blue",pch=16)
      points(stat()[3,2], confLevelRes(),  col="blue",pch=16)
    }
  }
  
 ### visualization
  output$stat <- renderTable({
    if (!is.null(data())){
      stat.print(stat())[c(6,2,3,5,1,4,7,8),]
    }
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$plot1 <- renderPlot({
    plotOF()
  })
  
  output$plot1limited <- renderPlot({
    plotOF(limited=T)
  })
  
  output$plot2 <- renderPlot({
    plotConf()
  })
  
  output$plot2limited <- renderPlot({
    plotConf(limited=T)
  })
  
  output$downloadPDF <- downloadHandler(
    filename = "CI results.pdf",
    content = function(file) {
      pdf(file=file, height=5, width=5)
      grid.table(stat.print(stat())[c(6,2,3,5,1,4,7,8),])
      plotOF(T)
      plotConf(T)
      dev.off()
    }
  )
})

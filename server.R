# server.R

shinyServer(function(input, output, session) {
  
  #==========Calibrating Portfolio Specificaitons==============
  #-----------------Count NUmber of Assets----------------
  # Track the number of assets to render
  counter <- reactiveValues(n = 0)
  
  # Track all user inputs
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })
  
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })
  
  output$counter <- renderPrint(cat("# of Asset(s):", counter$n))
  
  # getTickers()
  getTickers<- function(n){
    tickers=c()
    for(i in 1:n){
      tickers= c(tickers,AllInputs()[[paste0("tickerIn", i)]])
    }
    return (tickers)
  }
  
  # nlvCalc()
  nlvCalc<- function(n){ 
    result=0
    for(i in 1:n){
      result= result+AllInputs()[[paste0("quantityIn", i)]]*AllInputs()[[paste0("mktPriceIn", i)]]
    }
    return (result)
  }
  
  # getAllocation()
  getAllocation<-function(n){
    nlv=nlvCalc(n)
    alloc=c()
    for(i in 1:n){
      alloc=c(alloc, AllInputs()[[paste0("quantityIn", i)]]*AllInputs()[[paste0("mktPriceIn", i)]]/nlv)
    }
    return (alloc)
  }
  
  #----------------Assets layout------------------------------
  assets <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          fluidPage(
            fluidRow(
              column(3, textInput(inputId = paste0("tickerIn", i),
                                  label = paste0("Ticker #", i), 
                                  value = AllInputs()[[paste0("tickerIn", i)]],
                                  placeholder="(e.g., AAPL)")),
              column(3, numericInput(inputId = paste0("quantityIn", i),
                                     label=paste0("Quantity #", i),
                                     value = AllInputs()[[paste0("quantityIn", i)]])),
              column(3, numericInput(inputId = paste0("mktPriceIn", i),
                                     label = paste0("Current Price #", i), 
                                     value = AllInputs()[[paste0("mktPriceIn", i)]])),
              column(3, textOutput(outputId = paste0("allocOut", i)))
            )
          )
        })
      })
    }
  })
  
  output$asset_ui <- renderUI({ 
    # Calulating current asset allocation
    lapply(seq_len(counter$n), function(i) {
      output[[paste0("allocOut", i)]]<-renderPrint(cat("Weight #",i,": \n", AllInputs()[[paste0("quantityIn", i)]]*AllInputs()[[paste0("mktPriceIn", i)]]/nlvCalc(counter$n)))
    })
    assets()
  })
  
  #-------------------------Calculate NLV-------------------
  output$nlv <- renderPrint(cat("Net Liquidation Value:", nlvCalc(counter$n)))
  
  #--------------Calculate Prices and Returns for each asset------------
  prices<-reactive({
    getPrices(input$tickerInputUni,input$dateRangeUni[1], input$dateRangeUni[2])
  })
  
  lretsD<-reactive({
    getLogReturnsD(prices())
  })

  rets<-reactive({
    getReturns(getTickers(counter$n),input$dateRange[1], input$dateRange[2])
  })
  #====================Univariate Analysis====================
  #========================================================
  # =====================Running Exploratory Data Analysis
  output$timePlots<-renderPlot(NULL)
  output$sumStatsTable <- renderTable(NULL)
  
  # create time plots 
  observeEvent(input$runTimePlots, {
    output$timePlots<-renderPlot({
      isolate({
        # Time plots
        ## price ts
        tp1 = ggplot2::qplot(index(prices()), prices(), xlab="Date", ylab="Adjusted Closing Price", main=paste("Time Plot of", input$tickerInputUni, "Prices"),  geom="line")
        ## log-returns ts
        tp2 = ggplot2::qplot(index(prices())[-1], lretsD()$lrets, ylab="Log-returns", xlab="Date", main=paste("Time Plot of", input$tickerInputUni, "Log-returns"), geom = "line")
        
        grid.arrange(tp1, tp2, nrow=2, top = paste(input$tickerInputUni, top="Daily Price Data"))
      })
    })
  })
  # create summary statistics 
  observeEvent(input$runSumStats, {
    lrets<-lretsD()$lrets
    output$sumStatsTable<-renderTable({
      isolate({
        # summary statistics
        data.frame(statistic=c("Min", "1st IQR", "Median", "3rd IQR", "Max", "Mean", "Std. Dev.", "Skewness", "Excess Kurtosis"), value=c(fivenum(lrets), mean(lrets), sd(lrets), skewness(lrets)[1], kurtosis(lrets, method="excess")[1]))
      })
    })
    output$sumStatsBoxPlot<-renderPlot({
      isolate({
        ggplot2::qplot(y = lrets, geom='boxplot', ylab='log-returns', main=paste("Barplot of ", input$tickerInputUni, "Log-returns"))
      })
    })
  })
  # =====================Creating Histogram and QQplot pairs for each univariate model
  output$uniTailHistQQPlots<-renderPlot(NULL)
  output$uniTailRiskTable <- renderTable(NULL)
  observeEvent(input$runUniTailHistQQPlots, {
    if (is.null(input$uniTailCheck)){
      output$uniTailHistQQPlotsText<-renderText({"No model(s) selected."})
      return()  
    }else{
      output$uniTailHistQQPlotsText<-renderText({NULL})
    }
    output$uniTailHistQQPlots<-renderPlot({
      isolate({
        distrPlotList<-getUniTailHistQQPlots(lretsD(), input$uniTailCheck)
        grid.arrange(grobs = distrPlotList,
                     ncol=4,
                     top = paste("Distribution Plots of", input$tickerInputUni,"Log-returns"),
                     gp = gpar(fontsize = 15, font = 8, face="bold"))
      })
    })
  })
  
  # ==============Creating risk table for univariate distribution
  observeEvent(input$runUniTailRiskTable, {
    if (is.null(input$uniTailCheck)){
      output$uniTailRiskTableText<-renderText({"No model(s) selected."})
      return()  
    }else{
      output$uniTailRiskTableText<-renderText({NULL})
    }
    isolate({
      shinyCatch({uniTailRiskD<-getUniTailRiskTable(lretsD()$lrets, input$VaRlevelUni, input$uniTailCheck)},blocking_level="error")
    })
    output$uniTailRiskTable<-renderTable({
      isolate({
        uniTailRiskD
      })
    })
    output$uniTailRiskBarPlot<-renderPlot({
      isolate({
        # melt model results into variable-value pairs
        uniTailRiskMelt=melt(uniTailRiskD)
        
        uniTailRiskBarPlot<-list(ggplot(data=uniTailRiskMelt, aes(x=Model, y=value, fill=variable)) +
          geom_bar(stat='identity', position="dodge")+
          geom_text(aes(label=sprintf('%.2f %%',value*100)),
                    position=position_dodge(width=0.9),
                    size=4,
                    angle=10,
                    check_overlap = FALSE)+
          ggtitle("Barplot of Univariate Model Value-at-Risk & Expected Shortfall")+
          labs(x= 'Univariate Model', y = 'Proportion of Net Liquidation Value')+
          theme(text = element_text(size = 15),
                plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                axis.text.x = element_text(angle = 25),
                legend.title = element_text(size=15),
                legend.text = element_text(size=15)))
        
        grid.arrange(grobs = uniTailRiskBarPlot,
                     ncol=3,
                     gp = gpar(fontsize = 15, font = 8, face="bold"))
      })
    })
  })
  # =====================Creating Empirical CDF Plots
  output$ecdfPlots<-renderPlot(NULL)
  observeEvent(input$runECDFPlots, {
    output$ecdfPlots<-renderPlot({
      isolate({
        edfPlotsList<-getECDFPlots(-lretsD()$lrets)
        grid.arrange(grobs = edfPlotsList,
                     ncol=4,
                     top = paste("ECDF Plots of", input$tickerInputUni,"Negative Log-returns"),
                     gp = gpar(fontsize = 15, font = 8, face="bold"))
      })
    })
  })
  # ========================Creating Pareto Shape Plot
  output$paretoShapePlot<-renderPlot(NULL)
  observeEvent(input$runParetoShapePlot, {
    output$paretoShapePlot<-renderPlot({
      isolate({
        shinyCatch({shape(-lretsD()$lrets, models = 30, start = 400, end = 20, ci=.90, reverse = TRUE, auto.scale = TRUE)}, blocking_level="error")
      })
    })
  })
  # ====================Creating risk table under GPD assumption
  output$paretoTailRiskTable<-renderTable(NULL)
  output$paretoTailRiskBarPlot<-renderPlot(NULL)
  observeEvent(input$runParetoTailRiskTable, {
    isolate({
      shinyCatch({paretoTailRiskD<-getParetoTailRiskTable(-lretsD()$lrets, input$VaRlevelUni, input$paretoThreshold)}, blocking_level="error")
    })
    output$paretoTailRiskTable<-renderTable({
      isolate({
        paretoTailRiskD
      })
    })
    output$paretoTailRiskBarPlot<-renderPlot({
      isolate({
        # melt model results into variable-value pairs
        paretoTailRiskMelt=melt(paretoTailRiskD)
        ggplot(data=paretoTailRiskMelt, aes(x=Model, y=value, fill=variable)) +
                                      geom_bar(stat='identity', position="dodge")+
                                      geom_text(aes(label=sprintf('%.2f %%',value*100)),
                                                position=position_dodge(width=0.9),
                                                size=4,
                                                angle=10,
                                                check_overlap = FALSE)+
                                      ggtitle("Barplot of GPD Value-at-Risk & Expected Shortfall")+
                                      labs(x= 'Tail Model', y = 'Proportion of Net Liquidation Value')+
                                      theme(text = element_text(size = 15),
                                            plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                                            axis.text.x = element_text(angle = 25),
                                            legend.title = element_text(size=15),
                                            legend.text = element_text(size=15))
        
      })
    })
    output$paretoTailFitPlot<-renderPlot({
      isolate({
        shinyCatch({gpd_est = gpd(-lretsD()$lrets, input$paretoThreshold)}, blocking_level="error")
        tailplot(gpd_est)
      })
    })
    output$paretoTailQuantilePlot<-renderPlot({
      isolate({
        shinyCatch({quant(-lretsD()$lrets, p = 1-input$VaRlevelUni, models = 30, start = 400, end = 20,
              reverse=TRUE, ci =FALSE, auto.scale = TRUE, labels = TRUE)}, blocking_level="error")
      })
    })
  })
  # ==================Summary Table from GARCH Modeling
  lrets.ts<-reactive({
    ts(lretsD()$lrets, start=c(year(input$dateRangeUni[1]),month(input$dateRangeUni[1])), frequency=253, names=c("logret"))
  })

  # Fittign GARCH Models
  normLretsGARCHFit<-reactive({garchFit(substitute(~arma(arma_p, arma_q)+garch(sigma_p,arch_q), list(arma_p=input$arma_p, arma_q=input$arma_q, sigma_p=input$sigma_p, arch_q=input$arch_q)), data= lrets.ts(), cond.dist = c("norm"), include.mean = TRUE, algorithm = c("nlminb"), hessian = c("ropt"), trace=F)})
  stdLretsGARCHFit<-reactive({garchFit(substitute(~arma(arma_p, arma_q)+garch(sigma_p,arch_q), list(arma_p=input$arma_p, arma_q=input$arma_q, sigma_p=input$sigma_p, arch_q=input$arch_q)), data= lrets.ts(), cond.dist = c("std"), include.mean = TRUE, algorithm = c("nlminb"), hessian = c("ropt"), trace=F)})
  gedLretsGARCHFit<-reactive({garchFit(substitute(~arma(arma_p, arma_q)+garch(sigma_p,arch_q), list(arma_p=input$arma_p, arma_q=input$arma_q, sigma_p=input$sigma_p, arch_q=input$arch_q)), data= lrets.ts(), cond.dist = c("ged"), include.mean = TRUE, algorithm = c("nlminb"), hessian = c("ropt"), trace=F)})
  
  output$normGARCHSummaryTable <- renderPrint(NULL)
  output$stdGARCHSummaryTable <- renderPrint(NULL)
  output$gedGARCHSummaryTable <- renderPrint(NULL)
  observeEvent(input$runGARCHSummaryTable, {
    output$normGARCHSummaryTable<-renderPrint({
      isolate({
        if (!"norm" %in% input$garchResidCheck){
          print("Normal innovations not selected.")
        }else{
          tryCatch(expr = {summary(normLretsGARCHFit())}, error=function(e){print("Normal innovations not solvable.")})
        }
      })
    }) 
    output$stdGARCHSummaryTable<-renderPrint({
      isolate({
        if (!"std" %in% input$garchResidCheck){
          print("Student t innovations not selected.")
        }else{
          tryCatch(expr = {summary(stdLretsGARCHFit())}, error=function(e){print("Student t innovations not solvable.")})
        }
      })
    }) 
    output$gedGARCHSummaryTable<-renderPrint({
      isolate({
        if (!"ged" %in% input$garchResidCheck){
          print("GED innovations not selected.")
        }else{
          tryCatch(expr = {summary(gedLretsGARCHFit())}, error=function(e){print("GED innovations not solvable.")})
        }
      })
    }) 
  })

  # ==================Estimated volatilities and residuals
  output$normEstCondVolResidPlots <- renderPlot(NULL)
  output$stdEstCondVolResidPlots <- renderPlot(NULL)
  output$gedEstCondVolResidPlots <- renderPlot(NULL)
  observeEvent(input$runEstCondVolResidPlots, {
    # Normal innovations
    output$normEstCondVolResidPlots<-renderPlot({
      isolate({
        if (!"norm" %in% input$garchResidCheck){
          return()
        }else{
          normEstPlotsList<-getNormEstCondVolResidPlots(input$tickerInputUni, prices(), lretsD(), normLretsGARCHFit())
          grid.arrange(grobs = normEstPlotsList,
                       nrow=2,
                       gp = gpar(fontsize = 15, font = 8, face="bold"))
        }
      })
    })
    # Student t innovations
    output$stdEstCondVolResidPlots<-renderPlot({
        isolate({
          if (!"std" %in% input$garchResidCheck){
            return()
          }else{
            stdEstPlotsList<-getStdEstCondVolResidPlots(input$tickerInputUni, prices(), lretsD(), stdLretsGARCHFit())
            grid.arrange(grobs = stdEstPlotsList,
                         nrow=2,
                         gp = gpar(fontsize = 15, font = 8, face="bold"))
          }
      })
    })
    # GED innovations
    output$gedEstCondVolResidPlots<-renderPlot({
      isolate({
        if (!"ged" %in% input$garchResidCheck){
          return()
        }else{
          gedEstPlotsList<-getGedEstCondVolResidPlots(input$tickerInputUni, prices(), lretsD(), gedLretsGARCHFit())
          grid.arrange(grobs = gedEstPlotsList,
                       nrow=2,
                       gp = gpar(fontsize = 15, font = 8, face="bold"))
        }
      })
    })
  })

  # ==================Residuals Histogram and QQ Plots
  output$normResidHistQQPlot <- renderPlot(NULL)
  output$stdResidHistQQPlot <- renderPlot(NULL)
  output$gedResidHistQQPlot <- renderPlot(NULL)
  observeEvent(input$runResidHistQQPlots, {
    # Normal innovations
    output$normResidHistQQPlot<-renderPlot({
      isolate({
        if (!"norm" %in% input$garchResidCheck){
          return()
        }else{
          normResidDistrPlotsList<-getNormResidHistQQPlot(input$tickerInputUni,normLretsGARCHFit())
          grid.arrange(grobs = normResidDistrPlotsList,
                       ncol=2,
                       gp = gpar(fontsize = 15, font = 8, face="bold"))
        }
      })
    })
    # Student t innovations
    output$stdResidHistQQPlot<-renderPlot({
      isolate({
        if (!"std" %in% input$garchResidCheck){
          return()
        }else{
          stdResidDistrPlotsList<-getStdResidHistQQPlot(input$tickerInputUni, stdLretsGARCHFit())
          grid.arrange(grobs = stdResidDistrPlotsList,
                       ncol=2,
                       gp = gpar(fontsize = 15, font = 8, face="bold"))
        }
      })
    })
    # GED innovations
    output$gedResidHistQQPlot<-renderPlot({
      isolate({
        if (!"ged" %in% input$garchResidCheck){
          return()
        }else{
          gedResidDistrPlotsList<-getGedResidHistQQPlot(input$tickerInputUni, gedLretsGARCHFit())
          grid.arrange(grobs = gedResidDistrPlotsList,
                       ncol=2,
                       gp = gpar(fontsize = 15, font = 8, face="bold"))
        }
      })
    })
  })
  
  # ==================ACF of Squared Residuals
  output$normSquaredResidACF <- renderPlot(NULL)
  output$stdSquaredResidACF <- renderPlot(NULL)
  output$gedSquaredResidACF <- renderPlot(NULL)
  observeEvent(input$runSquaredResidACF, {
    # Normal innovations
    output$normSquaredResidACF<-renderPlot({
      isolate({
        if (!"norm" %in% input$garchResidCheck){
          return()
        }else{
          sigma_t = normLretsGARCHFit()@sigma.t
          ggAcf((normLretsGARCHFit()@residuals / sigma_t)^2, main=paste("ACF of", input$tickerInputUni, "'s Squared Residuals"))+ labs(y="Autocorrelation")
        }
      })
    })
    # Student t innovations
    output$stdSquaredResidACF<-renderPlot({
      isolate({
        if (!"std" %in% input$garchResidCheck){
          return()
        }else{
          sigma_t = stdLretsGARCHFit()@sigma.t
          ggAcf((stdLretsGARCHFit()@residuals / sigma_t)^2, main=paste("ACF of", input$tickerInputUni, "'s Squared Residuals"))+ labs(y="Autocorrelation")
        }
      })
    })
    # GED innovations
    output$gedSquaredResidACF<-renderPlot({
      isolate({
        if (!"ged" %in% input$garchResidCheck){
          return()
        }else{
          sigma_t = gedLretsGARCHFit()@sigma.t
          ggAcf((gedLretsGARCHFit()@residuals / sigma_t)^2, main=paste("ACF of", input$tickerInputUni, "'s Squared Residuals"))+ labs(y="Autocorrelation")
        }
      })
    })
  })
  
  # ==============Creating risk table for ARCH Models
  output$garchTailRiskTable<-renderTable(NULL)
  observeEvent(input$runGarchTailRiskTable, {
    output$garchTailRiskTableText<-renderText(NULL)
    isolate({
      if(is.null(input$garchResidCheck)){
        output$garchTailRiskTableText<-renderText("No innovation(s) selected.")
      }
    })
    isolate({
      garchTailRiskList=list()
      if ("norm" %in% input$garchResidCheck){
        tryCatch({
          garchTailRiskList[["norm"]]=normLretsGARCHFit()
        },error=function(e){
          output$garchTailRiskTableText<-renderText("Normal innovations not solvable.")
          return()
        })
      }
      if ("std" %in% input$garchResidCheck){
        tryCatch({
          garchTailRiskList[["std"]]=stdLretsGARCHFit()
        },error=function(e){
          output$garchTailRiskTableText<-renderText("Student t innovations not solvable.")
          return()
        })
      }
      if ("ged" %in% input$garchResidCheck){
        tryCatch({garchTailRiskList[["ged"]]=gedLretsGARCHFit()},error=function(e){
          output$garchTailRiskTableText<-renderText("GED innovations not solvable.")
          return()
        })
      }
      shinyCatch({garchTailRiskD<-getGedGARCHTailRiskTable(input$VaRlevelUni, garchTailRiskList)},blocking_level="error")
    })
    output$garchTailRiskTable<-renderTable({
      isolate({
        garchTailRiskD
      })
    })
    output$garchTailRiskBarPlot<-renderPlot({
      isolate({
        # melt model results into variable-value pairs
        garchTailRiskMelt=melt(garchTailRiskD)
        
        garchTailRiskBarPlot<-list(ggplot(data=garchTailRiskMelt, aes(x=Innovations, y=value, fill=variable)) +
                                   geom_bar(stat='identity', position="dodge")+
                                   geom_text(aes(label=sprintf('%.2f %%',value*100)),
                                             position=position_dodge(width=0.9),
                                             size=4,
                                             angle=10,
                                             check_overlap = FALSE)+
                                   ggtitle(paste("Barplot of ARMA(",input$arma_p,",",input$arma_q,")+GARCH(",input$sigma_p,",",input$arch_q,") Model Value-at-Risk & Expected Shortfall"))+
                                   labs(x= 'Innovations', y = 'Proportion of Net Liquidation Value')+
                                   theme(text = element_text(size = 15),
                                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                                         axis.text.x = element_text(angle = 25),
                                         legend.title = element_text(size=15),
                                         legend.text = element_text(size=15)))
        
        grid.arrange(grobs = garchTailRiskBarPlot,
                     ncol=3,
                     gp = gpar(fontsize = 15, font = 8, face="bold"))
      })
    })
  })

  
  #====================Multivariate Analysis====================
  #========================================================
  # =====================Running Correlation Diagnostics
  output$scatterMat <- renderPlot(NULL)
  output$qqPlots <- renderPlot(NULL)
  output$corrMat <- renderTable(NULL)
  
  # Creating Scatter Matrix between returns
  observeEvent(input$runScatterMatrix, {
    output$scatterMat<-renderPlot({
      isolate({
        shinyCatch({pairs(rets(), main="Scatter Matrix for Asset Returns")},blocking_level="error")
      })
    })
  })
  # Creating Normal QQ-plot returns of each asset
  observeEvent(input$runQQPlots, {
    
    output$qqPlots<-renderPlot({
      isolate({
        shinyCatch({getQQPlots(rets(), input$qqMethod)},blocking_level="error")
      })
    })
  })
  # Correlation Matrix
  observeEvent(input$runCorrMatrix, {
    # Creating Correlation Matrix between returns
    output$corrMat<-renderPlot({
      isolate({
        shinyCatch({corrplot(cor(rets(), method=input$corrMethod), method="number")},blocking_level="error")
      })
    })
  })
  
  #========================Running Tail Diagnostics
  #--------------------Multivariate--------------------------
  output$multiTailRiskTable <- renderTable(NULL)
  output$multiTailBiPlots <- renderPlot(NULL)
  ## actionary results
  observeEvent(input$runMultiTailRiskTable, {
    if (is.null(input$multiTailCheck)){
      output$multiTailRiskTableText<-renderText({"No model(s) selected."})
      return()  
    }else{
      output$multiTailRiskTableText<-renderText({NULL})
    }
    shinyCatch({multiTailRiskD<-getMultiTailRiskTable(rets(), getAllocation(counter$n), nlvCalc(counter$n), input$VaRlevel, input$multiTailCheck)},blocking_level="error")
    output$multiTailRiskTable<-renderTable({
      isolate({
        multiTailRiskD
      })
    })
    output$multiTailRiskBarPlot<-renderPlot({
      isolate({
        # melt model results into variable-value pairs
        multiTailRiskMelt=melt(multiTailRiskD[,c("Model", "rVaR", "rES")])
        
        multiTailRiskBarPlot<-list(ggplot(data=multiTailRiskMelt, aes(x=Model, y=value, fill=variable)) +
          geom_bar(stat='identity', position="dodge")+
          geom_text(aes(label=paste(sprintf('%.2f %%',value*100),"($",round(value* nlvCalc(counter$n),3),")")),
                    position=position_dodge(width=0.9),
                    size=4,
                    angle=10,
                    check_overlap = FALSE)+
          ggtitle("Barplot of Multivariate Model Value-at-Risk & Expected Shortfall")+
          labs(x= 'Multivariate Model', y = 'Percentage of Net Liquidation Value')+
          theme(text = element_text(size = 15),
                plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                axis.text.x = element_text(angle = 25),
                legend.title = element_text(size=15),
                legend.text = element_text(size=15)))
        grid.arrange(grobs = multiTailRiskBarPlot,
                     ncol=3,
                     gp = gpar(fontsize = 15, font = 8, face="bold"))
      })
    })
  })
  observeEvent(input$runMultiTailBiPlots, {
    if (is.null(input$multiTailCheck)){
      output$multiTailBiPlotsText<-renderText({"No model(s) selected."})
      return()  
    }else{
      output$multiTailBiPlotsText<-renderText({NULL})
    }
    output$multiTailBiPlots<-renderPlot({
      isolate({
        shinyCatch({getMultiTailBiPlots(rets(), getAllocation(counter$n), nlvCalc(counter$n), input$VaRlevel, input$multiTailCheck)},blocking_level="error")
      })
    })
  })
  #--------------------Copula--------------------------
  output$copTailRiskTable <- renderTable(NULL)
  output$copTailBiPlots <- renderPlot(NULL)
  
  ## actionary results
  observeEvent(input$runCopTailRiskTable, {
    if (is.null(input$copTailCheck)){
      output$copTailRiskTableText<-renderText({"No model(s) selected."})
      return()  
    }else{
      output$copTailRiskTableText<-renderText({NULL})
    }
    shinyCatch({copTailRiskD<-getCopTailRiskTable(rets(), getAllocation(counter$n), input$corrMethod, nlvCalc(counter$n), input$VaRlevel, input$copTailCheck)},blocking_level="error")
    output$copTailRiskTable<-renderTable({
      isolate({
        copTailRiskD
      })
    })
    output$copTailRiskBarPlot<-renderPlot({
      isolate({
        # melt model results into variable-value pairs
        copTailRiskMelt=melt(copTailRiskD[,c("Model", "rVaR", "rES")])
        
        copTailRiskBarPlot<-list(ggplot(data=copTailRiskMelt, aes(x=Model, y=value, fill=variable)) +
          geom_bar(stat='identity', position="dodge")+
          geom_text(aes(label=paste(sprintf('%.2f %%',value*100),"($",round(value*nlvCalc(counter$n),3),")")),
                    position=position_dodge(width=0.9),
                    size=4,
                    angle=10,
                    check_overlap = FALSE)+
          ggtitle("Barplot of Copula Model Value-at-Risk & Expected Shortfall")+
          labs(x= 'Copula Model', y = 'Percentage of Net Liquidation Value')+
          theme(text = element_text(size = 15),
                plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                axis.text.x = element_text(angle = 25),
                legend.title = element_text(size=15),
                legend.text = element_text(size=15)))
        grid.arrange(grobs = copTailRiskBarPlot,
                     ncol=3,
                     gp = gpar(fontsize = 15, font = 8, face="bold"))
        
      })
    })
  })
  
  observeEvent(input$runCopTailBiPlots, {
    if (is.null(input$copTailCheck)){
      output$copTailBiPlotsText<-renderText({"No model(s) selected."})
      return()  
    }else{
      output$copTailBiPlotsText<-renderText({NULL})
    }
    output$copTailBiPlots<-renderPlot({
      isolate({
        shinyCatch({getCopTailBiPlots(rets(), getAllocation(counter$n), input$corrMethod, nlvCalc(counter$n), input$VaRlevel, input$copTailCheck)},blocking_level="error")
      })
    })
  })
  
  # ======================Running Portfolio Evaluation
  output$optPortAllocTable <- renderTable(NULL)
  output$RRPlots <- renderPlot(NULL)
  observeEvent(input$runOptPortAllocTable, {
    
    shinyCatch({
      optPortAllocTableList<-getOptPortAllocTableList(rets(), getAllocation(counter$n), input$muPrange, input$riskFreeRate, input$shortSellCheck)
    },blocking_level="error")
    optPortAllocD=optPortAllocTableList[[1]]
    optPortPerformD=optPortAllocTableList[[2]]
    
    ### Allocation table
    output$optPortAllocTable<-renderTable({
      isolate({
        optPortAllocD
      })
    })
    ### Allocation plot
    output$optPortAllocBarPlot<-renderPlot({
      isolate({
        #------------------Creating Barplots for Asset Weights
        # A manipulated dataframe
        tmp=as.data.frame(t(optPortAllocD[,-1])); colnames(tmp)=optPortAllocD$Tickers
        portNum=dim(tmp)[1]
        # start constructing weigth plots
        weightPlotList = list()
        for (c in 1:portNum) {
          df=tmp[c,]
          # Create long form version of the return matrix
          allocDF<-melt(df, na.rm = FALSE, variable.name="tickers", value.name = "weights")
          weightPlotList[[c]] = ggplot(data=allocDF, aes(x=tickers, y=weights, fill=tickers)) +
            geom_bar(stat='identity', position="dodge")+
            geom_text(aes(label=sprintf('%.03f %%',weights*100)), 
                      position=position_dodge(width=2), 
                      size=4,
                      angle=10, 
                      check_overlap = FALSE)+
            ggtitle(switch(rownames(df), "TAN.SS"="Tangent With Short-sell (TAN.SS)", "MINVAR.SS"="Minimum Variance With Short-sell (MINVAR.SS)", "TAN.NSS"="Tangent Without Short-sell (TAN.NSS)", "MINVAR.NSS"="Minimum Variance Without Short-sell (MINVAR.NSS)", "CURRENT"="Current"),
                    subtitle = paste("Exp. Ret.:", round(optPortPerformD[c,2],5), ", Std. Dev.:", round(optPortPerformD[c,3],5)))+
            labs(x= 'Assets', y = 'Weight (%)')+ 
            theme(text = element_text(size = 15),
                  plot.title = element_text(size=10, face="bold", margin = margin(10, 0, 10, 0)), 
                  axis.text.x = element_text(angle = 45),
                  legend.title = element_text(size=15),
                  legend.text = element_text(size=15),
                  legend.position=ifelse(c==portNum,"right", "none"))
        }
        
        grid.arrange(grobs = weightPlotList,
                     ncol = 5,
                     gp = gpar(fontsize = 13, font = 8, face="bold"))
        
      })
    })
    ### Risk-to-reward plot
    output$RRPlots<-renderPlot({
      isolate({
        grid.arrange(grobs=optPortAllocTableList[[3]],
                     ncol = 4)
      })
    })
  })
})
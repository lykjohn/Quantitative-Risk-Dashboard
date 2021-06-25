library(corrplot)
library(tidyr)
library(metR) # long to install
library(reshape2)
library(ggplot2)
library(gridExtra)
library(quantmod)
library(purrr)
library(MASS)
library(mnormt)
require(nimble) # long to install
require(evir)
require(fBasics)
library(fGarch)
library(fCopulae)
library(copula)
library(quadprog)
library(tseries)
library(lubridate)
library(forecast)

#################getPrices#########################
getPrices<-function(ticker, start_date, end_date){  
  # Inherit price data into each symbol
  getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
  # Use Adjusted prices to form columns of prices
  prices<-reduce(map(ticker, function(x) Ad(get(x))), merge) 
  colnames(prices)<-gsub("\\..*","",colnames(prices)) 
  return(prices)
}
#################getLogReturnsD#########################
getLogReturnsD<-function(prices){  
  lretsD<-as.data.frame(apply(prices, 2, function(x) diff(log(x))))
  colnames(lretsD)="lrets"
  return(lretsD)
}
#################getUniTailHistQQPlots#########################

getUniTailHistQQPlots<-function(lretsD, distrType){ 
  mu_hat=mean(lretsD$lrets)
  sigma_hat=sd(lretsD$lrets)
  kappa = kurtosis(lretsD$lrets, method = 'excess')[1]
  
  ## histogram & qqplot
  distrPlotList=list()
  for (distr in distrType){
    switch(distr, 
           "Normal"={
             # Normal
             normal_param_est=as.list(MASS::fitdistr(lretsD$lrets, "normal")$estimate)
             distrPlotList[[paste(distr,"hist")]]=ggplot(lretsD, aes(x=lrets))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Log-returns', title=paste("Histogram with", distr, "Fit"))+ stat_function(fun = dnorm, args = normal_param_est)
             distrPlotList[[paste(distr,"qq")]]=ggplot(lretsD, aes(sample=lrets))+ stat_qq(distribution = qnorm, dparams=normal_param_est, color="darkred") + stat_qq_line(distribution = qnorm, dparams=normal_param_est, color="steelblue")+ggtitle(paste("QQ Plot with", distr, "Fit"))
           },
           "Student t"={
             # Student t
             kappa = kurtosis(lretsD$lrets, method = 'excess')[1]
             t_param_est=list(mean=mu_hat, sd=sigma_hat, nu= ifelse(4 + 6 / kappa>2, 4 + 6 / kappa, 2.01))
             names(t_param_est)=c("mean", "sd", "nu")
             distrPlotList[[paste(distr,"hist")]]=ggplot(lretsD, aes(x=lrets))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Log-returns', title=paste("Histogram with", distr, "Fit"))+ stat_function(fun = dstd, args = t_param_est)
             distrPlotList[[paste(distr,"qq")]]=ggplot(lretsD, aes(sample=lrets))+ stat_qq(distribution = qstd, dparams = t_param_est, color="darkred") + stat_qq_line(distribution = qstd, dparams = t_param_est, color="steelblue")+ggtitle(paste("QQ Plot with", distr, "Fit"))
           },
           "Double Exponential"={
             # DExp
             dexp_param_est=list()
             dexp_param_est$location=median(lretsD$lrets)
             dexp_param_est$scale=sum(abs(lretsD$lrets-dexp_param_est$location))/length(lretsD$lrets)
             distrPlotList[[paste(distr,"hist")]]=ggplot(lretsD, aes(x=lrets))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Log-returns', title=paste("Histogram with", distr, "Fit"))+ stat_function(fun = ddexp, args = dexp_param_est)
             distrPlotList[[paste(distr,"qq")]]=ggplot(lretsD, aes(sample=lrets))+ stat_qq(distribution = qdexp,dparams=dexp_param_est, color="darkred")+ stat_qq_line(distribution = qdexp,dparams=dexp_param_est, color="steelblue")+ggtitle(paste("QQ Plot with", distr, "Fit"))
           },
           "Generalized Error Distribution"={
             # GED
             ged_param_est=as.list(gedFit(lretsD$lrets)$par)
             distrPlotList[[paste(distr,"hist")]]=ggplot(lretsD, aes(x=lrets))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Log-returns', title=paste("Histogram with", distr, "Fit"))+ stat_function(fun = dged, args = ged_param_est)
             distrPlotList[[paste(distr,"qq")]]=ggplot(lretsD, aes(sample=lrets))+ stat_qq(distribution = qged, dparams=ged_param_est, color="darkred")+ stat_qq_line(distribution = qged, dparams=ged_param_est, color="steelblue")+ggtitle(paste("QQ Plot with", distr, "Fit"))
           }
    )
  }
  return(distrPlotList)
}

################getUniTailRiskTable######################
getUniTailRiskTable<-function(lrets, q, distrType){
  
  mu_hat=mean(lrets)
  sigma_hat=sd(lrets)
  kappa = kurtosis(lrets, method = 'excess')[1]
  N = 1e7
  
  # containers
  Model<-c()
  rVaR<-c()
  rES<-c()
  
  for (distr in distrType){
    switch(distr, 
           "Normal"={
             # Normal
             normal_param_est=as.list(MASS::fitdistr(lrets, "normal")$estimate)
             rVaR_norm=-(exp(qnorm(q, mean=mu_hat, sd=sigma_hat))-1)
             simLoss_norm = -(exp(rnorm(N, mean=mu_hat, sd=sigma_hat))-1)
             rES_norm = mean(simLoss_norm[simLoss_norm > rVaR_norm])
             Model<-c(Model, distr)
             rVaR<-c(rVaR, rVaR_norm)
             rES<-c(rES, rES_norm)
           },
           "Student t"={
             # Student t
             t_param_est=list(mean=mu_hat, sd=sigma_hat, nu= ifelse(4 + 6 / kappa>2, 4 + 6 / kappa, 2.01))
             names(t_param_est)=c("mean", "sd", "nu")
             rVaR_t=-(exp(qstd(q, mean=mu_hat, sd=sigma_hat, nu= 4 + 6 / kappa))-1)
             simLoss_t = -(exp(rstd(N, mean=mu_hat, sd=sigma_hat, nu= 4 + 6 / kappa))-1)
             rES_t = mean(simLoss_t[simLoss_t > rVaR_t])
             Model<-c(Model, distr)
             rVaR<-c(rVaR, rVaR_t)
             rES<-c(rES, rES_t)
           },
           "Double Exponential"={
             # DExp (MLE)
             dexp_param_est=list()
             dexp_param_est$location=median(lrets)
             dexp_param_est$scale=sum(abs(lrets-dexp_param_est$location))/length(lrets)
             rVaR_dexp=-(exp(qdexp(q, location=dexp_param_est$location, scale=dexp_param_est$scale))-1)
             simLoss_dexp = -(exp(rdexp(N, location = dexp_param_est$location, scale=dexp_param_est$scale))-1)
             rES_dexp = mean(simLoss_dexp[simLoss_dexp > rVaR_dexp])
             Model<-c(Model, distr)
             rVaR<-c(rVaR, rVaR_dexp)
             rES<-c(rES, rES_dexp)
           },
           "Generalized Error Distribution"={
             # GED
             ged_param_est=as.list(gedFit(lrets)$par)
             nu_grid = seq(0.5, 2, 0.001)
             kappas = (gamma(5 / nu_grid) * gamma(1 / nu_grid) / gamma(3 / nu_grid^2))- 3
             nu_hat = nu_grid[which.min(kappas > kappa)]
             rVaR_ged = -(exp(qged(q, mean = mu_hat, sd = sigma_hat, nu = nu_hat))-1)
             simLoss_ged = -(exp(rged(N, mean = mu_hat, sd = sigma_hat, nu = nu_hat))-1)
             rES_ged = mean(simLoss_ged[simLoss_ged > rVaR_ged])
             Model<-c(Model, distr)
             rVaR<-c(rVaR, rVaR_ged)
             rES<-c(rES, rES_ged)
           }
    )
  }
  uniTailRiskD=data.frame(Model, rVaR, rES)
  uniTailRiskD
}

################getECDFPlots######################
getECDFPlots<-function(neg_lrets){
  eecdf = ecdf(neg_lrets)
  
  #ECDF Plot (Zoomed-out)
  uv= seq(from =-0.15,to = 0.15, by = .001)
  ecdf_df=data.frame(neg_lrets=uv, ecdf=eecdf(uv))
  ecdfPlot1=ggplot(ecdf_df, aes(x=neg_lrets, y=ecdf))+ geom_point()+ labs(x='Negative Log-returns', y="ECDF", title="Zoomed-out")
  
  #ECDF Plot (Zoomed-in)
  uv_sub=tail(uv,round(length(uv)*0.5)) # lst 50% of possible negative log returns
  ecdf_sub_df=data.frame(neg_lrets=uv_sub, ecdf=eecdf(uv_sub))  
  ecdfPlot2=ggplot(ecdf_sub_df, aes(x=neg_lrets, y=ecdf))+ geom_point()+ labs(x='Negative Log-returns', y="ECDF", title="Zoomed-in")
  
  return(list(ecdfPlot1, ecdfPlot2))
}

################getParetoTailRiskTable######################
getParetoTailRiskTable<-function(neg_lrets, q, paretoThreshold){
  eecdf = ecdf(neg_lrets)
  N = 1e7
  
  # estimating gpd parameters
  gpd_est = gpd(neg_lrets, threshold = paretoThreshold)
  q2=1-q/(1-eecdf(paretoThreshold))
  xi=gpd_est$par.est[[1]]
  sc=gpd_est$par.est[[2]]
  rVaR_gpd=1-exp(-qgpd(q2, xi, paretoThreshold, sc))
  simLoss_gpd =  1 - exp(-rgpd(N, xi, paretoThreshold, sc))
  rES_gpd = mean(simLoss_gpd[simLoss_gpd > rVaR_gpd])
  paretoTailRiskD=data.frame(Model="Generalized Pareto", rVaR=rVaR_gpd, rES=rES_gpd)
  paretoTailRiskD
}

################getNormEstCondVolResidPlots######################
getNormEstCondVolResidPlots<-function(ticker, prices, lretsD, lretsGARCHFit){
  estPlotsList=list()
  ## Plot estimated conditional volatilities
  sigma_t = lretsGARCHFit@sigma.t
  estPlotsList[["volatilities"]]=ggplot2::qplot(index(prices)[-1], lretsD$lrets, ylab="Log-returns", xlab="Date", main=paste(ticker, "'s Estimated Volatilities with Normal Innovations"), geom = "line")+geom_line(data = as.data.frame(sigma_t), aes(y=sigma_t), color = "red")+geom_line(data = as.data.frame(-sigma_t), aes(y=-sigma_t), color = "red") 
  
  ## Plot estimated innovations
  estPlotsList[["residuals"]]=ggplot2::qplot(index(prices)[-1], lretsGARCHFit@residuals / sigma_t, ylab="Residuals", xlab="Date", main=paste(ticker, "'s  Estimated Residuals with Normal Assumption"), geom = "line")

  estPlotsList
}

################getStdEstCondVolResidPlots######################
getStdEstCondVolResidPlots<-function(ticker, prices, lretsD, lretsGARCHFit){
  estPlotsList=list()
  ## Plot estimated conditional volatilities
  sigma_t = lretsGARCHFit@sigma.t
  estPlotsList[["volatilities"]]=ggplot2::qplot(index(prices)[-1], lretsD$lrets, ylab="Log-returns", xlab="Date", main=paste(ticker, "'s Estimated Volatilities with Student t Innovations"), geom = "line")+geom_line(data = as.data.frame(sigma_t), aes(y=sigma_t), color = "red")+geom_line(data = as.data.frame(-sigma_t), aes(y=-sigma_t), color = "red") 
  
  ## Plot estimated innovations
  estPlotsList[["residuals"]]=ggplot2::qplot(index(prices)[-1], lretsGARCHFit@residuals / sigma_t, ylab="Residuals", xlab="Date", main=paste(ticker, "'s  Estimated Residuals with Student t Assumption"), geom = "line")
  
  estPlotsList
}

################getGedEstCondVolResidPlots######################
getGedEstCondVolResidPlots<-function(ticker, prices, lretsD, lretsGARCHFit){
  estPlotsList=list()
  ## Plot estimated conditional volatilities
  sigma_t = lretsGARCHFit@sigma.t
  estPlotsList[["volatilities"]]=ggplot2::qplot(index(prices)[-1], lretsD$lrets, ylab="Log-returns", xlab="Date", main=paste(ticker, "'s Estimated Volatilities with GED Innovations"), geom = "line")+geom_line(data = as.data.frame(sigma_t), aes(y=sigma_t), color = "red")+geom_line(data = as.data.frame(-sigma_t), aes(y=-sigma_t), color = "red") 
  
  ## Plot estimated innovations
  estPlotsList[["residuals"]]=ggplot2::qplot(index(prices)[-1], lretsGARCHFit@residuals / sigma_t, ylab="Residuals", xlab="Date", main=paste(ticker, "'s  Estimated Residuals with GED Assumption"), geom = "line")
  
  estPlotsList
}

################getGedGARCHTailRiskTable######################
getGedGARCHTailRiskTable<-function(q, garchModelType){
  N=1e7
  # containers
  Innovations<-c()
  rVaR<-c()
  rES<-c()
  for (mod in names(garchModelType)){
    switch(mod, 
           "norm"={
             # Normal
             norm_est= predict(garchModelType[[mod]], 1)
             mu_est_norm = norm_est[['meanForecast']]
             sigma_est_norm = norm_est[['standardDeviation']]
             rVaR_norm= - (exp(mu_est_norm + sigma_est_norm * qnorm(q)) - 1)
             simLoss_norm = -(exp(rnorm(N, mean=mu_est_norm, sd=sigma_est_norm))-1)
             rES_norm = mean(simLoss_norm[simLoss_norm > rVaR_norm])
             Innovations<-c(Innovations, "Normal")
             rVaR<-c(rVaR, rVaR_norm)
             rES<-c(rES, rES_norm)
           },
           "std"={
             # Student t
             std_est = predict(garchModelType[[mod]], 1)
             mu_est_std = std_est[['meanForecast']]
             sigma_est_std = std_est[['standardDeviation']]
             nu_est_std=coef(garchModelType[[mod]])[['shape']]
             rVaR_std= - (exp(mu_est_std + sigma_est_std * sqrt((nu_est_std-2)/nu_est_std) * qt(q, df = nu_est_std)) - 1)
             
             simLoss_std = -(exp(rstd(N, mean=mu_est_std, sd=sigma_est_std, nu= nu_est_std))-1)
             rES_std = mean(simLoss_std[simLoss_std > rVaR_std])
             Innovations<-c(Innovations, "Student t")
             rVaR<-c(rVaR, rVaR_std)
             rES<-c(rES, rES_std)
           },
           "ged"={
             # GED
             ged_est = predict(garchModelType[[mod]], 1)
             mu_est_ged = ged_est[['meanForecast']]
             sigma_est_ged = ged_est[['standardDeviation']]
             nu_est_ged=coef(garchModelType[[mod]])[['shape']]
             rVaR_ged = -(exp(qged(q, mean = mu_est_ged, sd = sigma_est_ged, nu = nu_est_ged))-1)
             simLoss_ged = -(exp(rged(N, mean = mu_est_ged, sd = sigma_est_ged, nu = nu_est_ged))-1)
             rES_ged = mean(simLoss_ged[simLoss_ged > rVaR_ged])
             Innovations<-c(Innovations, "GED")
             rVaR<-c(rVaR, rVaR_ged)
             rES<-c(rES, rES_ged)
           }
    )
  }
  garchTailRiskD=data.frame(Innovations, rVaR, rES)
  garchTailRiskD
  
}
################getNormResidHistQQPlot######################
getNormResidHistQQPlot<-function(ticker, lretsGARCHFit){
  normResidDistrPlotsList=list()
  sigma_t = lretsGARCHFit@sigma.t
  # standardized residuals in a data frame
  estResidsD=data.frame(estResids=lretsGARCHFit@residuals / sigma_t)
  ## Normal
  normResidDistrPlotsList[["hist"]]<-ggplot(estResidsD, aes(x=estResids))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Residuals', title=paste(ticker, "'s Residuals QQ Plot with Normal Fit"))+ stat_function(fun = dnorm)
  normResidDistrPlotsList[["qq"]]<-ggplot(estResidsD, aes(sample=estResids))+ stat_qq(distribution = qnorm, color="darkred") + stat_qq_line(distribution = qnorm, color="steelblue")+ggtitle(paste(ticker, "'s Residuals QQ Plot with Normal Fit"))
  normResidDistrPlotsList
}

################getStdResidHistQQPlot######################
getStdResidHistQQPlot<-function(ticker, lretsGARCHFit){
  stdResidDistrPlotsList=list()
  sigma_t = lretsGARCHFit@sigma.t
  # standardized residuals in a data frame
  estResidsD=data.frame(estResids=lretsGARCHFit@residuals / sigma_t)
  ## Student t
  nu=coef(lretsGARCHFit)[['shape']]
  stdResidDistrPlotsList[["hist"]]<-ggplot(estResidsD, aes(x=estResids))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Residuals', title=paste(ticker, "'s Residuals Histogram with Student t Fit"))+ stat_function(fun = dstd, args = list(nu=nu))
  stdResidDistrPlotsList[["qq"]]<-ggplot(estResidsD, aes(sample=estResids))+ stat_qq(distribution = qstd, dparams=list(nu=nu), color="darkred") + stat_qq_line(distribution = qstd, dparams=list(nu=nu), color="steelblue")+ggtitle(paste(ticker, "'s Residuals Histogram with Student t Fit"))
  stdResidDistrPlotsList
} 
################getGedResidHistQQPlot######################
getGedResidHistQQPlot<-function(ticker, lretsGARCHFit){
  gedResidDistrPlotsList=list()
  sigma_t = lretsGARCHFit@sigma.t
  # standardized residuals in a data frame
  estResidsD=data.frame(estResids=lretsGARCHFit@residuals / sigma_t)
  ## GED
  kappa = kurtosis(estResidsD$estResids, method = 'excess')[1]
  nu_grid = seq(0.5, 2, 0.001)
  kappas = (gamma(5 / nu_grid) * gamma(1 / nu_grid) / gamma(3 / nu_grid^2))- 3
  nu= nu_grid[which.min(kappas > kappa)]
  gedResidDistrPlotsList[["hist"]]<-ggplot(estResidsD, aes(x=estResids))+ geom_histogram(aes(y=..density..),alpha=0.5, bins = 50)+ labs(x='Residuals', title=paste(ticker, "'s Residuals QQ Plot with GED Fit"))+ stat_function(fun = dged, args = list(nu=nu))
  gedResidDistrPlotsList[["qq"]]<-ggplot(estResidsD, aes(sample=estResids))+ stat_qq(distribution = qged, dparams=list(nu=nu), color="darkred") + stat_qq_line(distribution = qged, dparams=list(nu=nu), color="steelblue")+ggtitle(paste(ticker, "'s Residuals QQ Plot with GED Fit"))
  gedResidDistrPlotsList
} 
  

##########################getReturns#################################
getReturns<-function(tickers, start_date, end_date){  
  
  # Inherit price data into each symbol
  getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)
  # Use Adjusted prices to form columns of prices
  prices<-reduce(map(tickers, function(x) Ad(get(x))), merge) 
  colnames(prices)<-gsub("\\..*","",colnames(prices)) # represent column names with tickers
  # Compute log-return for each stock
  log_rets<-apply(prices, 2, function(x) diff(log(x)))
  # Compute return for each stock
  rets<-exp(log_rets)-1; rets<-rets[complete.cases(rets),] # removed missing observations
  
  return(rets)
}

##########################getQQPlots#################################
getQQPlots<-function(rets, qqMethod){  

  switch(qqMethod,
         "Normal"={
          ## For Normal
          # dimension of assets
          p<-dim(rets)[2]
          # Construct qq plots for each asset
          plot_list = list()
          for (c in 1:p) {
            # Create long form version of the return matrix
            rets_df <- pivot_longer(as.data.frame(rets)[c], names_to = "ticker", values_to = "data", cols = everything())
            plot_list[[c]] = ggplot(rets_df, aes(sample=data)) +
                             stat_qq(distribution = qnorm, color="darkred") + 
                             stat_qq_line(line.p = c(0.25, 0.75), color="steelblue")+ 
                             ggtitle(colnames(rets)[c]) +
                             labs(x="Theoretical Normal Quantiles", y="Empirical Quantiles")+ 
                             theme(text = element_text(size = 15),
                             plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)))
          }
          grid.arrange(grobs = plot_list,
                       ncol = 4,
                       top = "Normal QQ-Plot of Assets Returns",
                       gp = gpar(fontsize = 15, font = 8, face="bold"))
         },
        
    "Student t"={
    ## For t,
    # degrees of freedom grid
    dfree=seq(2.01,6,0.01)
    n=length(dfree)
    loglikt<-rep(0,n)
    for (i in 1:n){
      fit<-cov.trob(rets, nu=dfree[i])
      mu<-as.vector(fit$center)
      loglikt[i]<-sum(log(dmt(rets,mean=fit$center, S=fit$cov, df=dfree[i])))
    }
    # find df that yields the the maximum profile likelihood and the maximum profile likelihood
    nuest<-dfree[which.max(loglikt)]
    
    qqt_scale = function(rets, nuest){
      xsort = sort(rets)
      N<-length(xsort)
      quantv<-(1/(N+1))*seq(1,N,1)
      qq = qt(quantv, nuest)
      
      # Construct dataframe to plot in ggplot
      df = data.frame("quantiles" = qq, "data" = xsort)
      
      # Compute theoretical quantiles
      xx = quantile(qq, c(0.25, 0.75))
      yy = quantile(xsort, c(0.25, 0.75))
      slope = (yy[2] - yy[1]) / (xx[2] - xx[1])
      inter = yy[1] - slope*xx[1]
      
      ggplot(df, aes(x = quantiles, y = data)) +
        geom_point(colour = "darkred", shape = 1)  + 
        geom_abline(intercept = inter, slope = slope, colour = "steelblue") +
        labs(x="Theoretical t Quantiles", y="Empirical Quantiles") 
    }
    # Construct qq plots for each asset
    plot_list = list()
    for (c in 1:dim(rets)[2]) {
      plot_list[[c]] = qqt_scale(rets[,c], nuest)+ 
                       ggtitle(colnames(rets)[c]) + 
                       theme(text = element_text(size = 15),
                            plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)))
    }
    grid.arrange(grobs = plot_list,
                 ncol = 4,
                 top = "Student-t QQ-Plot of Assets Returns",
                 gp = gpar(fontsize = 15, font = 8))
    
    }
  )
}

################getMultiTailRiskTable#######################
getMultiTailRiskTable<-function(rets, allocation, nlv, q, modelType){
  # number of assets
  p<-dim(rets)[2]
  # record sample mean and sample covariance
  sampmean<-colMeans(rets)
  sampcov<-cov(rets)
  
  # Expected return and std. dev. of current portfolio
  portfolio_mean<-t(allocation)%*%sampmean
  portfolio_var<-t(allocation)%*%sampcov%*%allocation
  
  # containers
  Model<-c()
  rVaR<-c()
  VaR<-c()
  rES<-c()
  ES<-c()
  
  for (mod in modelType){
    switch(mod, 
      #--------Bivariate Normal ECDFs------------------
      "Normal"={
        #--------VaR and ES for Multivariate Normal-------
        ## rVaR and VaR for mnorm
        rVaR_mnorm<--qnorm(q, mean=portfolio_mean, sd=sqrt(portfolio_var))
        VaR_mnorm<-nlv*rVaR_mnorm
        
        ## ES and ES for mnorm
        wret<-rets%*%allocation
        rES_mnorm<-mean(-wret[-wret>rVaR_mnorm])
        ES_mnorm<-nlv*rES_mnorm
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_mnorm)
        VaR<-c(VaR, VaR_mnorm)
        rES<-c(rES, rES_mnorm)
        ES<-c(ES, ES_mnorm)
      },
      "Student t"={
        #-----------VaR and ES for Multivariate t---------
        # degrees of freedom grid
        dfree=seq(2.01,6,0.01)
        n=length(dfree)
        loglikt<-rep(0,n)
        for (i in 1:n){
          fit<-cov.trob(rets, nu=dfree[i])
          mu<-as.vector(fit$center)
          loglikt[i]<-sum(log(dmt(rets,mean=fit$center, S=fit$cov, df=dfree[i])))
        }
        # find df that yields the the maximum profile likelihood and the maximum profile likelihood
        nuest<-dfree[which.max(loglikt)]
        # loglikt_max<-loglikt[which.max(loglikt)]
        
        # estimated mu and Lambda(scale)
        fitfinal<-cov.trob(rets, nu=nuest)
        muest<-fitfinal$center
        Lambdaest<-fitfinal$cov
        ## rVaR and VaR for mt
        rVaR_mt<- -(t(allocation)%*%muest+sqrt(t(allocation)%*%Lambdaest%*%allocation)*qt(q,df=nuest))[1]
        VaR_mt<-nlv*rVaR_mt
        ## rES and ES for mt
        rES_mt<-mean(-wret[-wret>rVaR_mt])
        ES_mt<-nlv*rES_mt
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_mt)
        VaR<-c(VaR, VaR_mt)
        rES<-c(rES, rES_mt)
        ES<-c(ES, ES_mt)
      }
    )
  }
  multiTailRiskD=data.frame(Model, rVaR, VaR, rES, ES)
  multiTailRiskD
}

###################getMultiTailBiPlots####################

getMultiTailBiPlots<-function(rets, allocation, nlv, q, modelType){
  # number of assets
  p<-dim(rets)[2]
  biPlotList<-list()
  counter=0
  # record sample mean and sample covariance
  sampmean<-colMeans(rets)
  sampcov<-cov(rets)
  
  
  for(mod in modelType){
    switch(mod, 
      #--------Bivariate Normal ECDFs------------------
      "Normal"={
        # bivariate ECDF to sompare observed and theoretical tails
        rmnorm_dist<-rmnorm(1e6, mean=sampmean, sampcov)
        for(i in 1:(p-1)){
          for(j in (i+1):p){
            # combine asset probabilities
            data1<-cbind(pnorm(rets[,i], sampmean[i], sqrt(sampcov[i,i])),
                         pnorm(rets[,j], sampmean[j], sqrt(sampcov[j,j])))
            # probability between observed asset pairs
            dem<-pempiricalCopula(data1[,1], data1[,2], N=20)
            # creating dem data
            demx=c(); demy=c(); demz=c()
            for(n in 1:length(dem$x)){
              for (m in 1:length(dem$y)){
                demx=c(demx,dem$x[n]);
                demy=c(demy,dem$y[m]);
                demz=c(demz,dem$z[n,m])
              }
            }
            demdf=data.frame(X=demx,Y=demy,Z=demz)
            demdf$type="dem"
            
            # creating theoretical normal probabilities
            data2<-cbind(pnorm(rmnorm_dist[,i], sampmean[i], sqrt(sampcov[i,i])),
                         pnorm(rmnorm_dist[,j], sampmean[j], sqrt(sampcov[j,j])))
            # probability between theoretical asset pairs
            demt<-pempiricalCopula(data2[,1], data2[,2])
            # creating demt data
            demtx=c(); demty=c(); demtz=c()
            for(n in 1:length(demt$x)){
              for (m in 1:length(demt$y)){
                demtx=c(demtx,demt$x[n]);
                demty=c(demty,demt$y[m]);
                demtz=c(demtz,demt$z[n,m])
              }
            }
            demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
            demtdf$type="demt"
            # combining dem and demt data
            DD = rbind(demdf, demtdf)
            counter=counter+1
            # creating bivariate plots
            biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
              stat_contour()+
              scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
              theme(panel.background=element_rect(fill="grey90")) +
              theme(panel.grid=element_blank()) +
              labs(title="Multivariate Normal", x=colnames(rets)[i], y=colnames(rets)[j])+ 
              geom_text_contour(aes(z = Z))+ 
              theme(text = element_text(size = 15),
                    plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)), 
                    axis.text.x = element_text(angle = 45),
                    legend.title = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
          }
        }
      },
      #--------Bivariate Student t ECDFs------------------
      "Student t"={
        # degrees of freedom grid
        dfree=seq(2.01,6,0.01)
        n=length(dfree)
        loglikt<-rep(0,n)
        for (i in 1:n){
          fit<-cov.trob(rets, nu=dfree[i])
          mu<-as.vector(fit$center)
          loglikt[i]<-sum(log(dmt(rets,mean=fit$center, S=fit$cov, df=dfree[i])))
        }
        # find df that yields the the maximum profile likelihood and the maximum profile likelihood
        nuest<-dfree[which.max(loglikt)]
        # loglikt_max<-loglikt[which.max(loglikt)]
        
        # estimated mu and Lambda(scale)
        fitfinal<-cov.trob(rets, nu=nuest)
        muest<-fitfinal$center
        Lambdaest<-fitfinal$cov
        # finding bivariate csf
        rmt_dist<-rmt(n=1e6, mean=muest, Lambdaest, nuest)
        for(i in 1:(p-1)){
          for(j in (i+1):p){
            # combine asset probabilities
            data1<-cbind(pstd(rets[,i], muest[i], sqrt(Lambdaest[i,i]), nuest),
                         pstd(rets[,j], sampmean[j], sqrt(sampcov[j,j]), nuest))
            # probability between observed asset pairs
            dem<-pempiricalCopula(data1[,1], data1[,2], N=20)
            # creating dem data
            demx=c(); demy=c(); demz=c()
            for(n in 1:length(dem$x)){
              for (m in 1:length(dem$y)){
                demx=c(demx,dem$x[n]);
                demy=c(demy,dem$y[m]);
                demz=c(demz,dem$z[n,m])
              }
            }
            demdf=data.frame(X=demx,Y=demy,Z=demz)
            demdf$type="dem"
            
            # creating theoretical normal probabilities
            data2<-cbind(pstd(rmt_dist[,i], muest[i], sqrt(Lambdaest[i,i]*nuest/(nuest-2)), nuest),
                         pstd(rmt_dist[,j], muest[j], sqrt(Lambdaest[j,j]*nuest/(nuest-2)), nuest))
            # probability between theoretical asset pairs
            demt<-pempiricalCopula(data2[,1], data2[,2])
            # creating demt data
            demtx=c(); demty=c(); demtz=c()
            for(n in 1:length(demt$x)){
              for (m in 1:length(demt$y)){
                demtx=c(demtx,demt$x[n]);
                demty=c(demty,demt$y[m]);
                demtz=c(demtz,demt$z[n,m])
              }
            }
            demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
            demtdf$type="demt"
            # combining dem and demt data
            DD = rbind(demdf, demtdf)
            counter=counter+1
            # creating bivariate plots
            biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
              stat_contour() +
              theme(panel.background=element_rect(fill="grey90")) +
              theme(panel.grid=element_blank()) +
              labs(title="Multivariate Student t", x=colnames(rets)[i], y=colnames(rets)[j])+
              geom_text_contour(aes(z = Z))+
              scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
              theme(text = element_text(size = 15),
                    plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                    axis.text.x = element_text(angle = 45),
                    legend.title = element_text(size=15),
                    legend.text = element_text(size=15),
                    legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
          }
        }
      }
    )
  }
  
  grid.arrange(grobs = biPlotList,
               ncol = 6,
               gp = gpar(fontsize = 10, font = 8, face="bold"))
}
  
################getCopTailRiskTable######################
getCopTailRiskTable<-function(rets, allocation, corrMethod, nlv, q, modelType){
  # number of assets
  p<-dim(rets)[2]
  N<-dim(rets)[1]
  #================== Copula rVaR, Var, rES, ES ================
  # simulate returns from t copula model
  n = 1e6
  # Gather coefficients between unique pairs
  omega = P2p(cor(rets, method=corrMethod))
  # Estimate the distribution of each assets returns with a t-distribution
  # to estimate 3 parameters (mu, scale matrix, df)
  est = matrix(rep(0, p * 3), p, 3)
  for (i in 1:p){
    est[i,] = tryCatch({fitdistr(rets[,i][rets[,i] !=0], 't')$estimate},
                       error=function(e){fitdistr(rets[,i][rets[,i] !=0], 't')$estimate})
  }
  # simulated probabilities assuming t distribution
  cdf_tran=rets
  
  for (i in 1:p){
    # ensure nu>2 to avoid undefined sd 
    if (est[i,3]<=2){
      est[i,3]=2.01
    }
    cdf_tran[,i]=pstd(rets[,i], mean=est[i,1], sd=est[i,2]*sqrt(est[i,3]/(est[i,3]-2)), nu=est[i,3])
  }
  
  # marginal log-likilihood
  marginal_ll=0
  for (i in 1:p){
    marginal_ll = sum(marginal_ll, log(dstd(rets[,i],mean=est[i,1],sd=est[i,2] * sqrt(est[i,3]/(est[i,3]-2)),nu = est[i,3])))
  }
  # containers
  Model<-c()
  rVaR<-c()
  VaR<-c()
  rES<-c()
  ES<-c()
  AIC<-c()
  BIC<-c()
  
  for (mod in modelType){
    switch(mod, 
      "Student t"={
        #--------------------Student t Copula---------------
        # degrees of freedom must be >2
        tcop<-tCopula(omega, dim=p, dispstr = "un", df=ifelse(length(omega)<=2, 2.01,length(omega)), df.min=2.01, df.fixed = FALSE)
        ft = fitCopula(data=as.matrix(cdf_tran), copula=tcop,method="ml", optim.method="BFGS",start=c(omega,length(omega)+1))
        Ut = rCopula(tCopula(param = ft@estimate[1:length(omega)], dim = p, df = ft@estimate[length(omega)+1], dispstr = 'un'), n = n)
        simRet_t = c()
        for (j in 1:p){
          simRet_t = cbind(simRet_t, qstd(p=Ut[,j], mean = est[j,1], sd = est[j,2]*sqrt(est[j,3]/(est[j,3]-2)), nu = est[j,3]))
        }
        
        ## rVaR and VaR of t copula
        wret_tcop<-simRet_t%*%allocation
        rVaR_tcop<--quantile(wret_tcop,q)
        VaR_tcop<-nlv*rVaR_tcop
        ## rES and ES of t copula
        rES_tcop<-mean(-wret_tcop[-wret_tcop>rVaR_tcop])
        ES_tcop<-nlv*rES_tcop
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_tcop)
        VaR<-c(VaR, VaR_tcop)
        rES<-c(rES, rES_tcop)
        ES<-c(ES, ES_tcop)
        AIC<-c(AIC, -2*(ft@loglik+marginal_ll)+2*(length(omega)+1))
        BIC<-c(BIC, -2*(ft@loglik+marginal_ll)+log(N)*(length(omega)+1))# here, n must be much larger than k

      },
      
      "Normal"={
        #-------------------------Gaussian Copula---------------
        normcop<-normalCopula(omega, dim=p, dispstr = "un")
        fnorm = fitCopula(data=as.matrix(cdf_tran), copula=normcop,method="ml", optim.method="BFGS",start=omega)
        # simulate returns from Gaussian copula model
        Unorm = rCopula(normalCopula(param = fnorm@estimate, dim = p, dispstr = 'un'), n = n)
        simRet_norm = c()
        for (j in 1:p){
          simRet_norm = cbind(simRet_norm, qstd(p=Unorm[,j], mean = est[j,1], sd = est[j,2]*sqrt(est[j,3]/(est[j,3]-2)), nu = est[j,3]))
        }
        
        ## rVaR and VaR of normal copula
        wret_normcop<-simRet_norm%*%allocation
        rVaR_normcop<--quantile(wret_normcop,q)
        VaR_normcop<-nlv*rVaR_normcop
        ## rES and ES of normal copula
        rES_normcop<-mean(-wret_normcop[-wret_normcop>rVaR_normcop])
        ES_normcop<-nlv*rES_normcop
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_normcop)
        VaR<-c(VaR, VaR_normcop)
        rES<-c(rES, rES_normcop)
        ES<-c(ES, ES_normcop)
        AIC<-c(AIC, -2*(fnorm@loglik+marginal_ll)+2*length(omega))
        BIC<-c(BIC, -2*(fnorm@loglik+marginal_ll)+log(N)*length(omega))# here, n must be much larger than k
      },
      "Gumbel"={
        #----------------------Gumbel Copula-----------------
        fgumbel = fitCopula(data=as.matrix(cdf_tran), copula=gumbelCopula(3, dim=p),method="ml",start=2)
        # simulate returns from Gumbel copula model
        Ugumbel = rCopula(gumbelCopula(param = fgumbel@estimate, dim = p), n = n)
        simRet_gumbel = c()
        for (j in 1:p){
          simRet_gumbel = cbind(simRet_gumbel, qstd(p=Ugumbel[,j], mean = est[j,1], sd = est[j,2]*sqrt(est[j,3]/(est[j,3]-2)), nu = est[j,3]))
        }
        
        ## rVaR and VaR of gumbel copula
        wret_gumbelcop<-simRet_gumbel%*%allocation
        rVaR_gumbelcop<--quantile(wret_gumbelcop,q)
        VaR_gumbelcop<-nlv*rVaR_gumbelcop
        ## rES and ES of gumbel copula
        rES_gumbelcop<-mean(-wret_gumbelcop[-wret_gumbelcop>rVaR_gumbelcop])
        ES_gumbelcop<-nlv*rES_gumbelcop
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_gumbelcop)
        VaR<-c(VaR, VaR_gumbelcop)
        rES<-c(rES, rES_gumbelcop)
        ES<-c(ES, ES_gumbelcop)
        AIC<-c(AIC, -2*(fgumbel@loglik+marginal_ll)+2*1)
        BIC<-c(BIC, -2*(fgumbel@loglik+marginal_ll)+log(N)*1)# here, n must be much larger than k
      },
      "Joe"={
      
        #------------------------Joe Copula----------------------------
        fjoe = fitCopula(data=as.matrix(cdf_tran), copula=joeCopula(3, dim=p),method="ml",start=2)
        # simulate returns from Joe copula model
        Ujoe = rCopula(joeCopula(param = fjoe@estimate, dim = p), n = n)
        simRet_joe = c()
        for (j in 1:p){
          simRet_joe = cbind(simRet_joe, qstd(p=Ujoe[,j], mean = est[j,1], sd = est[j,2]*sqrt(est[j,3]/(est[j,3]-2)), nu = est[j,3]))
        }
        
        ## rVaR and VaR of joe copula
        wret_joecop<-simRet_joe%*%allocation
        rVaR_joecop<--quantile(wret_joecop,q)
        VaR_joecop<-nlv*rVaR_joecop
        ## rES and ES of joe copula
        rES_joecop<-mean(-wret_joecop[-wret_joecop>rVaR_joecop])
        ES_joecop<-nlv*rES_joecop
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_joecop)
        VaR<-c(VaR, VaR_joecop)
        rES<-c(rES, rES_joecop)
        ES<-c(ES, ES_joecop)
        AIC<-c(AIC, -2*(fjoe@loglik+marginal_ll)+2*1)
        BIC<-c(BIC, -2*(fjoe@loglik+marginal_ll)+log(N)*1)# here, n must be much larger than k
      },
      "Clayton"={
      
        #---------------------Clayton Copula------------------
        fclayton = fitCopula(data=as.matrix(cdf_tran), copula=claytonCopula(3, dim=p),method="ml", optim.method = "BFGS",start=2)
      
        # simulate returns from Clayton copula model
        Uclayton = rCopula(claytonCopula(param = fclayton@estimate, dim = p), n = n)
        simRet_clayton = c()
        for (j in 1:p){
          simRet_clayton = cbind(simRet_clayton, qstd(p=Uclayton[,j], mean = est[j,1], sd = est[j,2]*sqrt(est[j,3]/(est[j,3]-2)), nu = est[j,3]))
        }
      
        ## rVaR and VaR of clayton copula
        wret_claytoncop<-simRet_clayton%*%allocation
        rVaR_claytoncop<--quantile(wret_claytoncop,q)
        VaR_claytoncop<-nlv*rVaR_claytoncop
        ## rES and ES of clayton copula
        rES_claytoncop<-mean(-wret_claytoncop[-wret_claytoncop>rVaR_claytoncop])
        ES_claytoncop<-nlv*rES_claytoncop
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_claytoncop)
        VaR<-c(VaR, VaR_claytoncop)
        rES<-c(rES, rES_claytoncop)
        ES<-c(ES, ES_claytoncop)
        AIC<-c(AIC, -2*(fclayton@loglik+marginal_ll)+2*1)
        BIC<-c(BIC, -2*(fclayton@loglik+marginal_ll)+log(N)*1)# here, n must be much larger than k
      },
    
      "Frank"={
        #------------------------Frank Copula--------------------
        ffrank = fitCopula(data=as.matrix(cdf_tran), copula=frankCopula(3, dim=p), method="ml",start=2)
        # simulate returns from Frank copula model
        Ufrank = rCopula(frankCopula(param = ffrank@estimate, dim = p), n = n)
        simRet_frank = c()
        for (j in 1:p){
          simRet_frank = cbind(simRet_frank, qstd(p=Ufrank[,j], mean = est[j,1], sd = est[j,2]*sqrt(est[j,3]/(est[j,3]-2)), nu = est[j,3]))
        }
        
        ## rVaR and VaR of frank copula
        wret_frankcop<-simRet_frank%*%allocation
        rVaR_frankcop<--quantile(wret_frankcop,q)
        VaR_frankcop<-nlv*rVaR_frankcop
        ## rES and ES of frank copula
        rES_frankcop<-mean(-wret_frankcop[-wret_frankcop>rVaR_frankcop])
        ES_frankcop<-nlv*rES_frankcop
        #populating containers
        Model<-c(Model,mod)
        rVaR<-c(rVaR, rVaR_frankcop)
        VaR<-c(VaR, VaR_frankcop)
        rES<-c(rES, rES_frankcop)
        ES<-c(ES, ES_frankcop)
        AIC<-c(AIC, -2*(ffrank@loglik+marginal_ll)+2*1)
        BIC<-c(BIC, -2*(ffrank@loglik+marginal_ll)+log(N)*1)# here, n must be much larger than k
      }
    )
  }
  copTailRiskD=data.frame(Model, rVaR, VaR, rES, ES, AIC, BIC)
  copTailRiskD[order(copTailRiskD$AIC, copTailRiskD$BIC),]
}
  

###################getCopulaTailBiPlots####################
getCopTailBiPlots<-function(rets, allocation, corrMethod, nlv, q, modelType){
  
  # number of assets
  p<-dim(rets)[2]
  # Gather coefficients between unique pairs
  omega = P2p(cor(rets, method=corrMethod))
  biPlotList<-list()
  counter=0

  # Estimate the distribution of each assets returns with a t-distribution
  # to estimate 3 parameters (mu, scale matrix, df)
  est = matrix(rep(0, p * 3), p, 3)
  for (i in 1:p){
    est[i,] = tryCatch({fitdistr(rets[,i][rets[,i] !=0], 't')$estimate},
                       error=function(e){fitdistr(rets[,i][rets[,i] !=0], 't')$estimate})
  }
  # simulated probabilities assuming t distribution
  cdf_tran=rets
  
  for (i in 1:p){
    # ensure nu>2 to avoid undefined sd 
    if (est[i,3]<=2){
      est[i,3]=2.01
    }
    cdf_tran[,i]=pstd(rets[,i], mean=est[i,1], sd=est[i,2]*sqrt(est[i,3]/(est[i,3]-2)), nu=est[i,3])
  }
  
  for (mod in modelType){
    switch(mod,
           "Student t"={
             
             #---------------Multivariate t Copula Model---------
             # degrees of freedom must be >2
             tcop<-tCopula(omega, dim=p, dispstr = "un", df=ifelse(length(omega)<=2, 2.01,length(omega)), df.min=2.01, df.fixed = FALSE)
             ft = fitCopula(data=as.matrix(cdf_tran), copula=tcop,method="ml",optim.method="BFGS",start=c(omega,length(omega)+1))
           
             for(i in 1:(p-1)){
               for(j in (i+1):p){
                 # probability between observed asset pairs
                 dem = pempiricalCopula(cdf_tran[,i], cdf_tran[,j])
                 # creating dem data
                 demx=c(); demy=c(); demz=c()
                 for(n in 1:length(dem$x)){
                   for (m in 1:length(dem$y)){
                     demx=c(demx,dem$x[n]);
                     demy=c(demy,dem$y[m]);
                     demz=c(demz,dem$z[n,m])
                   }
                 }
                 demdf=data.frame(X=demx,Y=demy,Z=demz)
                 demdf$type="dem"
                 
                 # fitting copula
                 ct = tCopula (p2P(coef(ft))[i,j], dim = 2, dispstr = "un", tail(coef(ft),n=1))
                 utdis = rCopula(1e6,ct)
                 demt = pempiricalCopula(utdis[,1],utdis[,2])
                 # creating demt data
                 demtx=c(); demty=c(); demtz=c()
                 for(n in 1:length(demt$x)){
                   for (m in 1:length(demt$y)){
                     demtx=c(demtx,demt$x[n]);
                     demty=c(demty,demt$y[m]);
                     demtz=c(demtz,demt$z[n,m])
                   }
                 }
                 demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
                 demtdf$type="demt"
                 # combining dem and demt data
                 DD = rbind(demdf, demtdf)
                 counter=counter+1
                 # creating bivariate plots
                 biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
                   stat_contour()+
                   scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
                   theme(panel.background=element_rect(fill="grey90")) +
                   theme(panel.grid=element_blank()) +
                   labs(title="Multivariate t Copula", x=colnames(rets)[i], y=colnames(rets)[j])+
                   geom_text_contour(aes(z = Z))+
                   theme(text = element_text(size = 15),
                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                         axis.text.x = element_text(angle = 45),
                         legend.title = element_text(size=15),
                         legend.text = element_text(size=15),
                         legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
               }
             }
           },
           "Normal"={
             #---------Multivariate Normal Copula Model----------
             normcop<-normalCopula(omega, dim=p, dispstr = "un")
             fnorm = fitCopula(data=as.matrix(cdf_tran), copula=normcop,method="ml", optim.method="BFGS",start=omega)
  
             for(i in 1:(p-1)){
               for(j in (i+1):p){
                 # probability between observed asset pairs
                 dem = pempiricalCopula(cdf_tran[,i], cdf_tran[,j])
                 # creating dem data
                 demx=c(); demy=c(); demz=c()
                 for(n in 1:length(dem$x)){
                   for (m in 1:length(dem$y)){
                     demx=c(demx,dem$x[n]);
                     demy=c(demy,dem$y[m]);
                     demz=c(demz,dem$z[n,m])
                   }
                 }
                 demdf=data.frame(X=demx,Y=demy,Z=demz)
                 demdf$type="dem"
                 
                 # fitting copula
                 cnorm = normalCopula (p2P(coef(fnorm))[i,j], dim = 2, dispstr = "un")
                 utdis = rCopula(1e6,cnorm)
                 demt = pempiricalCopula(utdis[,1],utdis[,2])
                 # creating demt data
                 demtx=c(); demty=c(); demtz=c()
                 for(n in 1:length(demt$x)){
                   for (m in 1:length(demt$y)){
                     demtx=c(demtx,demt$x[n]);
                     demty=c(demty,demt$y[m]);
                     demtz=c(demtz,demt$z[n,m])
                   }
                 }
                 demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
                 demtdf$type="demt"
                 # combining dem and demt data
                 DD = rbind(demdf, demtdf)
                 counter=counter+1
                 # creating bivariate plots
                 biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
                   stat_contour()+
                   scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
                   theme(panel.background=element_rect(fill="grey90")) +
                   theme(panel.grid=element_blank()) +
                   labs(title="Multivariate Normal Copula", x=colnames(rets)[i], y=colnames(rets)[j])+
                   geom_text_contour(aes(z = Z))+
                   theme(text = element_text(size = 15),
                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                         axis.text.x = element_text(angle = 45),
                         legend.title = element_text(size=15),
                         legend.text = element_text(size=15),
                         legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
               }
             }
           },
           "Gumbel"={
             #---------Multivariate Gumbel Copula Model---------
             fgumbel = fitCopula(data=as.matrix(cdf_tran), copula=gumbelCopula(3, dim=p),method="ml",start=2)
  
             for(i in 1:(p-1)){
               for(j in (i+1):p){
                 # probability between observed asset pairs
                 dem = pempiricalCopula(cdf_tran[,i], cdf_tran[,j])
                 # creating dem data
                 demx=c(); demy=c(); demz=c()
                 for(n in 1:length(dem$x)){
                   for (m in 1:length(dem$y)){
                     demx=c(demx,dem$x[n]);
                     demy=c(demy,dem$y[m]);
                     demz=c(demz,dem$z[n,m])
                   }
                 }
                 demdf=data.frame(X=demx,Y=demy,Z=demz)
                 demdf$type="dem"
                 
                 # fitting copula
                 # Gumble is an Archimedean type of copulas, and are therefore an exchangeable distribution.
                 cgumbel = gumbelCopula(coef(fgumbel), dim = 2)
                 utdis = rCopula(1e6,cgumbel)
                 demt = pempiricalCopula(utdis[,1],utdis[,2])
                 # creating demt data
                 demtx=c(); demty=c(); demtz=c()
                 for(n in 1:length(demt$x)){
                   for (m in 1:length(demt$y)){
                     demtx=c(demtx,demt$x[n]);
                     demty=c(demty,demt$y[m]);
                     demtz=c(demtz,demt$z[n,m])
                   }
                 }
                 demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
                 demtdf$type="demt"
                 # combining dem and demt data
                 DD = rbind(demdf, demtdf)
                 counter=counter+1
                 # creating bivariate plots
                 biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
                   stat_contour()+
                   scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
                   theme(panel.background=element_rect(fill="grey90")) +
                   theme(panel.grid=element_blank()) +
                   labs(title="Gumbel Copula", x=colnames(rets)[i], y=colnames(rets)[j])+
                   geom_text_contour(aes(z = Z))+
                   theme(text = element_text(size = 15),
                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                         axis.text.x = element_text(angle = 45),
                         legend.title = element_text(size=15),
                         legend.text = element_text(size=15),
                         legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
               }
             }
           },
           "Joe"={
             #-------Multivariate Joe Copula Model----------
             fjoe = fitCopula(data=as.matrix(cdf_tran), copula=joeCopula(3, dim=p),method="ml",start=2)
             
             for(i in 1:(p-1)){
               for(j in (i+1):p){
                 # probability between observed asset pairs
                 dem = pempiricalCopula(cdf_tran[,i], cdf_tran[,j])
                 # creating dem data
                 demx=c(); demy=c(); demz=c()
                 for(n in 1:length(dem$x)){
                   for (m in 1:length(dem$y)){
                     demx=c(demx,dem$x[n]);
                     demy=c(demy,dem$y[m]);
                     demz=c(demz,dem$z[n,m])
                   }
                 }
                 demdf=data.frame(X=demx,Y=demy,Z=demz)
                 demdf$type="dem"
                 
                 # fitting copula
                 # Joe is an Archimedean type of copulas, and are therefore an exchangeable distribution.
                 cjoe = joeCopula(coef(fjoe), dim = 2)
                 utdis = rCopula(1e6,cjoe)
                 demt = pempiricalCopula(utdis[,1],utdis[,2])
                 # creating demt data
                 demtx=c(); demty=c(); demtz=c()
                 for(n in 1:length(demt$x)){
                   for (m in 1:length(demt$y)){
                     demtx=c(demtx,demt$x[n]);
                     demty=c(demty,demt$y[m]);
                     demtz=c(demtz,demt$z[n,m])
                   }
                 }
                 demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
                 demtdf$type="demt"
                 # combining dem and demt data
                 DD = rbind(demdf, demtdf)
                 counter=counter+1
                 # creating bivariate plots
                 biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
                   stat_contour()+
                   scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
                   theme(panel.background=element_rect(fill="grey90")) +
                   theme(panel.grid=element_blank()) +
                   labs(title="Joe Copula", x=colnames(rets)[i], y=colnames(rets)[j])+
                   geom_text_contour(aes(z = Z))+
                   theme(text = element_text(size = 15),
                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                         axis.text.x = element_text(angle = 45),
                         legend.title = element_text(size=15),
                         legend.text = element_text(size=15),
                         legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
               }
             }
           },
           "Clayton"={
             #-----------Multivariate Clayton Copula Model------
             fclayton = fitCopula(data=as.matrix(cdf_tran), copula=claytonCopula(3, dim=p),method="ml", optim.method = "BFGS", start=2)
             
             for(i in 1:(p-1)){
               for(j in (i+1):p){
                 # probability between observed asset pairs
                 dem = pempiricalCopula(cdf_tran[,i], cdf_tran[,j])
                 # creating dem data
                 demx=c(); demy=c(); demz=c()
                 for(n in 1:length(dem$x)){
                   for (m in 1:length(dem$y)){
                     demx=c(demx,dem$x[n]);
                     demy=c(demy,dem$y[m]);
                     demz=c(demz,dem$z[n,m])
                   }
                 }
                 demdf=data.frame(X=demx,Y=demy,Z=demz)
                 demdf$type="dem"
                 
                 # fitting copula
                 # Clayton is an Archimedean type of copulas, and are therefore an exchangeable distribution.
                 cclayton = claytonCopula(coef(fclayton), dim = 2)
                 utdis = rCopula(1e6,cclayton)
                 demt = pempiricalCopula(utdis[,1],utdis[,2])
                 # creating demt data
                 demtx=c(); demty=c(); demtz=c()
                 for(n in 1:length(demt$x)){
                   for (m in 1:length(demt$y)){
                     demtx=c(demtx,demt$x[n]);
                     demty=c(demty,demt$y[m]);
                     demtz=c(demtz,demt$z[n,m])
                   }
                 }
                 demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
                 demtdf$type="demt"
                 # combining dem and demt data
                 DD = rbind(demdf, demtdf)
                 counter=counter+1
                 # creating bivariate plots
                 biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
                   stat_contour()+
                   scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
                   theme(panel.background=element_rect(fill="grey90")) +
                   theme(panel.grid=element_blank()) +
                   labs(title="Clayton Copula", x=colnames(rets)[i], y=colnames(rets)[j])+
                   geom_text_contour(aes(z = Z))+
                   theme(text = element_text(size = 15),
                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                         axis.text.x = element_text(angle = 45),
                         legend.title = element_text(size=15),
                         legend.text = element_text(size=15),
                         legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
               }
             }
           },
           "Frank"={
             #----------Multivariate Frank Copula Model----------
             ffrank = fitCopula(data=as.matrix(cdf_tran), copula=frankCopula(3, dim=p), method="ml",start=2)
             
             for(i in 1:(p-1)){
               for(j in (i+1):p){
                 # probability between observed asset pairs
                 dem = pempiricalCopula(cdf_tran[,i], cdf_tran[,j])
                 # creating dem data
                 demx=c(); demy=c(); demz=c()
                 for(n in 1:length(dem$x)){
                   for (m in 1:length(dem$y)){
                     demx=c(demx,dem$x[n]);
                     demy=c(demy,dem$y[m]);
                     demz=c(demz,dem$z[n,m])
                   }
                 }
                 demdf=data.frame(X=demx,Y=demy,Z=demz)
                 demdf$type="dem"
                 
                 # fitting copula
                 # Frank is an Archimedean type of copulas, and are therefore an exchangeable distribution.
                 cfrank = frankCopula(coef(ffrank), dim = 2)
                 utdis = rCopula(1e6,cfrank)
                 demt = pempiricalCopula(utdis[,1],utdis[,2])
                 # creating demt data
                 demtx=c(); demty=c(); demtz=c()
                 for(n in 1:length(demt$x)){
                   for (m in 1:length(demt$y)){
                     demtx=c(demtx,demt$x[n]);
                     demty=c(demty,demt$y[m]);
                     demtz=c(demtz,demt$z[n,m])
                   }
                 }
                 demtdf=data.frame(X=demtx,Y=demty,Z=demtz)
                 demtdf$type="demt"
                 # combining dem and demt data
                 DD = rbind(demdf, demtdf)
                 counter=counter+1
                 # creating bivariate plots
                 biPlotList[[counter]] = ggplot(DD, aes(x=X, y=Y, z=Z, colour=type)) +
                   stat_contour()+
                   scale_color_manual(labels = c("Empirical", "Theoretical"), values = c("blue", "red")) +
                   theme(panel.background=element_rect(fill="grey90")) +
                   theme(panel.grid=element_blank()) +
                   labs(title="Frank Copula", x=colnames(rets)[i], y=colnames(rets)[j])+
                   geom_text_contour(aes(z = Z))+
                   theme(text = element_text(size = 15),
                         plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)),
                         axis.text.x = element_text(angle = 45),
                         legend.title = element_text(size=15),
                         legend.text = element_text(size=15),
                         legend.position=ifelse(counter==(p*(p-1)/2)*length(modelType),"right", "none"))
               }
             }
           }
    )
  }
  grid.arrange(grobs = biPlotList,
               ncol = 6,
               gp = gpar(fontsize = 10, font = 8, face="bold"))
}

  
##########################getOptPortAllocTable##########################
getOptPortAllocTableList<-function(rets, allocation, muPrange, riskFreeRate, shortSellType){
  
  optPortAllocTableList<-list()
  RRPlotList<-list()
  # dimension of assets
  p<-dim(rets)[2]
  # record sample mean and sample covariance
  sampmean<-colMeans(rets)
  sampcov<-cov(rets)
  # Expected return and std. dev. of current portfolio
  portfolio_mean<-t(allocation)%*%sampmean
  portfolio_var<-t(allocation)%*%sampcov%*%allocation
  # Mean vector and covariance matrix
  mean_vect = colMeans(rets)
  cov_mat = cov(rets)
  sd_vect = sqrt(diag(cov_mat))
  # daily risk free rate
  mufree=riskFreeRate/253
  # container dataframe
  optPortAllocD=data.frame("Tickers"=colnames(rets))
  optPortType=c()
  Exp.Ret=c()
  Std.Dev=c()
  
  for(ss in shortSellType){
    switch(ss,
      "Short-sell"={data.frame("Tickers"=colnames(rets))
        #-------------------Allow Short-sell (SS)-------------------
        Amat_SS = cbind(rep(1,p),mean_vect) # set the constraints matrix
        muP_SS = seq(muPrange[1], muPrange[2], length=1000) # set of 1000 possible target values
        # for the expect portfolio return
        sdP_SS = muP_SS # set up storage for std devs of portfolio return
        weights_SS = matrix(0,nrow=1000,ncol=p) # storage for portfolio weights
        
        for (i in 1:length(muP_SS)){ # find the optimal portfolios for each target expected return
          bvec_SS = c(1,muP_SS[i]) # constraint vector
          result_SS = solve.QP(Dmat=2*cov_mat,dvec=rep(0,p),Amat=Amat_SS,bvec=bvec_SS, meq=2)
          sdP_SS[i] = sqrt(result_SS$value)
          weights_SS[i,] = result_SS$solution
        }
        sharpe_SS =(muP_SS-mufree)/sdP_SS # compute Sharpes ratios
        indTP_SS = (sharpe_SS == max(sharpe_SS)) # Find maximum Sharpe's ratio
        indMVP_SS = (sdP_SS == min(sdP_SS)) # Find minimum standard deviation
        # populating allocation table
        optPortAllocD$TAN.SS=weights_SS[indTP_SS,]
        optPortAllocD$MINVAR.SS=weights_SS[indMVP_SS,]
        # populating performance table
        optPortType=c(optPortType,"TAN.SS", "MINVAR.SS")
        Exp.Ret=c(Exp.Ret, muP_SS[indTP_SS], muP_SS[indMVP_SS])
        Std.Dev=c(Std.Dev, sdP_SS[indTP_SS], sdP_SS[indMVP_SS])
        
        # Storing plot 
        portfolio_values<-data.frame(reward=muP_SS, risk=sdP_SS, sharpe_ratio= sharpe_SS)
        RRPlotList[["Short-sell"]]<-portfolio_values %>%
          ggplot(aes(x = risk, y = reward, color = sharpe_ratio)) +
          geom_point() +
          theme_classic() +
          geom_abline(intercept = mufree, slope = (muP_SS[indTP_SS]-mufree)/sdP_SS[indTP_SS], color = 'red')+
          geom_vline(xintercept = sdP_SS[indMVP_SS], color="green")+
          scale_y_continuous(labels = scales::percent) +
          scale_x_continuous(labels = scales::percent)+
          xlim(0, sdP_SS[indTP_SS]+0.1)+
          ylim(0, muP_SS[indTP_SS]+0.01) +
          geom_point(aes(x = risk, y =  reward), data = portfolio_values[indMVP_SS,], color = 'green', size = 5) +
          geom_point(aes(x = risk, y = reward), data = portfolio_values[indTP_SS,], color = 'red', size = 5)+
          labs(x = 'Daily Proportion Risk',
               y = 'Daily Proportion Return',
               title = "Tangency and Minimum Variance Portfolios with Short-sell")+
          theme(text = element_text(size = 15),
                plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)))+
          annotate("text", sd_vect, mean_vect, label=colnames(rets),size=5)
      },
      "No Short-sell"={
        #-----------------Not Allow Short-sell (NSS)--------------------
        Amat_NSS = cbind(rep(1,p),mean_vect, diag(1,nrow=p)) # set the constraints matrix
        # When short sales are prohibited, the target expected return on the portfolio must lie between the smallest and largest expected returns on the stocks
        muP_NSS = seq(min(mean_vect)+.0001,max(mean_vect)-.0001, length=1000)
        # for the expect portfolio return
        sdP_NSS = muP_NSS # set up storage for std devs of portfolio return
        weights_NSS = matrix(0,nrow=1000,ncol=p) # storage for portfolio weights
        
        for (i in 1:length(muP_NSS)){ # find the optimal portfolios for each target expected return
          bvec_NSS = c(1,muP_NSS[i], rep(0,p)) # constraint vector
          result_NSS = solve.QP(Dmat=2*cov_mat,dvec=rep(0,p),Amat=Amat_NSS,bvec=bvec_NSS,meq=2)
          sdP_NSS[i] = sqrt(result_NSS$value)
          weights_NSS[i,] = result_NSS$solution
        }
        
        sharpe_NSS =(muP_NSS-mufree)/sdP_NSS # compute Sharpes ratios
        indTP_NSS = (sharpe_NSS == max(sharpe_NSS)) # Find maximum Sharpe's ratio
        indMVP_NSS = (sdP_NSS == min(sdP_NSS)) # Find minimum standard deviation
        # populating allocation table
        optPortAllocD$TAN.NSS=weights_NSS[indTP_NSS,]
        optPortAllocD$MINVAR.NSS=weights_NSS[indMVP_NSS,]
        # populating performance table
        optPortType=c(optPortType,"TAN.NSS", "MINVAR.NSS")
        Exp.Ret=c(Exp.Ret, muP_NSS[indTP_NSS], muP_NSS[indMVP_NSS])
        Std.Dev=c(Std.Dev, sdP_NSS[indTP_NSS], sdP_NSS[indMVP_NSS])
        # Storing plot 
        portfolio_values<-data.frame(reward=muP_NSS, risk=sdP_NSS, sharpe_ratio= sharpe_NSS)
        RRPlotList[["No Short-sell"]]<-portfolio_values %>%
          ggplot(aes(x = risk, y = reward, color = sharpe_ratio)) +
          geom_point() +
          theme_classic() +
          geom_abline(intercept = mufree, slope = (muP_NSS[indTP_NSS]-mufree)/sdP_NSS[indTP_NSS], color = 'red')+
          geom_vline(xintercept = sdP_NSS[indMVP_NSS], color="green")+
          scale_y_continuous(labels = scales::percent) +
          scale_x_continuous(labels = scales::percent)+
          xlim(0, sdP_NSS[indTP_NSS]+0.1)+
          ylim(0, muP_NSS[indTP_NSS]+0.01) +
          geom_point(aes(x = risk, y =  reward), data = portfolio_values[indMVP_NSS,], color = 'green', size = 5) +
          geom_point(aes(x = risk, y = reward), data = portfolio_values[indTP_NSS,], color = 'red', size = 5)+
          labs(x = 'Daily Proportion Risk',
               y = 'Daily Proportion Return',
               title = "Tangency and Minimum Variance Portfolios without Short-sell")+
          theme(text = element_text(size = 15),
                plot.title = element_text(size=15, face="bold", margin = margin(10, 0, 10, 0)))+
          annotate("text", sd_vect, mean_vect, label=colnames(rets),size=5)
      }
    )
  }
  # finalize allocation table
  optPortAllocD$CURRENT=allocation
  
  # populating performance table
  optPortType=c(optPortType,"CURRENT")
  Exp.Ret=c(Exp.Ret, portfolio_mean)
  Std.Dev=c(Std.Dev, portfolio_var)
  # finalize performance table
  optPortPerformD=data.frame(Portfolio=optPortType, Exp.Ret=Exp.Ret, Std.Dev=Std.Dev)
  
  ## store dataframes in list 
  optPortAllocTableList[[1]]=optPortAllocD
  optPortAllocTableList[[2]]=optPortPerformD
  optPortAllocTableList[[3]]=RRPlotList
  optPortAllocTableList
}

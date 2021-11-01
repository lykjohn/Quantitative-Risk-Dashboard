<p align="center">
  <a href="" rel="noopener"></a>
  <img src="images/banner2.png" alt='home' width='750' height='350' >
</p>
  
<h3  align='center'> Quantitative Risk Dashboard - an R Shiny Web Application for Risk Analysis</h3>

[![Setup Automated](/images/R.svg)](https://www.r-project.org/)
[![Setup Automated](/images/RShiny.svg)](https://www.rstudio.com/products/shiny/)
<br/>
[![Setup Automated](/images/github.svg)](https://github.com/lykjohn)
[![Setup Automated](/images/linkedin.svg)](https://www.linkedin.com/in/lykjohn)
[![Setup Automated](/images/cv.svg)](https://github.com/lykjohn/Resume/blob/main/Resume.pdf)

[What is This About?](#about)&nbsp;|&nbsp;[How It Works](#how_it_works)&nbsp;|&nbsp;[Quick Start](#quick_start)&nbsp;|&nbsp; [Examples](#examples)&nbsp;|&nbsp;[Limitations](#limitations)&nbsp;|&nbsp;[Why This Program?](#differences)&nbsp;|&nbsp;[References](#references)&nbsp;|&nbsp;[Other Projects](#projects)&nbsp;

## What is this about? <a name = "about"></a>
Have you just started your investment journey and wondered how much to invest? Did you follow promising strategies but still see your account in red? Are you worried about the risk that you cannot foresee in your holdings? If you are nodding along these questions, know that you are not alone. In fact, these are what professionals like quants, traders, and asset managers wonder but fail to solve all the time- one often enters the market with a correct timing at a correct price but walks out with a flat wallet, and worse off, with margin debt. The Quantitative Risk Dashboard is a research tool that helps users demystify risk prospects of any U.S. listed stocks. It delivers results from risk modeling, position tracking, and portfolio optimization tactics adopted in proprietary solutions. Users of the dashboard are expected to enter the market with a statistical edge, while being reminded about the key question- how much are you prepared to lose?

## How does this work? <a name = "how_it_works"></a>
The Quantitative Risk Dashboard serves as a risk manager to predict the amount of your net capital loss, given your portfolio strategy. To do this, it evaluates the Value-at-Risk (VaR) metric at a user-specific level. For instance, having a VaR of 100 at a 0.05 level means that there is a 5% likelihood to lose at least $100 of your net capital. All data are daily observations pulled via <a href="https://finance.yahoo.com/"> Yahoo Finance</a>. Keen on some statistical jargon? This dashboard takes on a statistical approach by modeling single and multiple asset risks using probabilistic techniques. For a single asset, univariate distributions, Pareto tails, and ARMA/GARCH time series models are used to reference the probability of negative returns at tail levels. For a multi-asset portfolio, multivariate distributions and copula methods are used to profile book losses at tail levels. One add-on feature is to perform Exploratory Data Analysis prior to risk-modeling with the primary goal to interpret summary statistics and correlated returns. Another add-on feature is the quadratic programming algorithm that optimizes a portfolio’s allocation based on adequate risk-to-reward measures such as the Sharpe Ratio. 

## Quick Start <a name = "quick_start"></a>
### Prerequisites

<ol> 
 <strong>  <li> Download <a href="https://www.rstudio.com/products/rstudio/download/"> R Studio</a> </li></strong>
 <strong> <li> Install R Libraries:</li></strong>
  
```
  ----- to build backend algorithms -----
install.packages("corrplot")
install.packages("tidyr")
install.packages("metR") # long to install
install.packages("reshape2")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("quantmod")
install.packages("purrr")
install.packages("MASS")
install.packages("mnormt")
install.packages("nimble") # long to install
install.packages("evir")
install.packages("fBasics")
install.packages("fGarch")
install.packages("fCopulae")
install.packages("copula")
install.packages("quadprog")
install.packages("tseries")
library("lubridate")
install.packages("forecast")
  
  ----- to build dashboard components-----
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("shinycssloaders")
install.packages("shinyBS")
install.packages("spsComps")
```
</ol> 

### Univariate (single-asset) Analysis
There are three pahses for performing the univariate analysis- asset specification, Exploratory Data Analysis (EDA), and Extreme Value Analysis (EVA). These precedures allow user to firstly, define their asset of choice and the time horizon to consider, secondly, understand the trend and spread of its returns, and thirdly, estimate the proportion of portfolio that was being risked at some specified level. Each of the phases are elaborated below.

<img src="images/Univariate Analysis.png" alt='Univariate Analysis' width='750' height='350'>
<details>
  <summary><strong>PHASE 1: Asset Specification <strong></summary>
  Users will specify asset ticker symbols from their asset of choice. One’s asset of choice can range from stocks, ETF, currencies, cryptocurrencies, etc., be they foreign or domestic. This is equivalent to searching up the ticker in <a href="https://finance.yahoo.com/"> Yahoo Finance</a>, clicking “Historical Data“, then setting the “Time Period”. R will then load in the resulted data from the webpage. The default unit of price data is in USD. With that in mind, user can now apply Exploratory Data Analysis (EDA) methods to the loaded data.
  <br/>
  In the following example, the user is searching up Apple's stock- with the ticker symbol AAPL, to retrieve stock data from 10/01/2019-10/01/2021.
  <img src="images/Univariate-Asset Specification.png" alt='Asset Specification' width='1000' height='350'>
</details>

<details>
  <summary><strong>PHASE 2: Exploratory Data Analysis (EDA)<strong></summary>
  The retrieved stock data is further delineated to only contain the asset's adjusted close price (in USD), which is the price after accounting for corporate actions such as stock-splits. EDA contains 2 steps: first, visualizing the asset’s empirical trends and second, assessing the distribution of asset return. After the process, user will develop a descriptive instinct on the the asset’s price evolution. Please see the following example for Apple’s stock over the same period as above. 
 <ul>
  <li> 
    <details>
      <summary>Time Plots:</summary> Time plots are what we use to visualize the asset’s trends. After clicking on the [Run Time Plots] button, 2 plots will be generated, demonstrating the price trend and differenced log-returns simultaneously. For the prior, trend is important in identifying the momentum and strength as price evolves. For the later, seekers for its predictability may look at the differenced log-returns, and check for stationarity- the more stationary the series, the more predictable the asset’s return. An asset's return is represented by its log-return. Log-return is used here because it has the nice arithmetic property of normalizing values.
      <img src="images/EDA-Time Plots.png" alt='EDA-Time Plots.png'>
    </details>
  </li>
  <li>  
    <details>
      <summary>Summary Statistics:</summary> How asset returns are distributed would constitute the scale of risk that the asset poses in terms of an account’s gains/losses. After clicking the [Summary Statistics] button, statistics such as the standard deviation, kurtosis, and skewness are tabulated alongside with a boxplot that highlights the 25%, 50% 75% risk thresholds. These are not for evaluating probabilstic risks just yet, but rather purposed for illustrating the spread/extremeness of the asset’s return.
  <img src="images/EDA-Summary Statistics.png" alt='EDA-Summary Statistics'>
    </details>
  </li>
 </ul>
</details>
    
<details>
  <summary><strong>PHASE 3: Extreme Value Analysis (EVA) <strong></summary>
    The goal of EVA is to seek probability distributions that best fit the tails of the asset's log returns, which in turn helps accurately model the Value-at-Risk(VaR) and Expected Shortfalls of an investment. To do this, some off-the-shelf univariate distributions are first examined, followed by Generalized Pareto Distibutions, then time-series ARMA/GARCH variants to model returns & volatilies with monitored residuals. 
   <ul>
     <li>
        <details>
          <summary>Univariate Tails:</summary>
              Here, various distribution curves- Normal, Student-t, Double-Exponential (DExp), and Generalized Error Distribution (GED)- are used to fit the log-returns of the speceified asset; qauntile-quantile plots are also used to compare the empirical tail distribution with the theoretical ones. To see these results, please hit the [Fit Histograms and QQ pLots] button. Users will select the diistribution that best fit the asset's log-returns before constructing risk models. For instance according to histograms and qqplots, the DExp and GED models seem to best fit AAPL's log-returns from 10/01/2019 to 10/01/2021.
              <br/>
              <img src="images/EVA-Univariate Tails-Hist & QQ.png" alt='EVA-Univariate Tails-Hist & QQ'>   
              <br/>
              The risk table displays the relative VaR and relative Expected Shortfall predicted by the selected models. To see these results, please hit the [Run Risk Table] button. Users can reference their risk based on the best-fitted model determined above. For instance, the GED model predicts that an investment in AAPL's stock has a 5% probability of losing at least 3% and on average losing 5.44%.<br/>
              <img src="images/EVA-Univariate Tails-Risk Table.png" alt='EVA-Univariate Tails-Risk Table'>
        </details>
     </li>
     <li>
       <details>
            <summary>Generalized Pareto Tails:</summary>
            While risk analyses are most concerned about the accuracy of loss projections, most distributions fail to capture the tail probabilities of an asset's log-returns. This is why the Generalized Pareto Distribution (GPD) is designed, to be fitted to exceedances over a threshold. After specifying a risk level, users have the option to click the [Run ECDF Plots] button to observe the zoomed in transgression of the empirical cumulative structure of the loss probabilities.
            <br/>
            <img src="images/EVA-GP Tails-ECDF Plots.png" alt='EVA-GP Tails-ECDF Plots'>
            <br/>
            Next, users can click the [Run Pareto Shape Plot] button to explore GPD's shape parameter plotted over different thresholds at different exceedances, then select the threshold at which the shape is the most stable. This is to ensure that a persistent shape estimate as for small and large sample sizes, but please note that the larger the shape parameter, the heavier the tail return, and vice versa. 
            <br/>
            <img src="images/EVA-GP Tails-Shape Plots.png" alt='EVA-GP Tails-Shape Plots'>
            <br/>
            It turns out that the shape parameter seems to be the most stable at the 0.002 threshold. Observations may vary for different users. Once the proper threshold is selected, users may specify it in the GPD threshold field. Hit the [Run Risk Table] button and 3 vizuals will be produced. The first vizual is the risk table containing the relative VaR and relative Expected Shortfall predicted by the specified GPD model. Same as DExp and GED, the GPD model predicts that an investment in AAPL's stock has a 5% probability of losing at least 3.38% and on average losing 5.39%. The second visual shows the goodness of fit of the Pareto right tail. The closer the 1-F(x) are to the line, the better the model's fit. The third visual ilustrates the stableness of the chosen threshold at the risk-level cut-off. The less fluctuation along decreasing exeedances indicates a more stable threshold. 
            <br/>
            <img src="images/EVA-GP Tails-Risk Table.png" alt='EVA-GP Tails-Risk Table'>
       </details>
     </li>
     <li>
       <details>
            <summary>GARCH Models:</summary>
            We take a time-dependent approach to model the tail values of the asset returns while accounting for volatility clustering. The default model is GARCH(1,1). This model generates data that appears to have heavy tails partly because of heteroskedasticity or the variance is going up and down such that the distribution has different scale factors. User can adjust the sigma and ARCH orders in the GARCH framework, and also add AR and MA components to form ARMA+GARCH models. If you believe that ARMA+GARCH is a better fit, then wait till you fit those models before fitting only GARCH because you will not get normally distributed residuals if you don't fit the ARMA+GARCH part correctly. User can also select model variants with Normal, Student-t, and GED innovations to draw side-by-side comparisons. After specifying these parameters, click the [Run Summary Table] button to obtain estimation and diagnostics of the selected models. Evaluate the outcomes of alpha{?} & beta{?} significances, |alpha{?}+beta{?}| term, Jarque-Bera Test, Shapiro-Wilk Test, First 3 Ljung-Box Tests, Last 3 Ljung-Box Tests, and AIC & BIC based on the diagnostic descrption (drop-down in blue). Then select the appropriate model(s) to assess their residuals. In the example below, the user has selected an ARMA(0,1)+GARCH(1,1) model.
            <br/>
            <img src="images/EVA-GARCH Models-Summary Table.png" alt='EVA-GARCH Models-Summary Table'>
            <br/>
            Volatility clustering is a phenomenon where one large swing in asset return will likely be followed by another large swing. For example, during the COVID-19 pandemic, the S&P 500 Index plummeted 15% in the first week, followed by at least a 10% drop in the next few weeks. The volatility (sigma) component in the GARCH model is coupled with white noise innovations, specifically Normal, Studnet-t, and GED, with the goal of preserving the persistence of outlier values. For a given innovation, two graphs are plotted after hitting the [Run Volatility and Residual Estimations] button- the estimated volatilities and the estimated residuals plots. For the former, the closer the red line traces the boundaries of asset returns the more persistently the volatility is captured; for the later, the more the series looks like a weak white noise, the more accuratedly the residuals are estimated. Volatilities for all three innovations seems to be estimated quite similarly in the cases below. 
            <br/>
            <img src="images/EVA-GARCH Models-Vol Res Est.png" alt='EVA-GARCH Models-Vol Res Est'>
            <br/>
            Next, we examine the distribution of the estimated residuals. Typically, such estimates are not normally distributed; rather they are expected to have heavier tail values, like the Student-t distribution. User may click on the [Fit Histogram and QQ Plots on Residuals] button to produce histogram and QQ plots to assess this. In the summary results above, Jarque Bera & Shapiro Wilk tests may also be used to determine normality if the residuals looks normal visually. Users would select a innovation such that its theoretical distribution closely aligns the distribution of estimated residuals. In our case, the Student-t innovation seems appropriate because its theoretical distribution properly models the estimated residuals (see by well-traced curve on the histogram and tails closely surrounding the linear line in the qqplot). Also, it also possesses the least AIC & BIC values compared to models assuming other innovations.
            <br/>
            <img src="images/EVA-GARCH Models-Res Hist & QQ.png" alt='EVA-GARCH Models-Res Hist & QQ'>
            <br/>
            If the estimates are correct, then the error from the predictive model should be uncorrelated with estimates you have.The frist 3 Ljung-Box tests examine whether the ARMA part of the model yields uncorrelated residuals. The last 3 Ljung-Box tests examine whether the GARCH part of the model yields uncorrelated residuals, which it should. User can also use a autocorrelation plot to view the correlation between squared residuals across lags, assuming different innovations. Press the [Run Autocorrelations on Squared Residuals] to obtain the autocorrelation plots. In our example, estimated residuals under all three innovations seems to be correlated at the 6th, 15, and 24th lag.
            <br/>
            <img src="images/EVA-GARCH Models-Res Squared ACF.png" alt='EVA-GARCH Models-Res Squared ACF'>
            <br/>
            Users can hit the [Run Risk Table] button and interpret the risk table in the same way as before. For this example, according to the ARMA(0,1)+GARCH(1,1) model with Student-t innovation, an investment in AAPL's stock has a 5% probability of losing at least 2.72% and on average losing 3.82%.
            <br/>
            <img src="images/EVA-GARCH Models-Risk Table.png" alt='EVA-GARCH Models-Risk Table'>
       </details>
     </li>
  </ul>
</details>

### Multivariate (multiple-asset) Analysis
There are four pahses for performing the multivariate analysis- asset specification, Exploratory Data Analysis (EDA), Extreme Value Analysis (EVA), and portfolio optimization. These precedures allow user to firstly, define their asset of choice and the time horizon to consider, secondly, understand the trend and spread of its returns, and thirdly, estimate the proportion of portfolio that was being risked at some specified level. Each of the phases are elaborated below.
  
<img src="images/Multivariate Analysis.png" alt='Multivariate Analysis' width='750' height='350'>

<details>
  <summary><strong>PHASE 1: Asset Specification <strong></summary>
  Users will specify the ticker symbol, quantity, and current price for each asset of their choice. Click the [Add Asset] button to add an asset and the [Remove Asset] button to remove the last asset specified. One’s chosen assets can range from stocks, ETF, currencies, cryptocurrencies, etc., be they foreign or domestic. This process replaces manual asset lookupd in <a href="https://finance.yahoo.com/"> Yahoo Finance</a>, and compiles a dataframe consisting the asset returns ready for analysis. The default unit of price data is in USD. <br/>
 **Note that entering more than 10 assets may lead to considerable lag in retrieving asset data.** 
  <br/>
  In the following example, the user is listing Apple (AAPL), JP Morgan (JPM), Lockheed Martin (LMT), and Tesla (TSLA)'s stocks as part of their portfolio. There are 4 stocks in total with a Net Liquidation Value (NLV) of $288755- the final worth of the user's account once all  positions are closed. The panel also displays the current allocation of each of the selected assets per portfolio. In this case, the account consists of 20.75% Apple-Technology, 17.65% JP Morgan-Financials, 23.02% Lockheed Martin-Industrials, and 38.58% Tesla-Consumer Cyclical, which seems fairly allocated by sectors.
  <img src="images/Multivariate-Asset Specification.png" alt='Asset Specification' width='1000' height='350'>
</details>
   
<details>
  <summary><strong>PHASE 2: Exploratory Data Analysis (EDA)<strong></summary>
  Here, EDA contains 3 parts: plotting pairwise  relationship between asset returns, selecting distribution that best fits the general tail extremes, and identifying correlations between asset returns forf hedging puroses. Please see the following for implementing these parts on the Apple, JP Morgan, Lockcheed Martin, and Tesla's stock data. 
 <ul>
  <li> 
    <details>
      <summary>Scatter Matrix:</summary> A scatter matrix is used to plot the relationships between pair variants of asset returns. It shows the direction, magnitude, linearity, strenght, and potential outliers within a return relationship. Users may click on the [Run Scatter Matrix] button to produce this plot.
      <br/><br/>
      <ul>
        <li>Direction: positive or negative. positive meaning as asset X's return increases (decreases), asset Y's return increases (decreases) as well; negative meaning as asset X's return increases (decreases), the asset Y's return decreases (increases).</li>
        <li>Linearity: linear or nonlinear. linear if the points are to a straight line, nonlinear otherwise.</li>
        <li>Strength: weak, moderate or strong. the more spread out the points are, the weaker the relationship. If the points are clearly clustered, or closely follow a curve or line, the relationship is described as strong.</li>
        <li>Potential Outliers: exists or don't exist.  exists for the point or points that are farthest from the regression line; these points normally stands out. DOn't exists for points closely clustered around the regression line; these points normally don't stand out.</li>
      </ul>
      <br/>
      Take LMT and TSLA's returns for example, they have a relatively weak nonlinear positive relationship with a good amount of outliers. The direction is positive because the returns generally seems to move in the same direction. The association is weak because points seem to be scattered randomly all over the place. It is also nonlinear because the overall relationship cannot be traced by a linear line. There are many outliers because many points lie far away from the cluster. 
      <img src="images/EDA-Scatter Matrix.png" alt='EDA-Scatter Matrix.png'>
    </details>
  </li>
  <li>  
    <details>
      <summary>Quantile-Quantile Plots:</summary> Here, QQ plots are used to compare the sample quantiles of asset returns thier theoretical quantiles. The objective is to evlauate the goodness of fit for various distributions (e.g., Normal, Stduent-t) on asset returns, particularly on extreme returns. A convex plot means the sample returns is more right-skewed compared to its theoretical, as a concave plot indicates more left-skewed returns. If either tail deviates from the center, this implies heavier/lighter tails, or more/less returns are located at the extremes relative to the center. In short, the closer the points lie along the linear line, the more appropriate the distibution in modeling the asset returns. Users may press the [Run QQ Plots] button to produce these plots, then select a distribution that best fits most of the plots using the drop-down option.<br/>
      <br/>
      In the example below, t-distribution is selected to model the each of the specified asset returns. AAPL and TSLA's returns seem to be properly modeled because their quantiles lie closely along the straight line. However, JPM and LMT's return seems to have lighter tails than the theoretical Student-t distribution, but not that far off that we could still use Student-t for EDA. 
  <img src="images/EDA-Multivariate QQPlots.png" alt='DA-Multivariate QQPlots'>
    </details>
  </li>
  <li>  
    <details>
      <summary>Correlation Martix:</summary> A correlation matrix is a table that display correlations between each pair of asset returns. Correlation is a measure that lies between -1 and 1. A perfect positive correlation means that the correlation coefficient is exactly 1. This implies that as one security moves, either up or down, the other security moves in lockstep, in the same direction. Please note that a pair of highly correlated assets is a double-bladed sword- like how it could substantially uplift your profits, it could substantially deteriorate them as well. A perfect negative correlation means that two assets move in opposite directions, while a zero correlation implies no linear relationship at all. Assets with low-correlated returns are often used to hedge cyclical market movements. Users may press the [RUn Correlation Matrix] button to generate this statistic.<br/>
      <br/>
      In the example below, LMT and AAPL return has a correlation coefficient of 0.27, as LMT and JPM return has a correlation coefficient of 0.46. This means that when one takes position in LMT stock, they may be better off holding AAPL if their target is to multiply thier profits, and holding JPM if their target is to offset market risks.
  <img src="images/EDA-Correlation Matrix.png" alt='EDA-Correlation Matrix'>
    </details>
  </li>
 </ul>
</details>
 
<details>
  <summary><strong>PHASE 3: Extreme Value Analysis (EVA) <strong> </summary>
    
  <ul>
   <li>
      <details>
        <summary>Multivariate Tails:</summary>
          Multivariate Tails here
        
          <img src="images/EVA-Multivariate Tails-Risk Table.png" alt='EVA-Multivariate Tails-Risk Table'>
      </details>
   </li>

   <li>
      <details>
        <summary>Copula Tails:</summary>
          Copula Tails here
        
          <img src="images/EVA-Multivariate Tails-Bivariate ECDF.png" alt='EVA-Multivariate Tails-Bivariate ECDF'>
      </details>
   </li>
  </ul>
</details>
    

## Company Examples and Key Takeaways<a name = "examples"></a>

## Limitations <a name = "limitations"></a>

## Why This Application? <a name = "differences"></a>

## References <a name = "references"></a>

## Other Projects <a name = "projects"></a>
<ul>
  <li> <a href="https://github.com/lykjohn/SEC-Python-Scraper"> U.S. Securities and Exchange Commission (SEC) Python Scraper</a> </li>
  <li> <a href="https://github.com/lykjohn/Viable-Long-Term-Investment/blob/main/final_report.pdf"> Large-Scale Computation to Identify Viable Long-Term Investment Opportunities</a> </li>
  <li> <a href="https://github.com/lykjohn/Bayesian-Recession-Forecast/blob/master/bayesian_recession_report.pdf"> Economic Recession Forecast Under the Bayesian Hierarchical Framework</a> </li>
  <li> <a href="https://github.com/lykjohn/Time-Series-Insurace-Premium/blob/master/premium_modeling_report.pdf"> Evaluating Time Series Models for Predicting Smokers and Non-Smoker Premiums</a> </li>
</ul>

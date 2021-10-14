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
The Quantitative Risk Dashboard serves as a risk manager to predict the amount of your net capital loss, given your portfolio strategy. To do this, it evaluates the Value-at-Risk (VaR) metric at a user-specific level. For instance, having a VaR of 100 at a 0.05 level means that there is a 5% likelihood to lose at least $100 of your net capital. Keen on some statistical jargon? This dashboard takes on a statistical approach by modeling single and multiple asset risks using probabilistic techniques. For a single asset, univariate distributions, Pareto tails, and ARMA/GARCH time series models are used to reference the probability of negative returns at tail levels. For a multi-asset portfolio, multivariate distributions and copula methods are used to profile book losses at tail levels. One add-on feature is to perform Exploratory Data Analysis prior to risk-modeling with the primary goal to interpret summary statistics and correlated returns. Another add-on feature is the quadratic programming algorithm that optimizes a portfolio’s allocation based on adequate risk-to-reward measures such as the Sharpe Ratio. 

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

### Single-asset Analysis

### Multiple-asset Analysis


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

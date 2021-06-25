# ui.R
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyBS)
library(spsComps)

shinyUI(
  dashboardPage(skin="yellow",
                dashboardHeader(
                  title = span(tags$img(src="banner.png",width = '100%'),
                               titleWidth = 300)
                ),
                dashboardSidebar(width=0),
                dashboardBody(
                  spsDepend("toastr"),
                  tags$p('Disclamer: Users should acknowledge that all models used in this program are WRONG. These models are meant to provide an approximation, but not an exact representation of asset behaviors. Past results obtained from this program DOES NOT imply future performances. Users are encouraged to perform due diligence of market research and SHOULD NEVER rely only on this program to make trading and investment decisions.', style = 'color: #B22222'),
                  tags$head(
                    tags$style(
                      HTML('
                /* ************* Adjust Banner ************** */
                /*  Move everything below the header */
                .content-wrapper {
                    margin-top: 0px;
                }
                .content {
                    padding-top: 50px;
                }
                
                /*  Make the image taller */
                .main-header .logo {
                    width: 500px;
                    height: 100px;
                }
                /* Override the default media-specific settings */
                @media (max-width: 5000px) {
                    .main-header {
                        padding: 0 0;
                        position: relative;
                    }
                    .main-header .logo,
                    .main-header .navbar {
                        width: 100%;
                        float: none;
                    }
                    .main-header .navbar {
                        margin: 0;
                    }
                    .main-header .navbar-custom-menu {
                        float: right;
                    }
                }
                /*  Move the sidebar down */
                .main-sidebar {
                    position: absolute;
                }
                .left-side, .main-sidebar {
                    padding-top: 170px;
                }"
                
                
                /* ************* Adjust Body ************ */
                /* logo when hovered */
                .skin-black .main-header .logo:hover {
                  background-color: black;
                  font-family: "Times New Roman"
                }
                        
                /* toggle button when hovered  */
                  .skin-black .main-header .navbar .sidebar-toggle:hover{
                  background-color: black;
                  font-family: "Times New Roman"
                }
                
                /* navbar*/
                  .skin-black .main-header .navbar  { 
                  background-color: black;
                  font-family: "Times New Roman";
                }
                
                
                /* navbar (rest of the header) */
                .skin-black .main-header > .logo { 
                  background-color: black;
                  font-family: "Times New Roman";
                }
                
                /* main sidebar */
                  .skin-black .main-sidebar {
                  background-color: black;
                  font-family: "Times New Roman"
                }
                
                /* body */
                  .content-wrapper, .right-side {
                  background-color: black;
                  font-family: "Times New Roman"
                }
              ')
                    )
                  ),
                  bsCollapsePanel("Univariate Analysis",
                    h1(HTML("<strong style = 'font-family:Bodoni MT; color:#6A7758'>Using Univariate Tail, Pareto Properties, and Time Series Techniques to Decode an Asset Performance</strong>")),
                    bsCollapsePanel("PHASE 1: Asset Specification",
                                    tags$p(HTML('Please specify the following for each asset: ')),
                                    tags$ul(
                                      tags$li(HTML("<p>Asset ticker</p>")),
                                      tags$li(HTML("<p>Date range of interest</p>"))
                                    ),
                                    column(2,
                                      textInput(inputId = "tickerInputUni",
                                                label="Ticker:",
                                                placeholder="(e.g., AAPL)")),
                                   
                                    column(8,
                                      dateRangeInput(inputId ="dateRangeUni", 
                                                     label="Date Range:", 
                                                     start="2018-01-01", 
                                                     end="2021-05-21")
                                    )
                    , style="warning"),
                    bsCollapsePanel("PHASE 2: Exploratory Data Analysis",
                                    tabBox(
                                      id="phase2TabUni",
                                      width=15,
                                      tabPanel(
                                        "Time Plots",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Time Plots: </strong> includes movement of prices along time with and without transformation (e.g., first difference).</p>"))
                                          )
                                        ), style="info"),
                                        actionButton("runTimePlots",
                                                     class = "btn-danger",
                                                     "Run Time Plots", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                     background-color: #8A0000"),
                                        shinycssloaders::withSpinner(
                                          type = 6, 
                                          color = "#800000", 
                                          size = 0.5,
                                          plotOutput(outputId="timePlots")
                                        )
                                      ),
                                      tabPanel(
                                        "Summary Statistics",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Summary Statistics:</strong> quantities and graphics to describe the center, spread, and shape of log returns. </p>"))
                                          )
                                        ), style="info"), 
                                        actionButton("runSumStats",
                                                     class = "btn-danger",
                                                     "Run Summary Statistics", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                     background-color: #8A0000"),
                                        fluidRow(
                                          column(4,
                                            shinycssloaders::withSpinner(
                                              type = 6, 
                                              color = "#800000", 
                                              size = 0.5,
                                              tableOutput(outputId="sumStatsTable")
                                            )
                                          ),
                                          column(3,
                                                 offset=0,
                                            plotOutput(outputId="sumStatsBoxPlot")
                                          )
                                        )
                                      )
                                    )          
                    ,style="warning"),
                    bsCollapsePanel("PHASE 3: Extreme Value Analysis",
                                    bsCollapsePanel("Definition:", tags$div(
                                      tags$ul(
                                        tags$li(HTML('<p><strong>Value-at-Risk level: </strong> the probability at which your minimum loss shall occur. </p>')),
                                        tags$li(HTML('<p><strong>Value-at-Risk (VaR): </strong> the minimum loss given an extreme risk level, normally 0.05 to 0.1. Relative VaR the proportion of the loss relative to existing capital.</p>')),
                                        tags$li(HTML('<p><strong>Expected Shortfall (ES): </strong> the expected loss amount given that you will lose at least the amount of VaR at an extreme risk level. Relative ES is the proportion of the conditional loss relative to existing capital.</p>'))
                                      )
                                    ), style = "success"),
                                    tags$p(HTML('Please specify the following:')),
                                    tags$ul(
                                      tags$li(HTML("<p>Value-at-Risk (VaR) level</p>"))),
                                    numericInput(inputId = "VaRlevelUni",
                                                 label = "Value-at-Risk Level (between 0 and 1):", 
                                                 value = 0.01),
                                    tabBox(
                                      id="phase3TabUni",
                                      width=15,
                                      tabPanel(
                                        "Univariate Tails",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Univariate Analysis: </strong> a set of techniques used for analysis portfolios with one and only one asset returns, preferably log-returns. Univariate distributions describe the frequency behavior of an asset's log-returns.</p>"))
                                          )
                                        ), style="info"),
                                        checkboxGroupInput("uniTailCheck", 
                                                           "Select Univariate Model(s):",
                                                           inline=TRUE,
                                                           selected=c("Normal",
                                                                      "Student t",
                                                                      "Double Exponential",
                                                                      "Generalized Error Distribution"),
                                                           c("Normal",
                                                             "Student t",
                                                             "Double Exponential",
                                                             "Generalized Error Distribution")),
                                        actionButton("runUniTailHistQQPlots",
                                                                                       class = "btn-danger",
                                                                                       "Fit Histograms and QQ Plots", 
                                                                                       width = "200px",
                                                                                       style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Histograms and QQ Plots",
                                                        verbatimTextOutput("uniTailHistQQPlotsText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          plotOutput("uniTailHistQQPlots")
                                                        )
                                        ),actionButton("runUniTailRiskTable",
                                                       class = "btn-danger",
                                                       "Run Risk Table", 
                                                       width = "200px",
                                                       style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Risk Table",
                                                        
                                                        verbatimTextOutput("uniTailRiskTableText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          tableOutput("uniTailRiskTable")
                                                        ),
                                                        plotOutput("uniTailRiskBarPlot")
                                        )
                                      ),
                                      tabPanel(
                                        "Generalized Pareto Tails",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Generalized Pareto Distribution (GPD): </strong> a family of continuous probability distributions used to model the tail of the asset's log-returns. It is specified by the center, scale, and shape parameters. The larger the shape parameter, the heavier the tail.</p>"))
                                          )
                                        ), style="info"),
                                        actionButton("runECDFPlots",
                                                                      class = "btn-danger",
                                                                      "Run ECDF Plots", 
                                                                      width = "200px",
                                                                      style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("ECDF Plots",
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          plotOutput("ecdfPlots")
                                                        )
                                        ),
                                        actionButton("runParetoShapePlot",
                                                     class = "btn-danger",
                                                     "Run Pareto Shape Plot", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Pareto Shape Plot",
                                                        
                                                        verbatimTextOutput("paretoShapePlotText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          plotOutput("paretoShapePlot")
                                                        )
                                        ),
                                        tags$p(HTML('Please specify the following:')),
                                        tags$ul(
                                          tags$li(HTML("<p>GPD threshold</p>"))),
                                        numericInput(inputId = "paretoThreshold",
                                                     label = "GPD Threshold:", 
                                                     value = 0.02),
                                        actionButton("runParetoTailRiskTable",
                                                     class = "btn-danger",
                                                     "Run Risk Table", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Pareto Risk Table",
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          tableOutput("paretoTailRiskTable")
                                                        ),
                                                        fluidPage(
                                                          column(4, 
                                                            plotOutput("paretoTailRiskBarPlot")
                                                          ),
                                                          column(4, 
                                                                  plotOutput("paretoTailFitPlot")
                                                          ),
                                                          column(4, 
                                                                 plotOutput("paretoTailQuantilePlot")
                                                          )
                                                        )
                                        )
                                      ),
                                      tabPanel(
                                        "GARCH Models",
                                        bsCollapsePanel("Description:", tags$div(
                                        tags$ul(
                                          tags$li(HTML("<p><strong> Generalized Autoregressive Conditional Heteroskedasticity (GARCH) process: </strong> an approach to estimating the volatility of asset returns.  Suppose we have noticed that recent daily returns
    have been unusually volatile. We might expect that tomorrow's return is also more variable than usual. More often than not, GARCH is a better time series model for nonconstant volatility over time.</p>")),
                                          tags$li(HTML("<p><strong> Innovations: </strong> residuals from the specified preferably with stationary, zero-mean, and uncorrelated when estimated. </p>"))
                                        )
              ), 
                                        style = "info"),
                                        tags$p(HTML('Please specify the following:')),
                                        tags$ul(
                                          tags$li(HTML("<p>ARMA(p,q) parameters</p>")),
                                          tags$li(HTML("<p>GARCH(p,q) parameters</p>")),
                                          tags$li(HTML("<p>Innovation distribution(s)</p>"))
                                        ),
                                        fluidRow(
                                          column(2,
                                                 numericInput(inputId = "arma_p",
                                                              label = "ARMA order p", 
                                                              value = 0)
                                          ),
                                          column(2,
                                                 numericInput(inputId = "arma_q",
                                                              label = "ARMA order q", 
                                                              value = 0)
                                          )
                                        ),
                                        fluidRow(
                                          column(2,
                                                 numericInput(inputId = "sigma_p",
                                                              label = "GARCH order p (sigma part)", 
                                                              value = 1)
                                          ),
                                          column(2,
                                                 numericInput(inputId = "arch_q",
                                                              label = "GARCH order q for (ARCH part)", 
                                                              value = 1)
                                          )
                                        ),
                                        checkboxGroupInput("garchResidCheck", 
                                                           "Select Innovation Model(s):",
                                                           inline=TRUE,
                                                           selected=c("Normal"="norm",
                                                                      "Student t"="std",
                                                                      "GED"="ged"),
                                                           c("Normal"="norm",
                                                             "Student t"="std",
                                                             "GED"="ged")),
                                        actionButton("runGARCHSummaryTable",
                                                          class = "btn-danger",
                                                          "Run Summary Table", 
                                                          width = "300px",
                                                          style="color: #FFFFFF;
                                  background-color: #8A0000"),
                                        bsCollapsePanel("GARCH Summary Table",
                                                        bsCollapsePanel("Diagnostics:", tags$div(
                                                          tags$ul(
                                                            tags$li(HTML("<p><strong> |alpha{?}+beta{?}|: </strong> alpha and beta are the coefficients of the variance and lagged observation squared, respectively in the GARCH part of the model. If |alpha_{i}+...+beta{j}|<1, then GARCH part is stable; unstable otherwise. If p-value for a coeeficient is small, then the respective term is significant and can include it in GARCH part, otherwise, cannot.</p>")),
                                                            tags$li(HTML("<p><strong> Jarque-Bera & Shapiro-Wilk Tests: </strong> Testing for normality of standardized residuals in GARCH part of the model. If significant, DO NOT reject the null of normal residuals right away; Need to look at QQ Plot to see whether residuals are normal because large enough data will always reject the null. Also, result not meaningful if innovations are not assumed normal.</p>")),
                                                            tags$li(HTML("<p><strong> First 3 Ljung-Box Tests: </strong> Testing to see if the ARMA part gives uncorrelated residuals. If significant, then means errors not captured by ARMA part of the model. If significant, then must need ARMA+GARCH instead of just GARCH. NOT testing for GARCH part because we know they should be uncorrelated. </p>")),
                                                            tags$li(HTML("<p><strong> Last 3 Ljung-Box Tests: </strong> Testing to see if the GARCH part gives uncorrelated squared residuals. If significant, then means volatility clustering not captured by GARCH part of the model.</p>")),
                                                            tags$li(HTML("<p><strong> AIC & BIC: </strong> We desire the model with the least AIC and BIC values. Compare these criteria for all models fitted, and select the model with the least value before it starts increasing.</p>"))    
                                                          )
                                                        ), 
                                                        style = "primary"),             
                                          fluidRow(
                                            column(4,
                                              tags$h4("with Normal Innovations"),
                                              shinycssloaders::withSpinner(
                                                                                                                       type = 6, 
                                                                                                                       color = "#800000", 
                                                                                                                       size = 0.5,
                                                                                                                       verbatimTextOutput("normGARCHSummaryTable")
                                                                                                                    
                                               )),
                                            column(4,
                                              tags$h4("with Student t Innovations"),
                                              shinycssloaders::withSpinner(
                                                type = 6, 
                                                color = "#800000", 
                                                size = 0.5,
                                                verbatimTextOutput("stdGARCHSummaryTable"))),
                                            column(4,
                                              tags$h4("with GED Innovations"),
                                              shinycssloaders::withSpinner(
                                                type = 6, 
                                                color = "#800000", 
                                                size = 0.5,
                                                verbatimTextOutput("gedGARCHSummaryTable")))
                                          )
                                           
                                        ),
                                       actionButton("runEstCondVolResidPlots",
                                                    class = "btn-danger",
                                                    "Run Volatility and Residual Estimations", 
                                                    width = "300px",
                                                    style="color: #FFFFFF;
                            background-color: #8A0000"),
                                       bsCollapsePanel("Volatility and Residual Estimations",

                                         fluidRow(
                                           column(4,
                                                  tags$h4("with Normal Innovations"),
                                                  
                                                       shinycssloaders::withSpinner(
                                                         type = 6, 
                                                         color = "#800000", 
                                                         size = 0.5,
                                                         plotOutput("normEstCondVolResidPlots")
                                                       )
                                           ),
                                           column(4,
                                                  tags$h4("with Student t Innovations"),
                                                  shinycssloaders::withSpinner(
                                                    type = 6, 
                                                    color = "#800000", 
                                                    size = 0.5,
                                                    plotOutput("stdEstCondVolResidPlots")
                                                  )
                                           ),
                                           column(4,
                                                  tags$h4("with GED Innovations"),
                                                  shinycssloaders::withSpinner(
                                                    type = 6, 
                                                    color = "#800000", 
                                                    size = 0.5,
                                                    plotOutput("gedEstCondVolResidPlots")
                                                  )
                                           )
                                         )
                                       ),
                                       actionButton("runResidHistQQPlots",
                                                    class = "btn-danger",
                                                    "Fit Histogram and QQ Plots on Residuals", 
                                                    width = "300px",
                                                    style="color: #FFFFFF;
                            background-color: #8A0000"),
                                       bsCollapsePanel("Histogram and QQ Plots",
                                                       fluidRow(
                                                         column(4,
                                                                tags$h4("with Normal Innovations"),
                                                                shinycssloaders::withSpinner(
                                                                  type = 6, 
                                                                  color = "#800000", 
                                                                  size = 0.5,
                                                                  plotOutput("normResidHistQQPlot")
                                                                )
                                                         ),
                                                         column(4,
                                                                tags$h4("with Student t Innovations"),
                                                                shinycssloaders::withSpinner(
                                                                  type = 6, 
                                                                  color = "#800000", 
                                                                  size = 0.5,
                                                                  plotOutput("stdResidHistQQPlot")
                                                                )
                                                         ),
                                                         column(4,
                                                                tags$h4("with GED Innovations"),
                                                                shinycssloaders::withSpinner(
                                                                  type = 6, 
                                                                  color = "#800000", 
                                                                  size = 0.5,
                                                                  plotOutput("gedResidHistQQPlot")
                                                                )
                                                         )
                                                       )
                                                       
                                       ),
                                        actionButton("runSquaredResidACF",
                                                     class = "btn-danger",
                                                     "Run Autocorrelations on Squared Residuals", 
                                                     width = "300px",
                                                     style="color: #FFFFFF;
                                                      background-color: #8A0000"),
                                        bsCollapsePanel("Squared Residuals ACF",
                                                        fluidRow(
                                                          column(4,
                                                                                                                              tags$h4("with Normal Innovations"),
                                                          shinycssloaders::withSpinner(
                                                            type = 6, 
                                                            color = "#800000", 
                                                            size = 0.5,
                                                            plotOutput("normSquaredResidACF")
                                                          )),
                                                        column(4,
                                                               tags$h4("with Student t Innovations"),
                                                          shinycssloaders::withSpinner(
                                                            type = 6, 
                                                            color = "#800000", 
                                                            size = 0.5,
                                                            plotOutput("stdSquaredResidACF")
                                                          )),
                                                        column(4,
                                                               tags$h4("with GED Innovations"),
                                                          shinycssloaders::withSpinner(
                                                            type = 6, 
                                                            color = "#800000", 
                                                            size = 0.5,
                                                            plotOutput("gedSquaredResidACF")
                                                          ))
                                                       )
                                        ),
                                        actionButton("runGarchTailRiskTable",
                                                     class = "btn-danger",
                                                     "Run GARCH Tail Risk Table", 
                                                     width = "300px",
                                                     style="color: #FFFFFF;
                                                                                background-color: #8A0000"),
                                        bsCollapsePanel("GARCH Tail Risk Table",
                                                        verbatimTextOutput("garchTailRiskTableText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          tableOutput("garchTailRiskTable")
                                                        ),
                                                        plotOutput("garchTailRiskBarPlot")
                                        )
                                      )
                                    ),
                    style="warning"),
                   style="danger"),
                  bsCollapsePanel("Multivariate Analysis",
                    tags$script(HTML('
        $(document).ready(function() {
          $("header").find("nav").append(\'<span id="Distributor"style="color:white; font-family:Bodoni MT Black; font-size:20px"> <strong> A FinStat Intelligence<sub><i> by fiscalbeastr </i></sub> </strong></span>\');
        })
       ')),
                    h1(HTML("<strong style = 'font-family:Bodoni MT; color:#6A7758'>Using Multivariate Tail, Copula, and Optimzation Techniques to Decode Asset Performances</strong>")),
                    ## PHASE 1
                    bsCollapsePanel("PHASE 1: Asset Specifications",
                                    tags$p(HTML('Press [Add Asset] to add asset and [Remove Asset] to remove. Please specify the following for each asset: ')),
                                    tags$ul(
                                      tags$li(HTML("<p>Asset ticker</p>")),
                                      tags$li(HTML("<p>Quantity in position</p>")),
                                      tags$li(HTML("<p>Asset market value</p>")),
                                      tags$li(HTML("<p>Date range of interest</p>"))
                                    ),
                                    tags$head(
                                      tags$style(HTML('#add_btn{color: #FFFFFF;
                         background-color: #8A0000}
                         
                         #rm_btn{color: #FFFFFF;
                         background-color: #8A0000}'))
                                    ),
                                    actionGroupButtons(inputId =c("add_btn", "rm_btn"),
                                                       label=list("Add Asset","Remove Asset"),
                                                       status = "danger",
                                                       size = "normal",
                                                       direction = "horizontal",
                                                       fullwidth = FALSE),
                                    textOutput(outputId ="counter"),
                                    textOutput(outputId ="nlv"),
                                    dateRangeInput(inputId ="dateRange", 
                                                   label="Date Range:", 
                                                   start="2018-01-01", 
                                                   end="2021-05-21"),
                                    uiOutput("asset_ui"),
                                    style="warning"),
                    ## PHASE 2
                    bsCollapsePanel("PHASE 2: Exploratory Data Analysis (EDA)",
                                    tabBox(
                                      id="phase2Tab",
                                      width=15,
                                      tabPanel(
                                        "Scatter Matrix",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Scatter Matrix: </strong>a matrix of scatterplots, one for each pair of asset returns. A portfolio is riskier if large negative returns on its assets tend to occur together on the same days.</p>"))
                                          )
                                        ), style="info"),
                                        actionButton("runScatterMatrix",
                                                     class = "btn-danger",
                                                     "Run Scatter Matrix", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                     background-color: #8A0000"),
                                        fluidRow(
                                          column(3,
                                            shinycssloaders::withSpinner(
                                              type = 6, 
                                              color = "#800000", 
                                              size = 0.5,
                                              plotOutput(outputId="scatterMat")
                                            )
                                          )
                                        ),
                                        tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }")
                                      ),
                                      tabPanel(
                                        "Quantile-Quantile Plot(s)",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Quantile-Quantile Plot:</strong> a plot that measures both the empirical and theretical quantiles of asset returns. This plot helps determine whether an asset's return matches a certain probability distribution. After that, the best-matched distribution is selected to build our models. </p>"))
                                          )
                                        ), style="info"),
                                        tags$p(HTML('Please specify the following:')),
                                        tags$ul(
                                          tags$li(HTML("<p>Marginal distribution to use</p>"))),
                                        selectInput(inputId = "qqMethod", 
                                                    label = "Select Marginal Distribution:", 
                                                    choices=c("Student t",
                                                              "Normal")), 
                                        actionButton("runQQPlots",
                                                     class = "btn-danger",
                                                     "Run QQ Plots", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                     background-color: #8A0000"),
                                        shinycssloaders::withSpinner(
                                          type = 6, 
                                          color = "#800000", 
                                          size = 0.5,
                                          plotOutput(outputId="qqPlots")
                                        )
                                      ),
                                      tabPanel(
                                        "Correlation Matrix",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML('<p><strong>Correlation Matrix: </strong>a matrix showing the direction and the strength (on a scale of -1 to 1) between each pair of asset returns.</p>'))
                                          )
                                        ), style="info"),
                                        tags$p(HTML('Please specify the following:')),
                                        tags$ul(
                                          tags$li(HTML("<p>Method of correlation measure</p>"))),
                                        selectInput(inputId = "corrMethod", 
                                                    label = "Select Correlation Method:", 
                                                    choices=c("Spearman"="spearman",
                                                              "Pearson"="pearson")), 
                                        actionButton("runCorrMatrix",
                                                     class = "btn-danger",
                                                     "Run Correlation Matrix", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                     background-color: #8A0000"),
                                        shinycssloaders::withSpinner(
                                          type = 6, 
                                          color = "#800000", 
                                          size = 0.5,
                                          plotOutput(outputId="corrMat")
                                        )
                                      )
                                    ),
                                    style="warning"),
                    ## PHASE 3
                    bsCollapsePanel("PHASE 3: Extreme Value Analysis",
                                    bsCollapsePanel("Definition:", tags$div(
                                      tags$ul(
                                        tags$li(HTML('<p><strong>Value-at-Risk level: </strong> the probability at which your minimum loss shall occur. </p>')),
                                        tags$li(HTML('<p><strong>Value-at-Risk (VaR): </strong> the minimum loss given an extreme risk level, normally 0.05 to 0.1. Relative VaR the proportion of the loss relative to existing capital.</p>')),
                                        tags$li(HTML('<p><strong>Expected Shortfall (ES): </strong> the expected loss amount given that you will lose at least the amount of VaR at an extreme risk level. Relative ES is the proportion of the conditional loss relative to existing capital.</p>'))
                                      )
                                    ), style = "success"),
                                    tags$p(HTML('Please specify the following:')),
                                    tags$ul(
                                      tags$li(HTML("<p>Value-at-Risk (VaR) level</p>"))),
                                    numericInput(inputId = "VaRlevel",
                                                 label = "Value-at-Risk Level (between 0 and 1):", 
                                                 value = 0.01),
                                    tabBox(
                                      id="phase3Tab",
                                      width=15,
                                      tabPanel(
                                        "Multivariate Tails",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML('<strong>Multivariate Analysis: </strong> a set of techniques used for analysis portfolios with more than one asset returns. Multivariate distributions describe the joint behavior of several asset returns.'))
                                          )
                                        ), style="info"),
                                        checkboxGroupInput("multiTailCheck", 
                                                           "Select Multivariate Model(s):",
                                                           inline=TRUE,
                                                           selected=c("Normal",
                                                                      "Student t"),
                                                           c("Normal",
                                                             "Student t")),
                                        actionButton("runMultiTailRiskTable",
                                                     class = "btn-danger",
                                                     "Run Risk Table", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Risk Table",
                                                        verbatimTextOutput("multiTailRiskTableText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          tableOutput("multiTailRiskTable")
                                                        ),
                                                        plotOutput("multiTailRiskBarPlot")
                                        ),
                                        actionButton("runMultiTailBiPlots",
                                                     class = "btn-danger",
                                                     "Run Bivariate ECDF Plots", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Bivariate ECDF Plots",
                                                        verbatimTextOutput("multiTailBiPlotsText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          plotOutput("multiTailBiPlots")
                                                        )
                                        )
                                      ),
                                      tabPanel(
                                        "Copula Tails",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML('<strong>Copulas: </strong> a popular framework for characterizing dependence-and
  only the dependence-between the components of a multivariate distribution; they can be combined with any set of univariate marginal distributions to form a full joint distribution. This framework is typically used in risk assessment and
  management of portfolios that contain assets which exhibit co-movements in extreme behavior. For example, a pair of assets may have weakly correlated returns, but their largest losses may tend to occur in the same periods.'))
                                          )
                                        ), style="info"),
                                        checkboxGroupInput("copTailCheck", 
                                                           "Select Copula Model(s):",
                                                           inline=TRUE,
                                                           selected=c("Normal",
                                                                      "Student t",
                                                                      "Gumbel",
                                                                      "Joe",
                                                                      "Clayton",
                                                                      "Frank"),
                                                           c("Normal",
                                                             "Student t",
                                                             "Gumbel",
                                                             "Joe",
                                                             "Clayton",
                                                             "Frank")),
                                        actionButton("runCopTailRiskTable",
                                                     class = "btn-danger",
                                                     "Run Risk Table", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Risk Table",
                                                        verbatimTextOutput("copTailRiskTableText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          tableOutput("copTailRiskTable")
                                                        ),
                                                        plotOutput("copTailRiskBarPlot")
                                        ),
                                        actionButton("runCopTailBiPlots",
                                                     class = "btn-danger",
                                                     "Run Bivariate ECDF Plots", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Bivariate ECDF Plots",
                                                        verbatimTextOutput("copTailBiPlotsText"),
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          plotOutput("copTailBiPlots")
                                                        )
                                        )
                                      )
                                    ),
                                    style="warning"),
                    bsCollapsePanel("PHASE 4: Portfolio Optimization",
                                    bsCollapsePanel("Definition:", tags$div(
                                      tags$ul(
                                        tags$li(HTML('<p><strong>Daily Expected Return Range: </strong> the range of daily expected return that you want the optimized portfolio to target.</p>')),
                                        tags$li(HTML('<p><strong>U.S. Annual Risk Free Rate: </strong> the annual interest rate at which you can lend  money under no risk of default by the borrower. This rate is normally represented by the 10 year treasury bond yield, or sometimes, the effective federal funds rate.</p>'))
                                      )
                                    ), style="success"),
                                    tags$p(HTML('Please specify the following:')),
                                    tags$ul(
                                      tags$li(HTML("<p>Range of daily expected return under short-sell condition</p>")),
                                      tags$li(HTML("<p>U.S. annual interest rate, or the 10-year U.S. treasury bond yield</p>"))),
                                    
                                    chooseSliderSkin("Nice", color = "#8A0000"),
                                    sliderInput(inputId ="muPrange", 
                                                label ="Range of Daily Expected Return Under Short-sell Condition (between 0 and 0.0001):", 
                                                min = 0, 
                                                max = 0.1, 
                                                step=0.001, 
                                                value = c(0,0.05)),
                                    numericInput(inputId = "riskFreeRate",
                                                 label = "U.S. Annual Risk Free Rate:", 
                                                 value = 0.0174),
                                    tabBox(
                                      id="phase4Tab",
                                      width=50,
                                      tabPanel(
                                        "Optimal Portfolio",
                                        bsCollapsePanel("Description:", tags$div(
                                          tags$ul(
                                            tags$li(HTML("<p><strong>Tangency Portfolio: </strong> this portfolio is estimated to maximize Sharpe's ratio (risk-to-reward ratio). This portfolio is the optimal mix with risk-free assets like Treasury bonds. If this portfolio with short-sell has all asset weights positive, then tangency portfolio is unchanged by the prohibition of short sales.</p>")),
                                            tags$li(HTML('<p><strong>Minimum Variance Portfolio: </strong> this portfolio yields the smallest possible risk. </p>')), 
                                            tags$li(HTML("<p><strong>Efficient Portfolio: </strong> the points on the locus that have expected return at least as large as this portfolio are called the efficient frontier. Portfolios on the efficient frontier are called efficient portfolios. A tangency portfolio forms a point on the efficient frontier with the highest Sharpe's ratio.</p>")),
                                            tags$li(HTML('<p><strong>Short-sell (no short-sell):</strong> a portfolio with (without) asset selling. These selling are denoted by negative asset weights.</p>'))
                                          )
                                        ), style = "info"),
                                        checkboxGroupInput("shortSellCheck", 
                                                           "Select Short-sell Type(s):",
                                                           inline=TRUE,
                                                           selected=c("Short-sell",
                                                                      "No Short-sell"),
                                                           c("Short-sell",
                                                             "No Short-sell")),
                                        actionButton("runOptPortAllocTable",
                                                     class = "btn-danger",
                                                     "Run Optimal Portfolio Table", 
                                                     width = "200px",
                                                     style="color: #FFFFFF;
                                      background-color: #8A0000"),
                                        bsCollapsePanel("Portfolio Allocation Table",
                                                        shinycssloaders::withSpinner(
                                                          type = 6, 
                                                          color = "#800000", 
                                                          size = 0.5,
                                                          tableOutput("optPortAllocTable")
                                                        ),
                                                        plotOutput("optPortAllocBarPlot"),
                                                        plotOutput("RRPlots")
                                        )
                                      )
                                    ),
                                    style="warning"),
                  style="danger")
                )
  )
)
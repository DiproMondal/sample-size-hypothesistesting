source("ssfuns.R", local = TRUE, echo = FALSE)
if (!requireNamespace("bslib", quietly = TRUE)) {
  install.packages("bslib")
}
library(bslib)
ui <- navbarPage(
  title = em("Sample Size App"),
  tabsetPanel(id="tabs",type = "tabs",
              tabPanel(title = strong("Choice of confidence interval method"),
                       fluidRow(
                         column(4, offset = 2, h1("Choose method"),
                                radioButtons("CIMet", label = NULL,
                                             choices = c("Delta method and matrix formulation",
                                                         "Modified large sample based on a transformation of the ICC",
                                                         "Modified large sample based on ratios of variance components", 
                                                         "Generalized confidence interval",
                                                         "Variance partitioning confidence interval using an F-distribution",
                                                         "Variance partitioning confidence interval using a beta-distribution"),
                                             selected = "Variance partitioning confidence interval using an F-distribution")
                         )
                       ),## close fluidrow
                       fluidRow(
                         column(6, offset = 2, h2("Description"),
                                mainPanel(
                                  conditionalPanel(
                                    condition = "input.CIMet=='Delta method and matrix formulation'",
                                    p("This is the confidence interval based on delta method with matrix formulation (Wmat) introduced by",
                                      HTML("<a href='https://www.tandfonline.com/doi/full/10.1080/03610918.2021.1897624'>Almehrizi and Emam</a>"), "."),
                                    p("Sample size for this method is based on closed form expression of the power function given in Mondal et al.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.CIMet=='Modified large sample based on a transformation of the ICC'",
                                    p("This is the modified large sample confidence interval based on ratios of variance components (MLSA) introduced by",
                                      HTML("<a href='https://www.tandfonline.com/doi/pdf/10.1080/03610928208828338'>Arteaga et al.</a>"), "."),
                                    p("Sample size for this method is based on simulations following Mondal et al.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.CIMet=='Modified large sample based on ratios of variance components'",
                                    p("This is the modified large sample confidence interval based on ratios of variance components (MLSG) introduced by",
                                      HTML("<a href='https://doi.org/10.1002/sim.1402'>Cappelleri and Ting</a>"), "."),
                                    p("Sample size for this method is based on simulations following Mondal et al.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.CIMet=='Generalized confidence interval'",
                                    p("This is the generalized confidence interval (GCI) introduced by",
                                      HTML("<a href='https://doi.org/10.1002/sim.1782'>Tian and Cappelleri</a>"), "."),
                                    p("Sample size for this method is based on simulations following Mondal et al.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.CIMet=='Variance partitioning confidence interval using an F-distribution'",
                                    p("This is based on the variance partitioning confidence interval using an F-distribution (VPF) introduced by",
                                      HTML("<a href='https://doi.org/10.1177/0962280214522787'>Demetrashvili et al.</a>"), "."),
                                    p("Sample size for this method is based on closed form expression of the power function given in Mondal et al.")
                                  ),
                                  conditionalPanel(
                                    condition = "input.CIMet=='Variance partitioning confidence interval using a beta-distribution'",
                                    p("This is based on the variance partitioning confidence interval using an beta-distribution (VPB) introduced by",
                                      HTML("<a href='https://doi.org/10.1177/0962280214522787'>Demetrashvili et al.</a>"), "."),
                                    p("Sample size for this method is based on simulations following Mondal et al.")
                                  )
                                )
                         )
                       ) ## close column for fluidrow Description
              ), ## close tabPanel
              tabPanel(title = strong("Sample Size Calculation"),
                       sidebarLayout(
                         sidebarPanel(
                           width = 4,
                           h3("General Parameters"),
                           numericInput("k", label = "Number of raters:",
                                        min = 2, max = 100, step = 1, value = 5),
                           sliderInput("rhoA", label = "Value for ICC for agreement under alternative hypothesis:",
                                       min = 0.05, max = 0.99, step = 0.01, value = 0.8),
                           sliderInput("rho0", label = "Value for ICC for agreement under null hypothesis:",
                                       min = 0.05, max = 0.95, step = 0.01, value = 0.7),
                           numericInput("R", label = "Rater to error variance ratio",
                                        min = 0.001, max = 100, step = 0.001, value = 0.1),
                           sliderInput("power", label = "Target power for the hypothesis test",
                                       min = 0.1, max = 0.9, step = 0.01, value = 0.8),
                           sliderInput("alpha", label = "Confidence level",
                                       min = 0.8, max = 0.99, step = 0.01, value = 0.95),
                           numericInput("nmin", label = "Acceptable minimum number of participants",
                                        min = 4, max = 1000, step = 1, value = 4),
                           numericInput("nmax", label = "Acceptable maximum number of participants",
                                        min = 10, max = 10000, step = 1, value = 1000),
                           actionButton("computeSS", "Calculate Sample Size")
                         ),
                         mainPanel(
                           conditionalPanel(
                             condition = "input.CIMet=='Modified large sample based on a transformation of the ICC'",
                             numericInput("nsimMLSA", label = "Number of simulations to calculate the power:",
                                          min = 100, max=1e6, step=10, value = 1e3),
                             numericInput("repeatsMLSA", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           conditionalPanel(
                             condition = "input.CIMet=='Modified large sample based on ratios of variance components'",
                             numericInput("nsimMLSG", label = "Number of simulations to calculate the power:",
                                          min = 100, max=1e6, step=10, value = 1e3),
                             numericInput("repeatsMLSG", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           conditionalPanel(
                             condition = "input.CIMet=='Variance partitioning confidence interval using a beta-distribution'",
                             numericInput("nsimVPB", label = "Number of simulations to calculate the power:",
                                          min = 100, max=1e6, step=10, value = 1e3),
                             numericInput("repeatsVPB", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           conditionalPanel(
                             condition = "input.CIMet=='Generalized confidence interval'",
                             numericInput("nsimGCIW", label = "Number of simulations to calculate confidence interval (using Rao-Blackwellization):",
                                          min = 100, max=1e6, step=10, value = 1e2),
                             numericInput("nsimGCIO", label = "Number of simulations to calculate the power:",
                                          min = 100, max=1e6, step=10, value = 1e2),
                             numericInput("repeatsGCI", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           fluidRow(
                             uiOutput("SampTable"),
                             column(6, offset = 2,
                                    div(textOutput("Tmps"), style = "color:red")
                             ),
                             column(6, offset =2,
                                    plotOutput("sampleSizePlot")
                             )
                           )
                         )),

              ), ## close tabPanel parameter choices
             ####### tabPanel(title = strong("Sample Sizes"),

                
            #######  ), ## close tabPanel Sample Sizes
              tabPanel(title = strong("Simulation"),
                       sidebarLayout(
                         sidebarPanel("Parameter Specification",
                                      numericInput("np", label = HTML("Number of participants (n):"),
                                                   min = 10, max=1e6, step=1, value = 20),
                                      numericInput("nk", label = HTML("Number of raters (k):"),
                                                   min = 2, max = 100, step=1, value=3),
                                      numericInput("R1", label = HTML("Rater to error variance ratio (R)"),
                                                   min = 0.001, max = 100, step = 0.001, value = 0.1),
                                      sliderInput("rho1", label = HTML("Value for ICC for agreement (&rho;):"),
                                                  min = 0.05, max = 0.99, step = 0.01, value = 0.8),
                                      numericInput("nsims", label = "Number of simulations:",
                                                   min = 100, max=1e6, step=100, value = 1e4),
                                      sliderInput("alpha1", label = "Confidence level",
                                                  min = 0.8, max = 0.99, step = 0.01, value = 0.95),
                                      radioButtons("CIMet1", label = NULL,
                                                   choices = c("Delta method and matrix formulation",
                                                               "Asymptotic confidence interval",
                                                               "Modified large sample based on a transformation of the ICC",
                                                               "Modified large sample based on ratios of variance components", 
                                                               "Generalized confidence interval",
                                                               "Variance partitioning confidence interval using an F-distribution",
                                                               "Variance partitioning confidence interval using a beta-distribution"),
                                                   selected = "Modified large sample based on ratios of variance components"),
                                      numericInput("seed", label = "Seed:",
                                                   min = 1, max=1e7, step=1, value = 124),
                                      actionButton("Simulate", "Simulate")
                         ),
                         mainPanel(
                           fluidRow(
                             uiOutput("Qprint"),
                             column(6,
                                    plotOutput("ICCest")
                             ),
                             column(6,
                                    plotOutput("Lows")
                             ),
                             uiOutput("collapsibleContent")
                         ) 
                       ))
                       
              ),
            tabPanel(
              title = strong("Help"),
              mainPanel(h2("Steps to determine sample size"),
                        HTML("
      <ul>
        <li><i><b>Step 1 </b></i> - Select an appropriate confidence interval method under 'Choose Method' in the <u><b>'Choice of confidence interval method'</b></u>. Use the <i><b>table</i></b> below as a guide for choosing the confidence interval method. </li>
        <li><i><b>Step 2 </b></i> - Specify the maximum number of raters available for the study under 'Number of Raters' in <u><b>'Sample Size Calculation'</u></b>.</li>
        <li><i><b>Step 3 </b></i> - Set the expected value for the ICC for agreement under the null and alternative hypothesis under 'Value for ICC for agreement under null hypothesis' and 'Value for ICC for agreement under alternative hypothesis' respectively.</li>
        <li><i><b>Step 4 </b></i> - Set the rater to error variance ratio under 'Rater to error variance ratio'.<i>Note that this value should be used for selecting the confidence interval method.</i></li>
        <li><i><b>Step 5 </b></i> - Set the target power for the hypothesis test under 'Target power for the hypothesis test'.</li>
        <li><i><b>Step 6 </b></i> - Set required confidence level under 'Confidence level'.</li>
        <li><i>(Optional) <b>Step 7 </b></i> -  Set the minimum and maximum  number of participants for the study under 'Acceptable minimum number of participants' and 'Acceptable maximum number of participants', respectively.</li>
      </ul>
    ")),
              mainPanel(HTML("
<table border='1' style='border-collapse: collapse; width: 100%;'>
  <tr>
    <th>Settings</th>
    <th>Recommended confidence interval methods</th>
  </tr>
  <tr>
    <td>R ≤ 0.1, 2 ≤ k ≤ 4</td>
    <td><i>VP<sub>B</sub></i>, <i>VP<sub>F</sub></i>, <i>W<sub>mat</sub></i></td>
  </tr>
  <tr>
    <td>R ≤ 0.1, k ≥ 5</td>
    <td><i>VP<sub>B</sub></i>, <i>W<sub>mat</sub></i></td>
  </tr>
  <tr>
    <td>0.1 < R ≤ 0.2, 2 ≤ k ≤ 4</td>
    <td><i>MLS<sub>A</sub></i></td>
  </tr>
  <tr>
    <td>0.1 < R ≤ 0.2, 4 < k ≤ 8</td>
    <td><i>MLS<sub>G</sub></i></td>
  </tr>
  <tr>
    <td>0.1 < R ≤ 0.2, k ≥ 8</td>
    <td><i>MLS<sub>G</sub></i>, <i>W<sub>mat</sub></i></td>
  </tr>
  <tr>
    <td>0.2 < R ≤ 0.6, 2 ≤ k ≤ 4</td>
    <td><i>MLS<sub>G</sub></i></td>
  </tr>
  <tr>
    <td>0.2 < R ≤ 0.6, k ≥ 5</td>
    <td><i>MLS<sub>G</sub></i>, <i>GCI</i></td>
  </tr>
  <tr>
    <td>R > 0.6, k > 2</td>
    <td><i>MLS<sub>A</sub></i>, <i>MLS<sub>G</sub></i>, <i>GCI</i></td>
  </tr>
</table>
<br>
<strong>Note:</strong> 'R' is the value of rater-to-error variance ratio, and 'k' is the maximum number of raters available for the study. <br>
<i>W<sub>mat</sub></i>: Delta method and matrix formulation <br>
<i>MLS<sub>A</sub></i>: Modified large sample based on a transformation of the ICC <br>
<i>MLS<sub>G</sub></i>: Modified large sample based on ratios of variance components <br>
<i>GCI</i>: Generalized confidence interval <br>
<i>VP<sub>F</sub></i>: Variance partitioning confidence interval using an F-distribution <br>
<i>VP<sub>B</sub></i>: Variance partitioning confidence interval using a beta-distribution
")),
              mainPanel(h2("Steps to keep in mind"),HTML("
      <ul>
        <li><h4>Generalized Confidence Interval:</h4><p>The construction of confidence interval method <i>GCI</i> uses simulations.
        Therefore, it is time consuming. The sample size determination using <i>GCI</i> requires simulations on top of these simulations.
        So, two sets of simulation parameters need to be defined, to be specified under 'Number of simulations to calculate confidence interval (using Rao-Blackwellization)',
        and 'Number of simulations to calculate the power', to calculate the confidence interval and to determine sample size, respectively.</p></li>
        <li><h4>Maximum number of participants</h4><p>If the estimate of the sample size procedure returns the maximum number of participants
        defined under 'Acceptable maximum number of participants', please increase the value of that parameter setting and rerun the procedure.</p></li>
        <li><h4>Stability Issues:</h4><p>Sample sizes using confidence interval methods MLS<sub>A</sub>, MLS<sub>G</sub>, GCI, and VP<sub>F</sub> are simulation based.
        The number of simulations dictate the accuracy of the sample size determination procedure. Therefore, it is recommended to repeat the sample size
        procedure using atleast 1000 simulations several times to see the variance of the resulting sample size estimates.</p><p>If the variance of the sample size
        estimates increases when increasing the number of repeats, a sample size for the parameter settings may not be possible. It is then recommended to 
        change parameter settings. In other cases, a sample siz is possible and the final estimate can be obtained using a larger number of simulations, for example,
        using 10,000 simulations.</p></li>
      </ul>
    "))
            )
  ) ## close tabsetPanel
) ## close navbarpage

server <- function(input,output,session) {
  res2 <- reactiveVal(NULL)

  observeEvent(input$computeSS, {
    sample_size_calculation <- function(){
      alpha  <- 1 - as.numeric(input$alpha)
      rhoA   <- as.numeric(input$rhoA)
      rho0   <- as.numeric(input$rho0)
      R      <- as.numeric(input$R)
      power  <- as.numeric(input$power)
      method <- as.character(input$CIMet)
      k      <- as.numeric(input$k) 
      nmin   <- as.numeric(input$nmin)
      nmax   <- as.numeric(input$nmax)
      mult   <- reactiveVal()
      

      if(input$CIMet == 'Delta method and matrix formulation'){
        mult(FALSE)
        SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
          SampleSize(k      = k,
                         rho    = rhoA,
                         rho.0  = rho0,
                         R      = R,
                         power  = power,
                         n_max  = nmax,
                         n_min  = nmin,
                         alpha  = alpha,
                         method = "Wmat"
        )[['bisection']][['final']]
          })
        
      }else if(input$CIMet == 'Modified large sample based on ratios of variance components'){
        if(as.numeric(input$repeatsMLSG)==1){
          mult(FALSE)
          SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
            SampleSize(k      = k,
                           rho    = rhoA,
                           rho.0  = rho0,
                           R      = R,
                           power  = power,
                           n_max  = nmax,
                           n_min  = nmin,
                           alpha  = alpha,
                           method = "MLSG",
                           nsim   = as.numeric(input$nsimMLSG)
          )[['bisection']][['final']]
            })
        }else{
          mult(TRUE)
          SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
            SampleSize.wrap(k      = k,
                                rho    = rhoA,
                                rho.0  = rho0,
                                R      = R,
                                power  = power,
                                n_max  = nmax,
                                n_min  = nmin,
                                alpha  = alpha,
                                method = "MLSG",
                                nsim   = as.numeric(input$nsimMLSG),
                                reps   = as.numeric(input$repeatsMLSG) 
          )})
        }
        }else if(input$CIMet == 'Modified large sample based on a transformation of the ICC'){
          if(as.numeric(input$repeatsMLSA)==1){
            mult(FALSE)
            SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
              SampleSize(k      = k,
                             rho    = rhoA,
                             rho.0  = rho0,
                             R      = R,
                             power  = power,
                             n_max  = nmax,
                             n_min  = nmin,
                             alpha  = alpha,
                             method = "MLSA",
                             nsim   = as.numeric(input$nsimMLSA)
            )[['bisection']][['final']]})
          }else{
            mult(TRUE)
            SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
              SampleSize.wrap(k      = k,
                                  rho    = rhoA,
                                  rho.0  = rho0,
                                  R      = R,
                                  power  = power,
                                  n_max  = nmax,
                                  n_min  = nmin,
                                  alpha  = alpha,
                                  method = "MLSA",
                                  nsim   = as.numeric(input$nsimMLSA),
                                  reps   = as.numeric(input$repeatsMLSA) 
            )})
          }
        
      }else if(input$CIMet == 'Generalized confidence interval'){
        if(as.numeric(input$repeatsGCI)==1){
          mult(FALSE)
          SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
            SampleSize(k      = k,
                           rho    = rhoA,
                           rho.0  = rho0,
                           R      = R,
                           power  = power,
                           n_max  = nmax,
                           n_min  = nmin,
                           alpha  = alpha,
                           method = "GCI",
                           nsim   = as.numeric(input$nsimGCIO),
                           nsimW  = as.numeric(input$nsimGCIW) 
          )[['bisection']][['final']]
            })
        }else{
          mult(TRUE)
          SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
            SampleSize.wrap(k      = k,
                                rho    = rhoA,
                                rho.0  = rho0,
                                R      = R,
                                power  = power,
                                n_max  = nmax,
                                n_min  = nmin,
                                alpha  = alpha,
                                method = "GCI",
                                nsim   = as.numeric(input$nsimGCIO),
                                nsimW  = as.numeric(input$nsimGCIW), 
                                reps   = as.numeric(input$repeatsGCI) 
          )})
        }
        
      }else if(input$CIMet == 'Variance partitioning confidence interval using an F-distribution'){
        mult(FALSE)
        SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
          SampleSize(k      = k,
                         rho    = rhoA,
                         rho.0  = rho0,
                         R      = R,
                         power  = power,
                         n_max  = nmax,
                         n_min  = nmin,
                         alpha  = alpha,
                         method = "VPF"
        )[['bisection']][['final']]
        })
        
      }else if(input$CIMet == 'Variance partitioning confidence interval using a beta-distribution'){
        if(as.numeric(input$repeatsVPB)==1){
          mult(FALSE)
          SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
            SampleSize(k      = k,
                           rho    = rhoA,
                           rho.0  = rho0,
                           R      = R,
                           power  = power,
                           n_max  = nmax,
                           n_min  = nmin,
                           alpha  = alpha,
                           method = "VPB",
                           nsim   = as.numeric(input$nsimVPB)
                           )[['bisection']][['final']]
          })
        }else{
          mult(TRUE)
          SS <- withProgress(message = 'Computing', style = 'notification', value = 0, {
            SampleSize.wrap(k      = k,
                                rho    = rhoA,
                                rho.0  = rho0,
                                R      = R,
                                power  = power,
                                n_max  = nmax,
                                n_min  = nmin,
                                alpha  = alpha,
                                method = "VPB",
                                nsim   = as.numeric(input$nsimVPB),
                                reps   = as.numeric(input$repeatsVPB) 
          )})
          
        }
        
      }

      output$SampTable <- renderUI({
        column(6, offset =2, 
             h3("Sample Size Table"),
             tableOutput("sampleSizeTable")
      )})


      return(list(SS   = SS,
                  SSmode = if(mult()=="TRUE"){
                    #as.numeric(names(sort(table(SS), decreasing = TRUE))[1]) ## Mode
                    sum(sapply(1:length(table(SS)), function(x) table(SS)[[x]]*as.integer(names(table(SS))[x])))/sum(table(SS))## Weighted mean
                  }else{
                    SS
                    },
                  rhoA = rhoA,
                  rho0 = rho0,
                  R    = R,
                  k    = k,
                  power=power,
                  method = method,
                  mult = mult))
    }
    
    res <- sample_size_calculation()
    
    output$sampleSizeTable <- renderTable({ 
      table_data <- data.frame(
        "&#961;A" = res$rhoA,
        "&#961;0" = res$rho0,
        "R" = res$R,
        "Power" = res$power,
        "n" = as.integer(res$SSmode),
        "k" = as.integer(res$k),
        check.names = FALSE
      )
      
      if (res$mult() == "TRUE") {
        table_data[['Variance(n)']]<- var(res$SS, na.rm = TRUE)
      }
      
      table_data
    },
    sanitize.text.function = function(x) x,
    rownames = FALSE)
    
    output$Tmps <- renderText({
      if (res$SSmode == as.numeric(input$nmax)) {
        "Warning! Sample size within supplied search range for the number of participants is not possible."
      }
    })
    
    output$sampleSizePlot <- renderPlot({
      if (res$mult() == "TRUE") {
        hist(res$SS, main = "Distribution of Sample Sizes",
             xlab = "Sample Sizes", border = "white")
      }
    })
  })
  

  
  
  observeEvent(input$Simulate, {
    output$Qprint <- renderUI({
      column(6, offset =2, 
         h3("Estimated Quantities"),
         tableOutput("Ests"),
         bsTooltip(id = "rho",  "ICC for agreement (specified value)", placement = "right"),
         bsTooltip(id = "RD", "Ratio of rater to error variance (specified value)", placement = "right"),
         bsTooltip(id = "MSS",  "Mean Squares between participants", placement = "right"),
         bsTooltip(id = "MSR", "Mean Squares between raters", placement = "right"),
         bsTooltip(id = "MSE", "Mean Square Error", placement = "right"),
         bsTooltip(id = "ICC", "Estimated ICC for agreement", placement = "right"),
         bsTooltip(id = "Rh", "Estimated ratio of rater to error variance", placement = "right"))
    })
    output$collapsibleContent <- renderUI({
        tags$div(
          class = "panel panel-default", # Bootstrap panel styling
          tags$div(
            class = "panel-heading",
            tags$a(
              class = "btn btn-primary", # Makes it look like a button
              "data-toggle" = "collapse",
              "href" = "#collapseTabsetPanel", # Link to the collapsible content
              "Additional quantities for hypothesis testing"
            )
          ),
          tags$div(
            id = "collapseTabsetPanel",
            class = "panel-collapse collapse", # Collapsible content class
            tags$div(
              class = "panel-body",
              tabsetPanel(
                column(7, h4(HTML(
                  "For hypothesis testing on the ICC for agreement (&rho;): \\( \\mathcal{H}_0 : \\rho = \\rho_0 \\) vs \\( \\mathcal{H}_A : \\rho \\geq \\rho_0 \\)"
                ), withMathJax()),
                h5(HTML("(&rho;<sub>0</sub> and &rho;<sub>A</sub> are values of ICC for agreement under \\(\\mathcal{H}_0\\) and \\(\\mathcal{H}_A\\))"), withMathJax())),
                column(6, offset = 1, h4(HTML("Empirical Coverage")),
                       h5(HTML("Consider the specified ICC for agreement is the value under null hypothesis (&rho;<sub>0</sub>)")),
                       tableOutput("CvrEst")),
                column(6, offset = 1, h4(HTML("Empirical Power")),
                       h5(HTML("Consider the specified ICC for agreement is the value under alternate hypothesis (&rho;<sub>A</sub>)")),
                       sliderInput("rho0sld", label = "Value under null hypothesis:", min = 0.05, max = 0.99, step = 0.01, value = 0.8),
                       tableOutput("PowerEst"))
              )
            )
          )
        )
    })
    MC_sim <- function(){
      n <- as.numeric(input$np)
      k <- as.numeric(input$nk)
      R <- as.numeric(input$R1)
      rho <- as.numeric(input$rho1)
      nsims <- as.numeric(input$nsims)
      met <- as.character(input$CIMet1)
      seed  <- as.numeric(input$seed)
      alpha <- 1-as.numeric(input$alpha1)
      
      metdstxt <-  c("Delta method and matrix formulation",
                     "Asymptotic confidence interval",
                     "Modified large sample based on a transformation of the ICC",
                     "Modified large sample based on ratios of variance components", 
                     "Generalized confidence interval",
                     "Variance partitioning confidence interval using an F-distribution",
                     "Variance partitioning confidence interval using a beta-distribution")
      metds <- c("Wmat", "ASN", "MLSA", "MLSG", "GCI", "VPF", "VPB")
      metdsmap <- setNames(metds, metdstxt)
      
      CIMet <- metdsmap[[met]]
      
      
      
      
      gm  <- gen(n = n,
                 k = k,
                 rho = rho,
                 R   = R,
                 nsim = nsims)
      sim <- 
        withProgress(message = 'Computing', style = 'notification', value = 0, {
        MC_SimL(n = n,
              k = k,
              rho = rho,
              R   = R,
              method = CIMet,
              nsim   = nsims,
              alpha  = alpha,
              seed   = seed)
        })
      coverage <-
        round(length(sim[sim<rho])/nsims,3)
      
      print(coverage)
      
      return(list("ICC"= gm$ICC.agree,
                  "MSS"= gm$MS.subj,
                  "MSR"= gm$MS.ratr,
                  "MSE"= gm$MS.errr,
                  "Lws" = sim,
                  "CV"  = coverage,
                  "method"=CIMet))
    }
    res2(MC_sim())
    
    #output$Ests <- renderTable({ 
    #  data.frame(
    #    "&#961;<span title='ICC for agreement (specified value)'>[?]</span>" = as.numeric(input$rho1),
    #    "R<span title='Ratio of rater to error variance (specified value)'>[?]</span>" = as.numeric(input$R),
    #    "E[MSS]<span title='Mean Squares between participants'>[?]</span>" = mean(res2()$MSS),
    #    "E[MSR]<span title='Mean Squares between raters'>[?]</span>" = mean(res2()$MSR),
    #    "E[MSE]<span title='Mean Square Error'>[?]</span>" = mean(res2()$MSE),
    #    "E[&#710;&#961]<span title='Estimated ICC for agreement (two-way ANOVA without repititions)'>[?]</span>" = mean(res2()$ICC),
    #    check.names = FALSE
    #  )
    #},
    #escape = FALSE,
    #sanitize.text.function = identity,
    #rownames = FALSE)
    output$Ests <- renderUI({
      HTML("
      <table border='1' style='border-collapse: collapse; width: 100%;'>
        <tr>
          <th id='rho'>&#961;</th>
          <th id='RD'>R</th>
          <th id='MSS'>E[MSS]</th>
          <th id='MSR'>E[MSR]</th>
          <th id='MSE'>E[MSE]</th>
          <th id='ICC'>E[&#710;&#961;]</th>
          <th id='Rh'>E[&#710;R]</th>
        </tr>
        <tr>
          <td>" , as.numeric(input$rho1), "</td>
          <td>" , as.numeric(input$R), "</td>
          <td>" , round(mean(res2()$MSS),3), "</td>
          <td>" , round(mean(res2()$MSR),3), "</td>
          <td>" , round(mean(res2()$MSE),3), "</td>
          <td>" , round(mean(res2()$ICC),3), "</td>
          <td>" , round((mean(res2()$MSR)-mean(res2()$MSE))/as.numeric(input$np)/mean(res2()$MSR),3), "</td>
        </tr>
      </table>
    ")
    })
    
    
    output$ICCest <- renderPlot({
      hist(res2()$ICC, breaks = as.integer(input$nsims/50), 
           main ="Distribution of ICC estimate",
           xlab = "ICC estimate",
           border='grey')
    })
    output$Lows <- renderPlot({
      hist(res2()$Lws, breaks = as.integer(input$nsims/50), 
           main ="Distribution of the lower limit", 
           xlab = paste0("Lower limit of confidence interval", 
                         " (",as.character(res2()$method), ")"),
           border='grey')
    })
    
    output$CvrEst <- renderTable({ 
      data.frame("&#961;0"=as.numeric(input$rho1),
                 "Coverage" = res2()$CV,
                 "Type-I Error" = 1-res2()$CV,
                 check.names = FALSE)
    },
    sanitize.text.function = function(x) x,
    rownames = FALSE)
    
  })
  powmean <- reactive({
    rh0 <- as.numeric(input$rho0sld)
    
    if (is.null(res2())) {
      return(NA)
    } else {
      return(mean(sapply(res2()$Lws, function(x) x > rh0)))
    }
  })

    
    output$PowerEst <- renderTable({ 
      data.frame("&#961;A"=as.numeric(input$rho1),
                 "&#961;0"=as.numeric(input$rho0sld),
                 "Power" = powmean(),
                 check.names = FALSE)
    },
    sanitize.text.function = function(x) x,
    rownames = FALSE)
  
  }

shinyApp(ui, server)
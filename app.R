source("ssfuns.R", local = TRUE, echo = FALSE)

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
                           numericInput("R", label = "Rater to error variance ratio",
                                        min = 0.001, max = 100, step = 0.001, value = 0.1),
                           actionButton("computeSS", "Calculate Sample Size")
                         ),
                         mainPanel(
                           conditionalPanel(
                             condition = "input.CIMet=='Modified large sample based on a transformation of the ICC'",
                             numericInput("nsimMLSA", label = "Number of simulations to calculate the power:",
                                          min = 10, max=1e6, step=10, value = 1e3),
                             numericInput("repeatsMLSA", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           conditionalPanel(
                             condition = "input.CIMet=='Modified large sample based on ratios of variance components'",
                             numericInput("nsimMLSG", label = "Number of simulations to calculate the power:",
                                          min = 10, max=1e6, step=10, value = 1e3),
                             numericInput("repeatsMLSG", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           conditionalPanel(
                             condition = "input.CIMet=='Variance partitioning confidence interval using a beta-distribution'",
                             numericInput("nsimVPB", label = "Number of simulations to calculate the power:",
                                          min = 10, max=1e6, step=10, value = 1e3),
                             numericInput("repeatsVPB", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           conditionalPanel(
                             condition = "input.CIMet=='Generalized confidence interval'",
                             numericInput("nsimGCIW", label = "Number of simulations to calculate confidence interval (using Rao-Blackwellization):",
                                          min = 10, max=1e6, step=10, value = 1e2),
                             numericInput("nsimGCIO", label = "Number of simulations to calculate the power:",
                                          min = 10, max=1e6, step=10, value = 1e2),
                             numericInput("repeatsGCI", label = "Repeat the sample size procedure several times:",
                                          min = 10, max=1e4, step=1, value = 10),
                           ),
                           fluidRow(
                             column(6, offset =2, 
                                    h3("Sample Size Table"),
                                    tableOutput("sampleSizeTable")
                             ),
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
                                      numericInput("np", label = "Number of participants:",
                                                   min = 10, max=1e6, step=1, value = 20),
                                      numericInput("nk", label = "Number of raters:",
                                                   min = 2, max = 100, step=1, value=3),
                                      numericInput("R1", label = "Rater to error variance ratio",
                                                   min = 0.001, max = 100, step = 0.001, value = 0.1),
                                      sliderInput("rho1", label = "Value for ICC for agreement:",
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
                             column(6, offset =2, 
                                    h3("Estimated Quantities"),
                                    tableOutput("Ests")
                             ),
                             column(6,
                                    plotOutput("ICCest")
                             ),
                             column(6,
                                    plotOutput("Lows")
                             ),
                             column(5, offset =2,
                                    h3(HTML("Empirical Power (&rho;<sub>A</sub>=&rho;)")),
                                    sliderInput("rho0sld", label = "Value under null hypothesis:",
                                                min = 0.05, max = 0.99, step = 0.01, value = 0.8),
                                    tableOutput("PowerEst")
                             ),
                             column(5, offset =2,
                                    h3(HTML("Empirical Power (&rho;<sub>0</sub>=&rho;)")),
                                    tableOutput("CvrEst")
                             ),
                         ) 
                       ))
                       
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
      mult   <- FALSE

      if(input$CIMet == 'Delta method and matrix formulation'){
        SS <- SampleSize(k      = k,
                         rho    = rhoA,
                         rho.0  = rho0,
                         R      = R,
                         power  = power,
                         n_max  = nmax,
                         n_min  = nmin,
                         alpha  = alpha,
                         method = "Wmat"
        )[['bisection']][['final']]
        
      }else if(input$CIMet == 'Modified large sample based on ratios of variance components'){
        if(as.numeric(input$repeatsMLSG)==1){
          SS <- SampleSize(k      = k,
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
        }else{
          mult <- TRUE
          SS <- SampleSize.wrap(k      = k,
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
          )
        }
        }else if(input$CIMet == 'Modified large sample based on a transformation of the ICC'){
          if(as.numeric(input$repeatsMLSA)==1){
            SS <- SampleSize(k      = k,
                             rho    = rhoA,
                             rho.0  = rho0,
                             R      = R,
                             power  = power,
                             n_max  = nmax,
                             n_min  = nmin,
                             alpha  = alpha,
                             method = "MLSA",
                             nsim   = as.numeric(input$nsimMLSA)
            )[['bisection']][['final']]
          }else{
            mult <- TRUE
            SS <- SampleSize.wrap(k      = k,
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
            )
          }
        
      }else if(input$CIMet == 'Generalized confidence interval'){
        if(as.numeric(input$repeatsGCI)==1){
          SS <- SampleSize(k      = k,
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
        }else{
          mult <- TRUE
          SS <- SampleSize.wrap(k      = k,
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
          )
        }
        
      }else if(input$CIMet == 'Variance partitioning confidence interval using an F-distribution'){
        SS <- SampleSize(k      = k,
                         rho    = rhoA,
                         rho.0  = rho0,
                         R      = R,
                         power  = power,
                         n_max  = nmax,
                         n_min  = nmin,
                         alpha  = alpha,
                         method = "VPF"
        )[['bisection']][['final']]
        
      }else if(input$CIMet == 'Variance partitioning confidence interval using a beta-distribution'){
        if(as.numeric(input$repeatsVPB)==1){
          SS <- SampleSize(k      = k,
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
        }else{
          mult <- TRUE
          SS <- SampleSize.wrap(k      = k,
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
          )
          
        }
        
      }

      print(SS)
      return(list(SS   = SS,
                  SSmode = ifelse(mult==TRUE,
                                  as.numeric(names(sort(table(SS), decreasing = TRUE))[1]),
                                  SS),
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
      data.frame("&#961;A"=res$rhoA,
                 "&#961;0"=res$rho0,
                 "R"=res$R,
                 "Power"=res$power,
                 "n"=as.integer(res$SSmode),
                 "k"=as.integer(res$k),
                 check.names = FALSE)
    },
    sanitize.text.function = function(x) x,
    rownames = FALSE)
    
    output$Tmps <- renderText({
      if (res$SS == as.numeric(input$nmax)) {
        "Warning! Sample size within supplied search range for the number of participants is not possible."
      }
    })
    
    output$sampleSizePlot <- renderPlot({
      if (res$mult == TRUE) {
        hist(res$SS, main = "Distribution of Sample Sizes",
             xlab = "Sample Sizes", border = "white")
      }
    })
  })
  
  observeEvent(input$Simulate, {
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
        MC_SimL(n = n,
              k = k,
              rho = rho,
              R   = R,
              method = CIMet,
              nsim   = nsims,
              alpha  = alpha,
              seed   = seed)
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
    
    output$Ests <- renderTable({ 
      data.frame(
        "&#961;<span title='ICC for agreement (true value)'>[?]</span>" = as.numeric(input$rho1),
        "R<span title='Ratio of rater to error variance (true value)'>[?]</span>" = as.numeric(input$R),
        "E[MSS]<span title='Mean Squares between participants'>[?]</span>" = mean(res2()$MSS),
        "E[MSR]<span title='Mean Squares between raters'> [?]</span>" = mean(res2()$MSR),
        "E[MSE]<span title='Mean Square Error'>[?]</span>" = mean(res2()$MSE),
        "E[&#710;&#961]<span title='Estimated ICC for agreement (two-way ANOVA without repititions)'>[?]</span>" = mean(res2()$ICC),
        check.names = FALSE
      )
    },
    sanitize.text.function = function(x) x,
    rownames = FALSE)
    
    
    
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
library(readxl)
library(dplyr)
library(lubridate)
library(Jmisc)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(cowplot)
library(scales)
library(shiny)
library(shinyWidgets)
library(plotly)
library("survival")
library(shinycssloaders)
library("openair")
library(randomForest)
#library(foreach)


#setwd("/Users/amin/Desktop/PhD/covid-19")

source('seir.R')
source('utils.R')
rm(list = ls())

ui <- fluidPage(
  
  titlePanel("How long will COVID-19 last in Ireland?"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "R0 - average number of infected people for each infected person", 0, 10, 0.8, step=0.1),
               
               #sliderInput("E", "Mean holding times at compartment E", 0, 20, 6.6, step=0.2, post = " days"),
               
               #sliderInput("I", "Mean holding times at compartment I", 0, 20, 7.4, step=0.2, post = " days"),
               
               
               
               numericInput("pop","Current number of people who can still get COVID-19",value = 4.9E6),
               
               numericInput(inputId = "exp",
                            label = "Current number of non-symptomatic spreaders",
                            value = 10),
               
               numericInput(inputId = "inf",
                            label = "Current number of symptomatic infected cases",
                            value = 20),
               
               numericInput(inputId = "rec",
                            label = "Current total of immune/recovered/dead",
                            value = 300000),
               
        ))),
    
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage("Output:",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Spread",  
      # Output: HTML table with requested number of observations ----
      
      
      fluidPage(
        fluidRow(
          plotOutput("plot2") %>% withSpinner(color="#1E90FF"),
          
          uiOutput("HospBedper")    
          
        )
      )
      
      
    ),
    
    tabPanel("Calender",  
             # Output: HTML table with requested number of observations ----
             
             
             fluidPage(
               fluidRow(
                 
                 p(HTML("Probability of the virus existence:")), 
                 
                 plotOutput("plot3")  %>% withSpinner(color="#1E90FF")    
                 
               )
             )
             
             
    )
    
    ))
    
    
    ########################
  ) 
)




server <- function(input, output) {
  
  
  realisation <- reactive({
    
    
    ##### General setup
    compt = c('Susceptible', 
              'Exposed', 
              'Infectious', 
              'Recovered') # SEIR compartment
    N = matrix(0, nrow = 1, ncol = length(compt)) # a matrix to store number of people in each compartment
    real = 1000 # number of simulation
    
    ##### Epidemic model setup: parameters
    N_phases = 1 # number of phases (e.g. intervention at t=t*)
    #R0 = 3.3 # estimated to have [2.4, 3.3]
    
    # equal mean holding times for E and I compartments (mean(E+I) = 14days)
    mean_holding_times = c(6.6, 7.4)  # mean holding times at compartment E and I
    total_holding_times = sum(mean_holding_times)
    beta = input$R0 / c(total_holding_times, total_holding_times)
    
    ##### Assign initial conditions
    N[1,1] = input$pop  # number of susceptible people (population of ROI)
    N[1,2] = input$exp   # number of exposed people
    N[1,3] = input$inf   # number of infected people
    N[1,4] = input$rec
    
    # initialise simulation condition
    t = 0
    dt = 1 # time increment in days
    t_phase = Inf
    
    ##### Run simulation
    realisation = list(Time = list(), S = list(), E = list(), I = list(), R = list()) # a list of lists to store results
    for (r in 1:real) {
      for(i in 1:N_phases){
        if (i == 1) { # for first phase of the epimedic
          res = seir_model(t_phase, t, dt, N, mean_holding_times, beta)
        }
        else {
          # TODO: pass results from the first phase with different infection parameters
        }
        
      }
      # rename the column names to appropriate compartments and convert it to a data frame
      res = as.data.frame(res) %>% rename_at(vars(paste0(c(rep("V", length(compt))), 2:5)), ~compt)
      
      # save the results as a list of vectors
      realisation$Time[[r]] = res$Time
      realisation$S[[r]] = res$Susceptible
      realisation$E[[r]] = res$Exposed
      realisation$I[[r]] = res$Infectious
      realisation$R[[r]] = res$Recovered
    }
    
    realisation
    
  })
  
  
  d <- reactive({

    x <- c()

    for(i in 1:100){

      x[i] <- max(unlist(realisation()$Time[i]))

    }

    as.data.frame(x)

  })
  
  output$HospBedper <- renderUI({
    
    fit = survfit(Surv(d()$x)~1)
    df <- as.data.frame(cbind(fit$time,fit$surv))
    x <- df %>% filter(V2 > 0.49 & V2 < 0.6)
    y <- df %>% filter(V2 < 0.059 & V2 > 0.01)
    t <- as.Date("2020-06-25")
    x <- max(x$V1) + t
    y <- min(y$V1) + t
    
    
    
    paste0("There is a 50% chance that COVID-19 will be gone by ", format(x, format = "%d-%b-%Y"), " and 95% chance that it will be gone by ",format(y, format = "%d-%b-%Y")," (Start date: 25-Jun-2020).")
  })
  
  output$plot2 <- renderPlot({

    #, fill = ..count..
    ggplot(d(), aes(x)) +
      geom_histogram(aes(x, fill = ..count.., colour = x), show.legend = FALSE) +
      #geom_density(aes(y = 20 *..count..) ,show.legend = FALSE) +
      theme_minimal() +
      labs(x = "Days", y = "Number of future scenarios") +
      ggtitle("Possible future scenarios")
    #+ scale_fill_gradient(low = "purple", high = "blue")

  })
  
  output$plot3 <- renderPlot({
    
    x <- c()
    
    for(i in 1:100){
      
      x[i] <- max(unlist(realisation()$Time[i]))
      
    }
    
   d <-  as.data.frame(x)
    
    names(d) <- "days"
    fit = survfit(Surv(d$days)~1)
    df <- as.data.frame(cbind(fit$time,fit$surv))
    
    t <- as.Date("2020-06-25")
    df$V1 <- df$V1 + t
    
    
    t1 <- as.Date("2020-06-25", tz = "UTC")
    t2 <- as.Date(max(df$V1), tz = "UTC")
    tseq <- as.data.frame(seq(from = t1, to = t2, by = "day"))
    names(tseq) <- 'date'
    names(df) <- c('date', "prob")
    
    
    df2 <- full_join(tseq, df)  
    
    df3 <- na.omit(df2)
    #lm.fit <- lm(prob ~ date , data = df3)
    rfMod <- randomForest(prob ~ date , data = df3)
    
    
    df2$prob[is.na(df2$prob)] <- predict(rfMod, newdata = df2[which(is.na(df2$prob)), ])
    
    calendarPlot(df2, pollutant = "prob")
    

  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(ggpubr)
library(png)
library(ggplot2)

source('run_emulator.R')

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("How long will COVID-19 last in Ireland?"),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "R0 - average number of infected people for each infected person", 0.1, 6, 0.8, step=0.1),
               
               numericInput(inputId = "exp",
                            label = "Current number of non-symptomatic spreaders",
                            value = 10),
               
               numericInput(inputId = "inf",
                            label = "Current number of symptomatic infected cases",
                            value = 20),
               
               numericInput(inputId = "rec",
                            label = "Current total of immune/recovered/dead",
                            value = 300000),
               
        )), width = 4),
    

    # Main panel for displaying outputs ----
    mainPanel(
      fluidPage(
        fluidRow(
          splitLayout(cellWidths = c("33%", "33%", "33%"), 
                      plotOutput("my_plot1") %>% withSpinner(color="#1E90FF"), 
                      plotOutput("my_plot2") %>% withSpinner(color="#1E90FF"), 
                      plotOutput("my_plot3") %>% withSpinner(color="#1E90FF"))
        )
      )
    )
    
))



# Server ------------------------------------------------------------------

server <- function(input, output) {

  img = png::readPNG('blank_cal3.png')
  
  df = data.frame(x = seq(0, 1, by = 0.1),
                  y = seq(0, 1, by = 0.1))
  
  re <- reactive({
    ans = run_emulator(input$R0,input$exp,input$inf,input$rec)
    today = as.Date(Sys.time())
    date10_raw = today + ans['q10']
    date50_raw = today + ans['q50']
    date90_raw = today + ans['q90']

    out = list(date10 = list(day = format(date10_raw, '%d'), month  = format(date10_raw, '%B'), year = format(date10_raw, '%Y')),
               date50 = list(day = format(date50_raw, '%d'), month  = format(date50_raw, '%B'), year = format(date50_raw, '%Y')),
               date90 = list(day = format(date90_raw, '%d'), month  = format(date90_raw, '%B'), year = format(date90_raw, '%Y')))
    return(out)    
    
  })
  
  output$my_plot1 <- renderPlot({
    
    ggplot(df, aes(x = x, y = y)) +
      background_image(img) +
      geom_point(alpha = 0) +
      annotate("text", x = 0.5, y = 0.9, label = re()$date10$month, 
               colour = "white",
               size = rel(20)) +
      annotate("text", x = 0.5, y = 0.5, label = re()$date10$day, 
               colour = "black",
               size = rel(70)) +
      annotate("text", x = 0.5, y = 0.15, label = re()$date10$year, 
               colour = "black",
               size = rel(30)) +
      labs(title = "10% chance it will be extinct by...") + 
      theme_void(base_size = rel(10))
  })
  
  output$my_plot2 <- renderPlot({
    
    ggplot(df, aes(x = x, y = y)) +
      background_image(img) +
      geom_point(alpha = 0) +
      annotate("text", x = 0.5, y = 0.9, label = re()$date50$month, 
               colour = "white",
               size = rel(20)) +
      annotate("text", x = 0.5, y = 0.5, label = re()$date50$day, 
               colour = "black",
               size = rel(70)) +
      annotate("text", x = 0.5, y = 0.15, label = re()$date50$year, 
               colour = "black",
               size = rel(30)) +
      labs(title = "50% chance it will be extinct by...") + 
      theme_void(base_size = rel(10))
  })
  
  output$my_plot3 <- renderPlot({
    
    ggplot(df, aes(x = x, y = y)) +
      background_image(img) +
      geom_point(alpha = 0) +
      annotate("text", x = 0.5, y = 0.9, label = re()$date90$month, 
               colour = "white",
               size = rel(20)) +
      annotate("text", x = 0.5, y = 0.5, label = re()$date90$day, 
               colour = "black",
               size = rel(70)) +
      annotate("text", x = 0.5, y = 0.15, label = re()$date90$year, 
               colour = "black",
               size = rel(30)) +
      labs(title = "90% chance it will be extinct by...") + 
      theme_void(base_size = rel(10))
  })
}  

# Run the application
shinyApp(ui = ui, server = server)


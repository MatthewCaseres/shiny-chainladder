# Load Libraries
library(shiny)
library(tidyverse)
library(ChainLadder)


# Prep the data
data0 <- read_delim("data/clrd.txt")

data <- data0 %>% 
  pivot_longer(cols = c("IncurLoss", "CumPaidLoss", "BulkLoss", "EarnedPremDIR", "EarnedPremCeded", "EarnedPremNet"),
               names_to = "Measure",
               values_to = "Amount")

lob_list <- data %>% distinct(LOB) %>% arrange(LOB) %>% pull(LOB)
measure_list <- data %>% distinct(Measure) %>% arrange(Measure) %>% pull(Measure)


# Setup the UI
ui <- fluidPage(

  h1("Hosting actuarial dashboards in R Shiny"),
  
    sidebarLayout(
        sidebarPanel(
          selectInput("lob", "Choose an LOB", lob_list),
          selectInput("measure", "Choose an Amount", measure_list),
          selectInput("lattice", "Do you want to lattice the charts?", c(TRUE, FALSE))
        ),

        mainPanel(
          tabsetPanel(
            type='tab',
            
            tabPanel('Data', 
                     dataTableOutput('data')),
            
            tabPanel('Triangle', 
                     h3('Incremental'), 
                     verbatimTextOutput('triangle_inc'),
                     h3('Cumulative'), 
                     verbatimTextOutput('triangle_cum')),
            
            tabPanel('Plot',
                     h3('Incremental'), 
                     plotOutput('plot_inc'),
                     h3('Cumulative'), 
                     plotOutput('plot_cum')),
            
            tabPanel('Chain Ladder',
                     verbatimTextOutput('cd'), 
                     plotOutput('cdplot')),
            
            tabPanel('Bootstrap',
                     verbatimTextOutput('boot'), 
                     plotOutput('bootplot'))
          )
        )
    ),
  
  hr(),
  
  print('This app is a demo of P&C insurance triangles in R Shiny.')

  )
          

# Server Calculations
server <- function(input, output) {
  
  data_filtered <- reactive({ filter(data, LOB == input$lob, Measure == input$measure) })
  
  output$data <- renderDataTable({ data_filtered() })
  
  data_triangle <- reactive({
    as.triangle(data_filtered(), dev = "DevelopmentLag", origin = "AccidentYear", value = "Amount") 
    })
  
  output$triangle_cum <- renderPrint({ data_triangle() })
  
  output$triangle_inc <- renderPrint({ cum2incr(data_triangle()) })
  
  output$plot_cum <- renderPlot({ plot(data_triangle(), lattice = as.logical(input$lattice)) })
  
  output$plot_inc <- renderPlot({ plot(cum2incr(data_triangle()), lattice = as.logical(input$lattice)) })
  
  output$cd <- renderPrint({ MackChainLadder(data_triangle()) })
  
  output$cdplot <- renderPlot({ plot(MackChainLadder(data_triangle()), lattice = as.logical(input$lattice)) })

  B <- reactive({ BootChainLadder( data_triangle()) })
  
  output$boot <- renderPrint({ B() })
  
  output$bootplot <- renderPlot({ plot(B()) })
  
  }


# Run the App
shinyApp(ui = ui, server = server)
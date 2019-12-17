library(shiny)
library(leaflet)

setwd("C:/Users/Dominic/Documents/tuition_v_income/app")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  fluidRow(
    column(4),
    column(8, tags$h2("Click a state to view"))
  ),
  
    
  fluidRow(
    column(4, plotOutput('income_vs_tuition', height = 420)),
    column(8, leafletOutput('map', height = 400))
  ),
  
  tags$div(style='height:10px'),
  
  fluidRow(
    column(4,
           htmlOutput("savings"),
           tags$body("(Adjust below)"),
           tags$br(),
           numericInput('pct_median', "Percent of median income:", value=100,
                 min = 1, max = 2000, width='300px'),
           numericInput('pct_save', "Percet of income saved for tuition:", value=5,
                 min = 1, max = 100, width='300px')),
    column(8,
           htmlOutput("summary_stats")
    )
    
  )
))

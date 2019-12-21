library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  fluidRow(
    HTML("<h3><center> Public University Affordability by State </center></h2>")
  ),
  fluidRow(
    HTML("<h4><center>(Click a state to view)</center></h4>")
  ),
  fluidRow(
    column(4, plotOutput('income_vs_tuition', height = 420)),
    column(8, leafletOutput('map', height = 400))
  ),
  
  tags$div(style='height:10px'),
  
  fluidRow(
    column(1),
    column(10,
           htmlOutput("savings_choice"),
           tags$br(),
           htmlOutput("savings_outcome")),
    column(1)
    ),
  tags$hr(),
  fluidRow(
    column(12,
           htmlOutput("summary_stats"))
  )
))

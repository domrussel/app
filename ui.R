library(shiny)
library(leaflet)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  setBackgroundColor(
    color = "#363636",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  
  HTML("<font color='#f0f0f0' face='lato' size='+0.8'>"),
  
  fluidRow(
    HTML("<h2><center> Public University Affordability by State </center></h2>")
  ),
  fluidRow(
    HTML("<h3><center>(Click a state to view)</center></h3>")
  ),
  fluidRow(
    column(8, leafletOutput('map', height = 400)),
    column(4, plotOutput('income_vs_tuition', height = 420))
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

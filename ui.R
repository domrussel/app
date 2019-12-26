library(shiny)
library(leaflet)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML(".leaflet-container { background: #414141;}"))
  ),
  
  setBackgroundColor(
    color = "#363636",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  
  HTML("<font color='#f0f0f0' face='lato' size='+1'>"),
  
  fluidRow(
    HTML("<h2 style='font-family:sans-serif;'><center><b> Public University Affordability by State </b></center></h2>")
  ),
  fluidRow(
    HTML("<h3 style='font-family:sans-serif;'><center>(Click a state then scroll to view)</center></h3>")
  ),
  tags$br(),
  fluidRow(
    column(7, leafletOutput('map', height = 400)),
    column(5, plotOutput('income_vs_tuition', height = 420))
  ),
  
  tags$div(style='height:10px'),
  
  tags$hr(),
  fluidRow(
    column(1),
    column(10,
           tags$br(),
           htmlOutput("savings_choice"),
           tags$br(),
           htmlOutput("savings_outcome"),
           tags$br()),
    column(1)
    ),
  tags$hr(),
  tags$br(),
  fluidRow(
    column(12,
           htmlOutput("summary_stats"))
  ),
  tags$hr()
))

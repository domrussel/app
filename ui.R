library(shiny)
library(leaflet)
library(shinyWidgets)

shinyUI(fluidPage(
  
  # Set the background color for the map
  tags$head(
    tags$style(HTML(".leaflet-container { background: #414141;}"))
  ),
  
  # Set the app background color
  setBackgroundColor(
    color = "#363636",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  
  # Set the default font, font color, and font size
  HTML("<font color='#f0f0f0' face='lato' size='+1'>"),
  
  # Titles
  fluidRow(
    HTML("<h2 style='font-family:sans-serif;'><center><b> Flagship Public University Affordability by State </b></center></h2>")
  ),
  fluidRow(
    HTML("<h3 style='font-family:sans-serif;'><center>(Click a state then scroll to view)</center></h3>")
  ),
  
  # Display the map and bar graph side-by-side
  # (will get split into two rows if viewing on narrow browser, e.g. mobile)
  tags$br(),
  fluidRow(
    column(7, leafletOutput('map', height = 400)),
    column(5, plotOutput('income_vs_tuition', height = 420))
  ),
  
  tags$div(style='height:10px'),
  
  # Display the savings choice and outcome
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
  
  # Display the raw summary stats
  tags$hr(),
  tags$br(),
  fluidRow(
    column(12,
           htmlOutput("summary_stats"))
  ),
  tags$hr()
))

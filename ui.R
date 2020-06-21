library(shiny)
library(plotly)
library(shinyWidgets)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Outbreak Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "n", 
                   label = "Length of simulation (day)", 
                   value = 365, 
                   min = 0),
      numericInput(inputId = "N", 
                   label = "Population", 
                   value = 10000, 
                   min = 0),
      numericInput(inputId = "init_infected", 
                   label = "Initially infected", 
                   value = 10, 
                   min = 0),
      numericRangeInput(inputId = "init_prevention", 
                        label = "Intervention period", 
                        value = c(30,120), 
                        width = NULL, 
                        separator = " to "),
      sliderInput(inputId = "incubation",
                  label = "Incubation length (day)",
                  min = 1,
                  max = 60,
                  value = 14),
      sliderInput(inputId = "infected_to_severity",
                  label = "Days from infected to severity",
                  min = 1,
                  max = 60,
                  value = 7),
      sliderInput(inputId = "hospital_to_death",
                  label = "Days from severity to death",
                  min = 1,
                  max = 60,
                  value = 30),
      sliderInput(inputId = "epsilon",
                  label = "Probability of severity",
                  min = 0,
                  max = 1,
                  value = 0.1),
      numericInput(inputId = "hospital_cap", 
                   label = "Hospital capacity", 
                   value = 1000, 
                   min = 0),
      sliderInput("alpha", 
                  label = "Fatality Probability (In the hospital)", 
                  min = 0, 
                  max = 1, 
                  value = c(0.4, 0.6)),
      numericInput(inputId = "non_severe_infection_length", 
                   label = "Non severe cases infection length (day)", 
                   value = 14, 
                   min = 1),
      numericInput(inputId = "severe_infection_length", 
                   label = "Severe cases infection length (day)", 
                   value = 14, 
                   min = 1),
      sliderInput(inputId = "R0",
                  label = "Basic reproduction number (R0)",
                  min = 0,
                  max = 30,
                  step = 0.1,
                  value = c(2.5, 6)),
      actionButton('sim','Launch simulation')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Plot
      # plotlyOutput("summary")
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", plotlyOutput('SRF'),
                           plotlyOutput('IHE'),
                           plotlyOutput('IHD_new_case')),
                  tabPanel("Statistics", plotlyOutput("pie")),
                  tabPanel("Death rate", plotlyOutput("death_rate"))
      )
    )
  )
)
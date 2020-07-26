library(shiny)
library(plotly)
library(shinyWidgets)
library(shinydashboard)


body_colwise <- dashboardBody(
  fluidRow(
    box(
      width = 3,
      height = 320,
      numericInput(inputId = "n", 
                   label = "Length of simulation (day)", 
                   value = 365, 
                   min = 0),
      numericInput(inputId = "N", 
                   label = "Population", 
                   value = 300000, 
                   min = 0),
      numericInput(inputId = "init_infected", 
                   label = "Initially infected", 
                   value = 500, 
                   min = 0),
      numericRangeInput(inputId = "init_prevention", 
                        label = "Intervention period", 
                        value = c(30,90), 
                        width = NULL, 
                        separator = " to ")
        ),
    box(
      width = 3,
      height = 320,
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
                  value = 14)
      ),
    box(
      width = 3,
      height = 320,
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
                  value = c(0.4, 0.6))
      ),
    box(
      width = 3,
      height = 320,
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
                  value = c(0.3, 6)),
      actionButton('sim','Launch simulation')
      )
    ),
  fluidRow(
    valueBoxOutput("infected", width = 3),
    valueBoxOutput("recovered", width = 3),
    valueBoxOutput("hospital", width = 3),
    valueBoxOutput("death", width = 3)
  ),
  fluidRow(
    box(
      width = 6,
      plotlyOutput("SRF")
      ),
    box(
      width = 6,
      plotlyOutput("IHE")
      )
    ),
  fluidRow(
    box(
      plotlyOutput("IHD_new_case")
      ),
    box(
      plotlyOutput("death_rate")
      )
    )
  )


ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body_colwise
)

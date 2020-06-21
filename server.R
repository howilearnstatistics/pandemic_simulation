library(shiny)
library(shinyWidgets)
library(plotly)
server <- function(input, output) {
  delta            = reactive(1/input$incubation)                  #incubation period of 14 days
  mu               = reactive(1/ input$infected_to_severity)       #severity rate 
  epsilon          = reactive(input$epsilon)                       #severity probability
  rho_hospital     = reactive(1/input$hospital_to_death)           #rate at which people die at the hospital 
  hospital_cap     = reactive(input$hospital_cap)                  #hospital capacity
  alpha_base       = reactive(min(input$alpha))                    #min fatality probability
  alpha_max        = reactive(max(input$alpha))                    #max fatality probability
  gamma_non_severe = reactive(1/input$severe_infection_length)     #recovery rate of n on severe cases
  gamma_hospital   = reactive(1/input$non_severe_infection_length) #recovery rate at the hospital
  x_0              = reactive(min(input$init_prevention))          #lockdown start date
  x_1              = reactive(max(input$init_prevention))          #lockdown end date
  n                = reactive(input$n)                             #simulation length
  N                = reactive(input$N)                             #population
  init_infected    = reactive(input$init_infected)                 #initially infected
  R_0_start        = reactive(max(input$R0))                       #min R0
  R_0_end          = reactive(min(input$R0))                       #max R0
  observeEvent(input$sim, {
    t = seq(1,n())
    k = 0.7
    R_0 = c()
    R0_lockdown <- function(x){
      return((R_0_start() - R_0_end())/(1 + exp(-k * (-x + x_0()))) + R_0_end())
    }
    R0_lift_lockdown <- function(x){
      return((R_0_end() - R_0_start())/(1 + exp(-k * (-x + x_1() + 10))) + R_0_start())
    }
    #non constant parameters
    R_0[c(1:x_1()-1)] = R0_lockdown(c(1:x_1()-1)) #R0 as a variable dependent on time
    R_0[c(x_1():n())] = R0_lift_lockdown(c(x_1():n())) #R0 as a variable dependent on time
    beta = R_0 * gamma_non_severe() #transmission rate 
    #time step
    step = 1
    
    S = c() #susceptible  
    E = c() #exposed  
    I = c() #infected
    SE = c() #severity 
    R = c() #recovery  
    D = c() #death  
    H = c() #hospitalized  
    alpha = c()
    
    D_perday = c() #death per day
    I_perday = c() #Infected per day
    H_perday = c() #Hospitalised per day
    
    I_difference = c()
    
    I[1]  = init_infected()
    S[1]  = N() - I[1]
    R[1]  = 0
    SE[1] = 0
    E[1]  = 0
    D[1]  = 0
    H[1]  = 0
    
    I_difference[1] = 0
    
    D_perday[1] = 0 #death per day
    I_perday[1] = I[1] #Infected per day
    H_perday[1] = 0 #Hospitalized per day
    
    I_cumulate = 0
    H_cumulate = 0
    D_cumulate = 0
    alpha[1]   = alpha_base()
    
    for (i in 2:n()){
      if(H[i-1] <= hospital_cap()){
        alpha[i] = alpha_base()
      }
      else{
        temp = alpha_base() + 0.0005 * (H[i-1] - hospital_cap())
        if (temp < alpha_max()){
          alpha[i] = temp
        }
        else{
          alpha[i] = alpha_max()
        } 
      }
      S[i] = S[i-1] + step*(-beta[i] * S[i-1] * I[i-1])/N() 
      E[i] = E[i-1] + step*(beta[i] * S[i-1] * I[i-1] / N() 
                            -delta() * E[i-1])
      I[i] = I[i-1] + step*(-mu() * epsilon() * I[i-1] 
                            -gamma_non_severe() * (1 - epsilon()) * I [i-1] 
                            + delta() * E[i-1])
      H[i] = H[i-1] + step*(-gamma_hospital() * (1 - alpha[i]) * H[i-1]
                            -rho_hospital() * alpha[i] * H[i-1]
                            +mu() * epsilon() * I[i-1] )
      D[i] = D[i-1] + step*(+alpha[i] * rho_hospital() * H[i-1])
      R[i] = R[i-1] + step*(+gamma_hospital() * (1 - alpha[i]) * H[i-1] 
                            +gamma_non_severe() * (1 - epsilon()) * I [i-1])
      #
      I_perday[i] = delta() * E[i-1] #cumulative number of infected people
      H_perday[i] = mu() * epsilon() * I[i-1] #cumulative number of hospitalized people
      D_perday[i] = alpha[i] * rho_hospital() * H[i-1] #cumulative number of dead people
      #
      I_cumulate = I_cumulate + delta() * E[i-1] #cumulative number of infected people
      H_cumulate = H_cumulate + mu() * epsilon() * I[i-1] #cumulative number of hospitalized people
      D_cumulate = D_cumulate + alpha[i] * rho_hospital() * H[i-1] #cumulative number of dead people
      #
      I_difference[i] = (round(I[i] - I[i-1])) * 100 / round(I[i-1]) 
    }
    #plot parameters
    f = list(
      family = "Courier New, monospace",
      size = 18,
      color = "#00008b"
    )
    x_SRF = list(title = "Time (day)",
                 titlefont = f)
    y_SRF = list(title = "Population",
                 titlefont = f)
    lockdown_IHE = list(
      x = c(x_0(), x_1()),
      y = c(E[x_0()], E[x_1()]),
      text = c("Lockdown start", "Lockdown end"),
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = -20,
      ay = -40
    )
    x_IHE = list(title = "Time (day)",
                 titlefont = f)
    y_IHE = list(title = "Active Cases",
                 titlefont = f)
    x_IHD_new_case = list(title = "Time (day)",
                          titlefont = f)
    y_IHD_new_case = list(title = "New Cases",
                          titlefont = f)
    lockdown_IHD_new_case = list(
      x = c(x_0(), x_1()),
      y = c(I_perday[x_0()], I_perday[x_1()]),
      text = c("Lockdown start", "Lockdown end"),
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = -20,
      ay = -40
    )
    #
    x_death_rate = list(title = "Time (day)",
                        titlefont = f)
    y_death_rate = list(title = "Death rate",
                        titlefont = f)
    x_hospitalized = list(title = "Time (day)",
                          titlefont = f)
    y_hospitalized = list(title = "Active Cases",
                          titlefont = f)
    lockdown_hospitalized = list(
      x = c(x_0(), x_1()),
      y = c(H[x_0()], H[x_1()]),
      text = c("Lockdown start", "Lockdown end"),
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = c(-20, 20),
      ay = c(-40, 40))
    #label and value for pie charts
    population_label = c("Infected", "Healthy")
    population_value = c(round(I_cumulate), round(N() - I_cumulate))
    
    infected_label = c("Hospitalized", "Recovered")
    infected_value = c(round(H_cumulate), round(I_cumulate - H_cumulate))
    
    hospital_label = c("Fatality", "Recovered")
    hospital_value = c(round(D_cumulate), round(H_cumulate - D_cumulate))
    #plot
    output$SRF = renderPlotly(plot_ly(x = ~t, 
                                      y = ~S, 
                                      name = "Susceptible",
                                      type = 'scatter',
                                      mode = 'lines',
                                      line = list(width = 3,
                                                  color = "#0000ff"),
                                      hoverinfo = 'text',
                                      text = ~paste('</br> Day: ', t,
                                                    '</br> Number of Susceptible: ', round(S)))
                              %>% add_trace(y = ~R,
                                            name = "Recovered",
                                            mode = "lines",
                                            line = list(width = 3,
                                                        color = "#008000"),
                                            text = ~paste('</br> Day: ', t,
                                                          '</br> Number of Recovered: ', round(R)))
                              %>% add_trace(y = ~D,
                                            name = "Fatality",
                                            mode = "lines",
                                            line = list(width = 3,
                                                        color = "#000000"),
                                            text = ~paste('</br> Day: ', t,
                                                          '</br> Number of Fatality: ', round(D)))
                              %>% layout(xaxis = x_SRF,
                                         yaxis = y_SRF,
                                         hovermode = 'x'))
    output$IHE = renderPlotly(plot_ly(x = ~t, 
                                      y = ~I, 
                                      name = "Infected",
                                      type = 'scatter',
                                      mode = 'lines',
                                      line = list(width = 3,
                                                  color = "#ffa500"),
                                      hoverinfo = 'text',
                                      text = ~paste('</br> Day: ', t,
                                                    '</br> Number of Infected: ', round(I)))
                              %>% add_trace(y = ~H,
                                            name = "Hospitalized",
                                            mode = "lines",
                                            line = list(width = 3,
                                                        color = "#00ffff"),
                                            text = ~paste('</br> Day: ', t,
                                                          '</br> Number of Hospitalised: ', round(H)))
                              %>% add_trace(y = ~E,
                                            name = "Exposed",
                                            mode = "lines",
                                            line = list(width = 3,
                                                        color = "#ff7f50"),
                                            text = ~paste('</br> Day: ', t,
                                                          '</br> Number of Exposed: ', round(E)))
                              %>% layout(xaxis = x_IHE,
                                         yaxis = y_IHE,
                                         annotations = lockdown_IHE,
                                         hovermode = 'x'))
    output$IHD_new_case = renderPlotly(plot_ly(x = ~t, 
                                               y = ~I_perday, 
                                               type = 'scatter',
                                               mode = "line",
                                               name = 'New Infected',
                                               color = "red",
                                               line = list(width = 3,
                                                           color = "#ffa500"),
                                               hoverinfo = 'text',
                                               text = ~paste('</br> Day: ', t,
                                                             '</br> New Infected: ', round(I_perday)),
                                               showlegend = TRUE)
                                       %>% add_trace(y = ~H_perday, 
                                                     name = 'New Hospitalized',
                                                     color = "purple",
                                                     line = list(width = 3,
                                                                 color = "#00ffff"),
                                                     hoverinfo = 'text',
                                                     text = ~paste('</br> Day: ', t,
                                                                   '</br> New Hospitalised: ', round(H_perday)),
                                                     showlegend = TRUE)
                                       %>% add_trace(y = ~D_perday, 
                                                     name = 'New Fatality',
                                                     color = "green",
                                                     line = list(width = 3,
                                                                 color = "#000000"),
                                                     hoverinfo = 'text',
                                                     text = ~paste('</br> Day: ', t,
                                                                   '</br> New Fatality: ', round(D_perday)),
                                                     showlegend = TRUE)
                                       %>% layout(xaxis = x_IHD_new_case,
                                                  yaxis = y_IHD_new_case,
                                                  annotations = lockdown_IHD_new_case,
                                                  hovermode = 'x'))
    output$pie = renderPlotly(plot_ly(labels = population_label, 
                                      values = population_value,
                                      type = 'pie',
                                      textinfo ='label+percent',
                                      insidetextorientation ='radial',
                                      hoverinfo = 'text',
                                      text = ~paste('</br> Status: ', population_label,
                                                    '</br> Number: ', population_value),
                                      marker = list(colors = c("#ffa500","#0000ff"),
                                                    line = list(color = '#FFFFFF', 
                                                                width = 1)),
                                      domain = list(x = c(0, 0.6), 
                                                    y = c(0, 1)),
                                      showlegend = FALSE,
                                      title = list(text = 'Breakdown of the Population', 
                                                   y = -1, 
                                                   font = f))
                              %>% add_pie(labels = infected_label, 
                                          values = infected_value, 
                                          textinfo ='label+percent',
                                          insidetextorientation ='radial',
                                          hoverinfo = 'text',
                                          text = ~paste('</br> Status: ', infected_label,
                                                        '</br> Number: ', infected_value),
                                          marker = list(colors = c("#00ffff","#008000"),
                                                        line = list(color = '#FFFFFF', 
                                                                    width = 1)),
                                          domain = list(x = c(0.6, 1), 
                                                        y = c(0.3, 1)),
                                          showlegend = FALSE,
                                          title = list(text = 'Breakdown of the Infected', 
                                                       y = 0.8, 
                                                       font = f))
                              %>% add_pie(labels = hospital_label, 
                                          values = hospital_value, 
                                          textinfo ='label+percent',
                                          insidetextorientation ='radial',
                                          hoverinfo = 'text',
                                          text = ~paste('</br> Status: ', hospital_label,
                                                        '</br> Number: ', hospital_value),
                                          marker = list(colors = c("#000000", "#008000"),
                                                        line = list(color = '#FFFFFF', 
                                                                    width = 1)),
                                          domain = list(x = c(0.6, 1), 
                                                        y = c(0.0, 0.3)),
                                          showlegend = FALSE,
                                          title = list(text = 'Breakdown of the Hospitalised', 
                                                       font = f)))
    output$death_rate = renderPlotly(subplot(
      plot_ly(x = ~t, 
              y = ~alpha, 
              type = "scatter",
              mode = "line",
              name = "Fatality probability (in hospital)",
              line = list(width = 3),
              hoverinfo = 'text',
              text = ~paste('</br> Day: ', t,
                            '</br> Fatality probability: ', round(alpha, 3)))
      %>% layout(xaxis = x_death_rate,
                 yaxis = y_death_rate,
                 hovermode = 'x'),
      plot_ly(x = ~t, 
              y = ~H, 
              name = "Hospitalised",
              type = 'scatter',
              mode = 'lines',
              line = list(width = 3,
                          color = "#00ffff"),
              hoverinfo = 'text',
              text = ~paste('</br> Day: ', t,
                            '</br> Hospitalised: ', round(H)))
      %>% layout(xaxis = x_hospitalized,
                 yaxis = y_hospitalized,
                 hovermode = 'x',
                 annotations = lockdown_hospitalized)
      %>% add_lines(x = t, 
                    y = hospital_cap(),
                    name = "Hospital Capcacity",
                    line = list(width = 3,
                                color = "#9932cc"),
                    hoverinfo = 'text',
                    text = ~paste('</br> Day: ', t,
                                  '</br> Hospital Capacity: ', hospital_cap())),
      nrows = 2,
      shareX = TRUE,
      titleY = TRUE
    )
    )
  })
}
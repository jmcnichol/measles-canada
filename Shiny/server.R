# measles shiny app ui 
library(shiny)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr) 
library(tidyr)
library(stringr)
library(parallel)
library(rmarkdown)
library(markdown)

source("simulator.R")

server <- function(input, output, session) {
  
  
  
  output$mainplot <- renderPlotly({
    
    simdata <- measles.seir.sim(vax.rate = input$vaxFrac, 
                                pop.size = input$N ,
                                I0 = 2, 
                                qs = (1/input$contactTime)*(0.5)*(0.5)*(input$isolateEff/100), 
                                qi = input$isolateI,
                                qspep = (1/input$contactTime)*(0.5)*(input$pepAcc/100)*(0.5),
                                nsims=25)
    
    ##-- ggplot of outbreaks --####
    outbreak.plot <- simdata %>%
      mutate(week = day%/%7) %>% 
      group_by(week,vax,simnum) %>% 
      summarise(size=sum(inci)) %>% 
      group_by(week,vax) %>%
      summarise(medsize=median(size)) %>%
      ggplot(aes(y = medsize,x=week,color=vax)) +
      # geom_point(size = 0.5, alpha = 0.3) + 
      geom_point() +
      geom_line() +
      labs(x = "Weeks since first case",
           y = "Number of cases") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none") 
    
    #convert ggplot to plotly
    mainplot <- ggplotly(outbreak.plot) 
    
    
    # function output to ui 
    mainplot
    
  })
  
  
}



ui <- fluidPage(
  titlePanel("Modeling measles outbreaks in Canada"),
  hr(),
  p(div(HTML("Disclaimer: This simulation is for research and educational purposes only."))),
  p(HTML("<b>User instructions:</b> This interactive tool implements a stochastic SEIR model. Adjust intervetion paramters to model measles outbreaks. To see how the sliders below translate to model parameters see the model information tab.   ")),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      fluidRow(
        h4(div(HTML("<em>Set simulation values</em>"))),
        column(width = 12,
               sliderInput("vaxFrac", "Vaccination coverage", value = 0.95, max = 1, min = 0, step=0.05),
               sliderInput("N", "Population size", value = 1000, max = 10000, min = 250, step=250),
               br(),
        ),
        h4(div(HTML("<em>Set intervention parameters</em>"))),
        column(width = 6,
               sliderInput("contactTime", "Time to contact exposed individuals", value = 3, max = 20, min = 1, step=1, post = " days"),
               sliderInput("isolateI", "Proportion of individuals who are contagious and isolate", value = 0, max = 100, min = 0, post="%", step=10),
        ),
        column(width = 6,
               sliderInput("isolateEff", "Effectiveness of isolation in exposed individuals", value = 0, max = 100, min = 0, post="%", step=10),
               sliderInput("pepAcc", "Proportion of eligible people who accept PEP", value = 0, max = 100, min = 0, post="%", step=10),
               br(),
               # ( 1/time to contact (2- 3 days)) (frac eligible for PEP) (acceptance) (50 % effective) 
               # qspep = (1/input$contactTime)*(0.5)*(input$pepAcc/100)*(0.5) -- these are /100 since the input is as %
               # ( 1/2)* (1/2)* (0.9)* (0.5) ~ 0.1125  (i used 1/2 for 2 days )
               # (1/time to contact) (frac not PEP eligible) (want to isolate) (effectiveness of isolation)
               # qs = (1/input$contactTime)*(0.5)*(input$isolateE/100)*(input$isolateEff/100) 
               # (0.5 )* (1/2)* (0.6)* (1/2) = 0.075 
        ),
      ),
    ),
    
    mainPanel(
      
      #p(div(HTML("Test")))
      navbarPage("",
                 
                 tabPanel("Outbreak simulation",
                          fluidPage(
                            fluidRow(
                              
                              h3("Measles outbreak"),
                              plotlyOutput("mainplot"),
                              br(),
                              
                              br(),
                            )
                          )
                 ),
                 tabPanel("Model information",
                          fluidPage(
                            br(),
                            includeMarkdown("measles-shiny-info.Rmd"),
                          )),
                 tabPanel("About",
                          fluidPage(
                            br(),
                            withMathJax(includeMarkdown("measles-shiny-about.Rmd")),
                          )),
                 
      ),
      width=7
    )
  )
)


shinyApp(ui = ui, server = server)


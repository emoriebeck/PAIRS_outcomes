#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Idiographic Personality Networks"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("SID",
                                "Subject ID:",
                                choices = ""),
                 selectizeInput("Cor1",
                                "Select Network Type",
                                choices = c("Temporal", "Contemporaneous"))
               ), 
               mainPanel(
                 plotOutput("gVARPlot")
               ))),
    tabPanel("Centrality",  
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("SID3",
                                "Subject ID:",
                                choices = ""),
                 selectizeInput("Cor3",
                                "Select Network Type",
                                choices = c("Temporal", "Contemporaneous"))
               ), 
               mainPanel(
                 plotOutput("centrality")
               ))),
    tabPanel("Outcomes",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("predictor",
                                "Choose Predictor",
                                choices = c("Centrality", "Density", "Edge Weights")),
                 selectizeInput("outcome",
                                "Choose Outcome",
                                choices = ""),
                 selectizeInput("type",
                                "Choose Network Type",
                                choices = c("Temporal", "Contemporaneous")),
                 
                 conditionalPanel(
                   condition = "input.predictor == 'Centrality'",
                   selectizeInput("centrality", "Choose Centrality Index",
                                  list("Degree", "Strength", "Betweenness", "Closeness"))
                 ),
                 conditionalPanel(
                   condition = "input.predictor != 'Density'",
                   checkboxGroupInput("vars", label = "Choose Variables", 
                                      choices = list("A_rude","E_quiet","C_lazy","N_relaxed",
                                                     "N_depressed","E_outgoing","A_kind","C_reliable", 
                                                     "N_worried"), 
                                      selected = c("A_rude","E_quiet"))
                   )
               ),
               mainPanel(
                 plotOutput("outcome_plot")
               )))
  )
    
    # Show a plot of the generated distribution
    
  )


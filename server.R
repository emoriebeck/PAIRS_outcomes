
# Define server logic required to draw a histogram

load_url <- function (url, ..., sha1 = NULL) {
  # based very closely on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  load(temp_file, envir = .GlobalEnv)
}

load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/idiographic_plots.RData")
#load("~/Box Sync/network/PAIRS/PAIRS_graphicalVAR/centralityPlots.RData")
load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/centralityPlots.RData")
load_url("https://github.com/emoriebeck/PAIRS_outcomes/raw/master/outcome_res.Rdata")

library(graphicalVAR)
library(tidyverse)
library(gridExtra)


server <- function(input, output, session) {
observe({
  subs1 <- names(plot_kappa_w1)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
})

observe({
  subs3 <- names(centralityPCC)
  updateSelectizeInput(session, 'SID3', choices = c("",subs3))
})

observe({
  outcomes <- colnames(target.ratings.w1)[3:ncol(target.ratings.w1)]
  updateSelectizeInput(session, 'outcome', choices = c("", outcomes))
})

# observe({
#   vars <- as.list(unique((dat %>% 
#     filter(measuretype == "Edge Weights" & type == "Temporal") %>%
#     separate(var, c("from", "to"), sep = "[.]"))$from))
#   updateCheckboxGroupInput(session, 'vars', choices = vars)
# })
    
    output$gVARPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      validate(
        need(input$SID, 'Please select 2 Subject IDs'))
      
        if(input$Cor1 == "Temporal"){
          plot1  <-  plot_beta_w1[[input$SID]]
        } else{
          plot1  <-  plot_kappa_w1[[input$SID]]
        }
    
      # draw the histogram with the specified number of bins
      if(!("" %in% input)){
        plot(plot1)
      }
    })
    
    output$centrality <- renderPlot({
      # generate bins based on input$bins from ui.R
      validate(
        need(input$SID3, 'Please select 2 Subject IDs'))
        if(input$Cor3 == "Temporal"){
          plot1  <-  centralityPDC[[input$SID3]]
        } else{
          plot1  <-  centralityPCC[[input$SID3]]
        }
    })
    
    selectedData <- reactive({
      dat[, c(input$predictor, input$outcome)]
    })

    output$outcome_plot <- renderPlot({
      validate(
        need(input$predictor, "Please choose a predictor variable")
      )
      validate(
        need(input$outcome, "Please choose an outcome variable")
      )

      subset_dat <- dat %>%
        select(contains(input$predictor), contains(input$outcome))
      subset_dat %>%
        ggplot(aes(x = subset_dat[,1], y = subset_dat[,2])) +
        geom_point() +
        geom_smooth() +
        theme_classic()
      Type <- input$type
      predictor <- input$predictor
      outcome <- input$outcome
      vars <- input$vars
      print(vars)
      if(input$predictor == "Edge Weights"){
        dat %>%
          filter(type == type & measuretype == predictor & outcome == outcome ) %>%
          select(SID, type, var, measure, outcome, value, value2) %>%
          separate(var, c("from", "to"), sep = "[.]") %>%
          filter(from %in% vars & to %in% vars) %>%
          ggplot(aes(x = value, y = value2)) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x = predictor, y = outcome) +
          facet_grid(from~to) +
          theme_bw()
      } else if(input$predictor == "Centrality"){
        dat %>%
          filter(type == Type & measuretype == predictor & 
                   outcome == outcome & var %in% vars) %>%
          select(SID, type, var, measure, outcome, value, value2) %>%
          ggplot(aes(x = value, y = value2)) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x = predictor, y = outcome) +
          facet_grid(measure~var) +
          theme_bw()
      }else{
        dat %>%
          filter(type == type & measuretype == predictor & outcome == outcome) %>%
          select(SID, type, var, measure, outcome, value, value2) %>%
          ggplot(aes(x = value, y = value2)) +
          geom_point() +
          geom_smooth(method = "lm") +
          labs(x = predictor, y = outcome) +
          facet_grid(measure~.) +
          theme_bw()
      }
      
    })

}
a
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

load_url("https://github.com/emoriebeck/PAIRS_outcomes/raw/master/idiographic_plots.RData")
load_url("https://github.com/emoriebeck/PAIRS_outcomes/raw/master/centralityPlots.RData")
load_url("https://github.com/emoriebeck/PAIRS_outcomes/raw/master/outcome_res.Rdata")
# load("~/Box Sync/network/PAIRS/PAIRS_outcomes/idiographic_plots.RData")
# load("~/Box Sync/network/PAIRS/PAIRS_outcomes/centralityPlots.RData")
# load("~/Box Sync/network/PAIRS/PAIRS_outcomes/outcome_res.RData")
     

library(graphicalVAR)
library(tidyverse)
library(gridExtra)


server <- function(input, output, session) {
observe({
  subs1 <- names(PCC_plot0)
      updateSelectizeInput(session, 'SID', choices = c("", subs1),
                           selected = subs1[1])
})

observe({
  subs3 <- names(beta_centrality_plot)
  updateSelectizeInput(session, 'SID3', choices = c("",subs3),
                       selected = "10167")
})

observe({
  outcomes <- unique(dat$outcome)
  updateSelectizeInput(session, 'outcome', choices = c("", outcomes),
                       selected = "GPA")
})

    output$gVARPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      
      validate(
        need(input$SID, 'Please select a Subject ID'))
      
        if(input$Cor1 == "Temporal"){
          plot1  <-  PDC_plot0[[input$SID]]
        } else{
          plot1  <-  PCC_plot0[[input$SID]]
        }
    
      # draw the histogram with the specified number of bins
      if(!("" %in% input)){
        plot(plot1)
      }
    })
    
    output$centrality <- renderPlot({
      # generate bins based on input$bins from ui.R
      validate(
        need(input$SID3, 'Please select a Subject ID'))
        if(input$Cor3 == "Temporal"){
          plot1  <-  beta_centrality_plot[[input$SID3]]
        } else{
          plot1  <-  kappa_centrality_plot[[input$SID3]]
        }
      if(!("" %in% input)){
        plot(plot1)
      }
    })

    output$outcome_plot <- renderPlot({
      validate(
        need(input$predictor, "Please choose a predictor variable")
      )
      validate(
        need(input$outcome, "Please choose an outcome variable")
      )

      if(input$predictor == "Edge Weights"){
        Type <- input$type
        predictor <- input$predictor
        Outcome <- input$outcome
        Vars <- input$vars
        dat %>%
          filter(type == Type & measuretype == predictor & outcome == Outcome ) %>%
          select(SID, type, var, measure, outcome, value, value2) %>%
          separate(var, c("from", "to"), sep = "[.]") %>%
          filter(from %in% Vars & to %in% Vars) %>%
          ggplot(aes(x = value, y = value2)) +
          geom_point(size = .8, color = "grey") +
          geom_smooth(method = "lm") +
          labs(x = predictor, y = Outcome) +
          facet_grid(from~to) +
          theme_classic() +
          theme(axis.text = element_text(face = "bold", size = rel(1.2)),
                axis.title = element_text(face = "bold", size = rel(1.2)),
                strip.text = element_text(face = "bold", size = rel(1.2)))
      } else if(input$predictor == "Centrality"){
        Type <- input$type
        predictor <- input$predictor
        Outcome <- input$outcome
        Vars <- input$vars
        Centrality <- input$centrality
        dat2 <- dat  %>%
          filter(type == Type & measuretype == predictor & grepl(Centrality, measure) == T &
                   outcome == Outcome & var %in% Vars) %>%
          select(SID, type, var, measure, outcome, value, value2) %>%
          group_by(var, measure) %>%
          mutate(ymin = floor(min(value2, na.rm = T)), ymax = ceiling(max(value2, na.rm = T)),
                 xmin = floor(min(value, na.rm = T)), xmax = ceiling(max(value, na.rm = T))) 
        
        dat2 %>%
          ggplot(aes(x = value, y = value2)) +
          # scale_x_continuous(breaks = seq(floor(min(dat2$value, na.rm = T)), 
          #                                 ceiling(max(dat2$value, na.rm = T)), length.out = 3)) +
          #geom_blank(aes(x = xmin)) + geom_blank(aes(y = xmax)) +
          scale_y_continuous(breaks = seq(floor(min(dat2$value2, na.rm = T)), ceiling(max(dat2$value2, na.rm = T)), 
                                          length.out = 3)) +
          geom_jitter(size = .8, color = "grey") +
          geom_smooth(method = "lm") +
          labs(x = predictor, y = Outcome) +
          facet_grid(measure~var, scales = "free_x") +
          theme_classic() +
          theme(axis.text = element_text(face = "bold", size = rel(1.2)),
                axis.title = element_text(face = "bold", size = rel(1.2)),
                strip.text = element_text(face = "bold", size = rel(1.2)))
      }else{
        Type <- input$type
        predictor <- input$predictor
        Outcome <- input$outcome
        dat %>%
          filter(type == Type & measuretype == predictor & outcome == Outcome) %>%
          select(SID, type, var, measure, outcome, value, value2) %>%
          ggplot(aes(x = value, y = value2)) +
          geom_point(size = .8, color = "grey") +
          geom_smooth(method = "lm") +
          labs(x = predictor, y = Outcome) +
          facet_grid(measure~.) +
          theme_classic() +
          theme(axis.text = element_text(face = "bold", size = rel(1.2)),
                axis.title = element_text(face = "bold", size = rel(1.2)),
                strip.text = element_text(face = "bold", size = rel(1.2)))
      }
      
    })

}
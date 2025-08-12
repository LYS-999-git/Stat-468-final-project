#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(trelliscope)
library(dplyr)
library(ggplot2)
library(httr2)
library(jsonlite)
library(aws.s3)
library(duckdb)
library(log4r)


# Load environment variables from .Renviron
readRenviron(".Renviron")
Sys.setenv(
  AWS_ACCESS_KEY_ID = Sys.getenv("AWS_ACCESS_KEY_ID"),
  AWS_SECRET_ACCESS_KEY = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
  AWS_DEFAULT_REGION = Sys.getenv("AWS_DEFAULT_REGION"),
  S3_BUCKET=Sys.getenv("S3_BUCKET")
)
bucket <- Sys.getenv("S3_BUCKET")
api_url <- Sys.getenv("API_URL")

log <- log4r::logger()
log4r::info(log, "App Started")
########### Reading dataset and model
read_bucket_RDS <- function(file_path){
  obj <- get_object(file_path, 
                    bucket = Sys.getenv("S3_BUCKET"))
  
  # Save to a temporary file
  tmp <- tempfile(fileext = ".rds")
  writeBin(obj, tmp)
  
  # Load the R object
  return(readRDS(tmp))
}

log4r::info(log, "Reading Data and connect DuckDB")
fd_data_shiny <- read_bucket_RDS("projects/stat468/data/fd_data_shiny.rds")
con <- dbConnect(duckdb::duckdb())
duckdb::dbWriteTable(con, "fd_data_shiny", fd_data_shiny)
fd_data <- read_bucket_RDS("projects/stat468/data/fd_data.rds")
id <- fd_data$athlete_id
log4r::info(log, "Successfully loaded Data and connected DuckDB")

########### global var
world_class_standard_men <- 1147.174
world_class_standard_women <- 1126.130
# 
# ########### find gender func

find_gender<- function(x_id) {
  g <- fd_data_shiny[which(fd_data_shiny$athlete_link == x_id),]$gender
  if (length(g) > 0){
    return(g[1])
  } else {
    return(NA)
  }
}


######### UI

ui <- fluidPage(

    # Application title
    titlePanel("Olympic Track & Field: Athlete Comparisons and Aging Curves"),
    
    tabsetPanel(
      id = "tabs",
      
      tabPanel("Athlete Comparisons",value = "Athlete Comparisons",
           sidebarLayout(
             sidebarPanel(
               selectizeInput("event_t", "Choose Event:", choices = NULL,options = list(placeholder = "Type a event…"),
                              multiple = FALSE, selected = "10,000mW"),
               selectizeInput("athlete_t", "Choose Athletes:", choices = NULL,options = list(placeholder = "Type a name…"),
                              multiple = TRUE)
             ),
             mainPanel(
             )
           ),
          trelliscopeOutput("trelliscope", style = "height: 820px")
      ),
      
      tabPanel("Athlete Aging Curves Prediction",value = "Athlete Aging Curves Prediction",
        sidebarLayout(
            sidebarPanel(
              selectizeInput("athlete", "Choose Athlete:", choices = NULL,options = list(placeholder = "Type a name…"),
                          multiple = FALSE),
              selectizeInput("event", "Choose Event:", choices = NULL,options = list(placeholder = "Type a event…"),
                          multiple = FALSE)
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
               plotOutput("agingPlot"),
               tableOutput("selected_athlete_link")
            )
        )
      )
  )
)


############ server

server <- function(input, output, session) {
  
  ######### Tresliiscope
  
  # Give event choices
  updateSelectInput(session, "event_t",
                    choices = sort(unique(fd_data_shiny$event)))
  
  # Update athlete based on chosen event
  observeEvent(input$event_t, {
    log4r::info(log, "User selected Event: ", input$event_t)
    
    # duckDB
    query <- paste0("
              SELECT DISTINCT name
              FROM fd_data_shiny
              WHERE event = '", input$event_t, "'")
    athletes <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) {
      log4r::warn(log, "DuckDB query failed: ", conditionMessage(e))
      return(data.frame(name = character(0)))  # fallback
    })
    
    log4r::info(log, "Returned athletes for chosen event")
    
    updateSelectInput(session, "athlete_t", choices = sort(athletes$name))
  })
  
  # limit up to 20 athlete for each event
  observeEvent(input$athlete_t, {
    if (length(input$athlete_t) > 20) {
      showNotification("Please select no more than 20 athletes.", type = "error")
      
      # Reset selection to first 20
      updateSelectizeInput(session, "athlete_t",
                           selected = input$athlete_t[1:20])
    }
  })
  
  # Rendering trelliscope
  observe({
    req(input$event_t)
    req(input$athlete_t)
    req(input$tabs == "Athlete Comparisons")
    log4r::info(log, "User selected event: ",input$event)
    log4r::info(log, "User selected athletes: ",paste(input$athlete_t, collapse = ", "))
    
    # duckDB
    query <- paste0("
              SELECT *
              FROM fd_data_shiny
              WHERE name IN ('", paste(input$athlete_t, collapse = "', '"), "')
              AND event = '", input$event_t,"'")
    log4r::info(log, "Finding related datapoints")
    fd_data_shiny_filtered <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) {
      log4r::warn(log, "DuckDB query failed: ", conditionMessage(e))
      return(NULL)  # fallback
    })
    log4r::info(log, "Found related datapoints")
    
    log4r::info(log, "Starting to render trelliscope")
    tr_dir <- tempfile()
    dir.create(tr_dir)
    add_trelliscope_resource_path("trelliscope", tr_dir)
    p <- ggplot(fd_data_shiny_filtered, aes(age_years, points)) +
      geom_point(alpha = 0.6, size = 1.7, color = "red") +
      labs(
        title = NULL,
        x = "Age (years)",
        y = "IAAF points"
      ) +
      facet_panels(vars(name, event))
    tdf <- as_trelliscope_df(as_panels_df(p),
                             name = "Olympic Track and Field Athlete IAAF-points vs. Age",
                             description = "Observed IAAF points by age for each athlete-event panel",
                             path = file.path(tr_dir, "data"),
                             jsonp = FALSE)
    output$trelliscope <- renderTrelliscope({
      tdf
    })
    log4r::info(log, "Trelliscope is ready")
  })
  
  
  ######### Aging curves
  
  # Give athlete choices
  updateSelectInput(session, "athlete",
                    choices = sort(unique(fd_data_shiny$name)))
  
  # Update events based on chosen athlete link
  observeEvent(input$athlete, {
    log4r::info(log, "User selected athlete: ", input$athlete)
    
    # duckDB
    query <- paste0("
              SELECT DISTINCT event
              FROM fd_data_shiny
              WHERE name = '", input$athlete, "'")
    events <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) {
      log4r::warn(log, "DuckDB query failed: ", conditionMessage(e))
      return(data.frame(event = character(0)))  # fallback
    })
    
    log4r::info(log, "Returned events: ", paste(events$event, collapse = ", "))
    
    updateSelectInput(session, "event", choices = sort(events$event))
  })
  
  # Get corresponding link
  selected_athlete_link <- reactive({
    req(input$athlete)
    link <- unique(fd_data_shiny[which(fd_data_shiny$name == input$athlete),]$athlete_link)
    if (length(link) == 0) {
      log4r::warn(log, "No athlete link found for: ", input$athlete)
      return(NA)
    }
    log4r::info(log, "Selected athlete link: ", link)
    link
  })
  
  # Choices for event
  selected_event <- reactive({
    req(input$event)
    log4r::info(log, "Selected event: ", input$event)
    paste(input$event)
  })
  
  # Providing link to the athlete profile
  output$selected_athlete_link<- renderTable({
    link <- selected_athlete_link()
    data.frame(
      Link = sprintf("<a href='%s' target='_blank'>%s</a>", link, link)
    )
  }, sanitize.text.function = identity)
  
  # Plot aging curve
  output$agingPlot <- renderPlot({
    req(input$athlete, input$event)  # wait until both are chosen
    input_link <- selected_athlete_link()
    input_event <- selected_event()
    input_id <- paste(input_link, input_event)
    
    if (is.na(input_link) || input_event == "") {
      log4r::warn(log, "Missing athlete link or event.")
      return(NULL)
    }
    
    log4r::info(log, "Preparing API request for athlete_id: ", input_id)
    
    athlete_gender <- find_gender(input_link)
    if (is.na(athlete_gender)) {
      log4r::warn(log, "Gender not found for athlete link: ", input_link)
      athlete_gender <- "unknown"
    }
    world_class_standard <- ifelse(athlete_gender == "men", world_class_standard_men, world_class_standard_women)
    log4r::info(log, "Determined gender: ", athlete_gender)
    
    pred  <- tryCatch({
      req <- request(api_url) |>
        req_body_json(list(data = list(
              list(athlete_id = input_id)
            ))) |>
        req_headers("Content-Type" = "application/json")
      log4r::info(log, "Sending API request for athlete_id: ", input_id)
      log4r::info(log, "Sending API request for athlete_id: ", input_id)
      resp <- req_perform(req)
      fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)$prediction[[1]]
    }, error = function(e) {
      log4r::warn(log, "API call failed: ", conditionMessage(e))
      return(rep(NA, 48))  # fallback prediction
    })
    if (all(is.na(pred))) {
      log4r::warn(log, "Prediction is empty or invalid for athlete_id: ", input_id)
      return(NULL)
    }
    log4r::info(log, "Received prediction with ", length(pred), " values.")
    
    log4r::info(log, "Plotting Aging Curves")
    plot(11:58, pred,
         main = "Predicted Aging Curve with Observed Data",
         xlab = "Performance Age", ylab = "IAAF Points", 
         ylim = c(min(min(pred), min(fd_data[which(fd_data$athlete_id == input_id),]$iaaf_score), world_class_standard) - 25,
                  max(max(pred), max(fd_data[which(fd_data$athlete_id == input_id),]$iaaf_score), world_class_standard) + 25),
         col = "blue", type = "l",lty = 1)
    points(fd_data[which(fd_data$athlete_id == input_id),]$performance_age, 
           fd_data[which(fd_data$athlete_id == input_id),]$iaaf_score,col = "black", pch = 19)
    abline(h = world_class_standard, col = "red", lty = 2, lwd = 2)
    # Add legend
    legend("bottomleft",
           legend = c("Predicted Aging Curve", "Observed Data", 
                      paste("World Class Standard IAAF Points for", athlete_gender, world_class_standard)),
           col = c("blue", "black", "red"),
           lty = c(1, NA, 2),
           pch = c(NA, 19, NA),
           lwd = c(2, NA, 2),
           bty = "n")
  })
  
}


log4r::info(log, "Shiny app initialized.")
# Run the application 
shinyApp(ui = ui, server = server)







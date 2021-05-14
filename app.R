#########################################################################################
#########################################################################################
###################                                               #######################
###################           SCIENCE PULSE - SCRIPT APP          #######################
###################                                               #######################

#########################################################################################

### PACKAGES
suppressMessages(library(shiny))
suppressMessages(library(DBI))
suppressMessages(library(odbc))
suppressMessages(library(RPostgreSQL))
suppressMessages(library(pool))
suppressMessages(library(rtweet))
suppressMessages(library(tidyverse))
suppressMessages(library(dbplyr))
suppressMessages(library(rsconnect))
suppressMessages(library(lubridate))
suppressMessages(library(scales))
suppressMessages(library(DT))
suppressMessages(library(shinycssloaders))
suppressMessages(library(stringi))
suppressMessages(library(tidytext))
suppressMessages(library(ggthemes))
suppressMessages(library(sysfonts))
suppressMessages(library(pracma))

### FUNCTIONS
source("science_pulse_functions.R")

# Sanitize error messages
options(shiny.sanitize.errors = TRUE)

#########################################################################################

### UI

ui <- fluidPage(

  tags$head(includeHTML("google-analytics.html")),

  navbarPage(
    title = tags$div(tags$h1(tags$a(img(src = "logo-rosa.png", height = "", width = "130px"),
                                    href="https://sciencepulse.org/eng/", target="_blank")),
                     tags$small("(Beta Version 0.9.1)"),
                     style = "width: 150px !important"),

    theme = "custom.css",

    navbarMenu(tags$div(icon("twitter-square", class = "icons"), " Twitter Data"),
               tabPanel(tags$div(icon("fire"), " Trends"),
                        mod_trends_ui("trends")),
               tabPanel(tags$div(icon("search-plus"), " Explore"),
                        mod_explore_ui("explore")),
               tabPanel(tags$div(icon("fire-alt"), " Popularity"),
                        mod_popularity_ui("popularity")),
               tabPanel(tags$div(icon("user"), " Profiles"),
                        mod_profile_ui("profiles")),
               tabPanel(tags$div(icon("search"), " Tweets Search"),
                        mod_tweets_ui("tweets")),
               tabPanel(tags$div(icon("virus"), " Covid-19 Special"),
                        mod_covid_ui("covid"))
               ),

    tabPanel(tags$a(tags$div(class = "calltoaction",
                             icon("envelope", class = "calltoaction"), "newsletter",
                             style = "background-color:#333333;color:#fff !important;padding:5px 9px;margin-top: -17px !important;margin-left:5px;border-radius:5px"),
                    href="https://sciencepulse.org/news/", target="_blank")),

    tabPanel(tags$div(class = "calltoaction",
                      icon("bullhorn", class = "calltoaction"),
                      includeHTML("important_notes.html"),
                      style = "background-color:#ababab;color:#fff !important;padding:5px 9px;margin-top: -12px !important;margin-left:5px;border-radius:5px"))
    ),

  includeHTML("footer.html")

)

#########################################################################################

### SERVER

server <- function(input, output, session){

  ##################################
  # CONNECT TO DATABASE

  # We remove the database credentials to avoid overuse for non-app related queries
  monitor_db <- dbPool(drv = "PostgreSQL", dbname = "monitordb",
                       host = "",
                       port = , user = "", password = "")

  onStop(function() {
    poolClose(monitor_db)
  })

  ##################################

  mod_trends_server("trends", monitor_db)
  mod_explore_server("explore", monitor_db)
  mod_popularity_server("popularity", monitor_db)
  mod_profile_server("profiles", monitor_db)
  mod_tweets_server("tweets", monitor_db)
  mod_covid_server("covid", monitor_db)

}

#########################################################################################

shinyApp(ui = ui, server = server)

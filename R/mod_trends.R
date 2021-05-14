#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULE - TWITTER - TRENDS           #######################
###################                                               #######################

#########################################################################################

##### SERVER

mod_trends_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      ##### PREPARE MAIN DATASET

      trends_dataset <- reactive({

        # Load trends view
        sql_t <- "SELECT * from v_mod_trends"
        query_t <- sqlInterpolate(base, sql_t)
        trends <- as_tibble(dbGetQuery(base, query_t))

        # Language filter
        trends <- trends %>%
          filter(language == input$language)

        # Update tweets time-zone
        trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
        trends$created_at <- trends$created_at - hours(3)

        # Filter the last 12h
        trends$current_time <- lubridate::now()
        trends <- filter(trends, created_at > current_time - hours(12))

      })

      ##################################################
      ##### MAIN TRENDS

      ##########
      ### OVERPERFORMING
      output$overperform <- renderTable({
        trends_dataset() %>%
          overperforming()
      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR ON PULSE
      output$own_sample <- renderTable({

        # DISCOVERY (RT-RATIO)
        if(input$mode == "Discovery"){
          trends_dataset() %>%
            rising_popularity()
        }
        # POPULARITY
        else{
          trends_dataset() %>%
            popular_within_pulse() }
        }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### DISCOVER MORE

      # Creates sample button
      sample_table <- eventReactive(input$new_sample, {
        trends_dataset() %>%
          sample_more_than_one()
        })

      # Generate reactive table
      output$pulse_radar <- renderTable({
        if(input$new_sample == 0){
          trends_dataset() %>%
            sample_more_than_one()
          } else
            sample_table()
        }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    # Closes module
    })

}

###################################################################################################

### UI

mod_trends_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### PAGE TITLE AND DESCRIPTION
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),

             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("POPULAR TWEETS AND TRENDS", style = "text-align:center"),
                      tags$p("The best fromt Science Pulse's monitored accounts over the last 12 hours. Each column curates content according to specific criteria. Explore them and find different tweets. Read",
                             tags$a("here", href="https://sciencepulse.org/eng/methodology", target="_blank"),
                             "about our algorithms."),
                      tags$br(),
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Choose tweets\' language'),
                                  c("English" = "en",
                                    "Portuguese" = "pt",
                                    "Spanish" = "es"))
                      )),

    ### COLUNAS
    tags$div(class = "container-fluid", style = "text-align:center",
             tags$style(HTML(".control-label{float:left;margin-left:30px;margin-top:3px}")),

             column(4,

                    tags$div(tags$b("TRENDING"), icon("chart-line")),
                    tags$br(),
                    tags$p("Our main column, which shows tweets with the highest interactions, compared to what each account would generally have. This measure is designed to find content which stands out from a profile's average."),
                    tags$br(),
                    tags$br(),
                    include_spinner_large_column(ns("overperform"))

             ),

             column(4,

                    tags$div(tags$b("ON FIRE"), icon("fire")),
                    tags$br(),
                    tags$p("The most popular tweets. Choose between",
                           tags$b("Popularity,"), "which shows the most shared posts, and", tags$b("Discovery"),
                           "which shows posts with the highest RTs:followers ratio."),
                    tags$br(),
                    radioButtons(inputId = ns("mode"),
                                 label = tags$div(style="font-weight:200",
                                           icon("stream", class = "icons")," "),
                                 inline = TRUE,
                                 c("Popularity", "Discovery")),
                    include_spinner_large_column(ns("own_sample"))

             ),

             column(4,

                    tags$div(tags$b("RADAR"), icon("random")),
                    tags$br(),
                    tags$p("Random sample of tweets published by profiles monitored by Science Pulse. This column considers only posts with more than one RT. Click on the button to see new posts."),
                    actionButton(inputId = ns("new_sample"),
                                 label   = "Show new tweets"),
                    tags$br(),
                    include_spinner_large_column(ns("pulse_radar"))

             )

    ))

}

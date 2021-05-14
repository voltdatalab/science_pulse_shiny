#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULE - TWITTER - EXPLORE          #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_explore_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      ##### PREPARE MAIN DATASET

      trends_dataset <- reactive({

        # Load explore view
        sql_t <- "SELECT * from v_mod_explore"
        query_t <- sqlInterpolate(base, sql_t)
        trends <- as_tibble(dbGetQuery(base, query_t))

        # Language filter
        trends <- trends %>%
          filter(language == input$language)

        # Updates tweets time-zone
        trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
        trends$created_at <- trends$created_at - hours(3)

        # Filter last 12h
        trends$current_time <- lubridate::now()
        trends <- filter(trends, created_at > current_time - hours(12))

      })

      ##################################################
      ##### EXPLORE

      ##########
      ### MOST ACTIVE USERS
      output$active_users <- renderTable({

        trends_dataset() %>%
          active_users()

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### MOST USED HASHTAGS
      output$hashtags <- renderTable({

        hashtags <- trends_dataset() %>%
          .$hashtags

        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = str_trim(stri_trans_general(lista_hashtags, "Latin-ASCII"), "both"))
        lista_hashtags %>%
          most_hashtags()

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### ALSO POPULAR ON PULSE
      output$own_sample <- renderTable({

        trends_dataset() %>%
          also_popular()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### PULSE RADAR
      output$hidden_gems <- renderTable({

        trends_dataset() %>%
          pulse_radar()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR AMONG SCIENTISTS
      output$rts12h_overall <- renderTable({

        trends_dataset() %>%
          popular_among_scientists()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    }

  )

}

#########################################################################################

### UI

mod_explore_ui <- function(id, i18n){

  ns <- NS(id)

  tagList(

    ### PAGE TITLE AND DESCRIPTION
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),

             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("DISCOVER MORE", style = "text-align:center"),
                      tags$p("This section reveals further information from ", tags$b("TRENDING"),
                             " posts of our datasets. See other trending tweets and some hidden content from profiles that don't necessarily have many followers or engagement in their posts. You can",
                      tags$a("click here", href="https://sciencepulse.org/eng/methodology", target="_blank"),
                      " to read the methodology.")),
             selectInput(inputId = ns("language"),
                         label = tags$div(icon("language", class = "icons"), 'Choose tweets\' language'),
                         c("English" = "en",
                           "Portuguese" = "pt",
                           "Spanish" = "es"))
                      ),

    tags$div(class = "container-fluid", style = "text-align:center",

             ### TABLES TO THE LEFT
             column(2,

                    tags$p(" Most active users and hashtags in the last 12h."),
                    # Usuarios mais ativos
                    include_spinner_thin_column(ns("active_users")),
                    tags$br(),
                    # Hashtags mais usadas
                    include_spinner_thin_column(ns("hashtags"))

             ),

             ### MAIN COLUMNS

             column(1),

             column(3,

                    tags$div(tags$b("ALSO POPULAR ON PULSE"), icon("angle-double-up")),
                    tags$br(),
                    tags$p("Somewhat popular tweets in Science Pulse that did not make into the trends."),
                    include_spinner_large_column(ns("own_sample"))

                    ),

             column(3,

                    tags$div(tags$b("HIDDEN GEMS"), icon("dot-circle")),
                    tags$br(),
                    tags$p("Random sample of other popular (but usually not trending) tweets. This column is useful to find new content."),
                    include_spinner_large_column(ns("hidden_gems"))

                    ),

             column(3,

                    tags$div(tags$b("POPULAR AMONG SCIENTISTS"), icon("retweet")),
                    tags$br(),
                    tags$p("Contains popular tweets, including retweets from profiles not included in Science Pulse. Subjects may vary from science"),
                    include_spinner_large_column(ns("rts12h_overall"))

                    )

             # Close tags$div
             )

    # Close tagList
    )

}

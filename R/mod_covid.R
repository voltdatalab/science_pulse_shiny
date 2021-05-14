#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULE - TWITTER - COVID            #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_covid_server <- function(id, base){

  shiny::moduleServer(
    id,
    function(input, output, session){

      ##################################################
      ##### PREPARE MAIN DATASET

      trends_dataset <- reactive({

        # Load covid view
        sql_t <- "SELECT * from v_mod_covid"
        query_t <- sqlInterpolate(base, sql_t)
        trends <- as_tibble(dbGetQuery(base, query_t))

        # Language filter
        trends <- trends %>%
          filter(language == input$language)

        # Update tweets time-zone
        trends$created_at <- as.POSIXct(trends$created_at, format="%Y-%m-%d %H:%M:%S")
        trends$created_at <- trends$created_at - hours(3)

        # Filter last 24h
        trends$current_time <- lubridate::now()
        trends <- filter(trends, created_at > current_time - hours(12))

        # Lists covid keywords
        keywords <- c("Covid", "covid", "Coronavirus", "coronavirus",
                      "Corona", "corona", "SARS-CoV-2", "Sars-CoV-2",
                      "SRAG", "sindrome", "syndrome", "pandemic",
                      "pandemia", "WHO", "OMS", "quarantine", "social distancing",
                      "quarentena", "isolamento social", "distanciamento social",
                      "mascara", "mask", "distanciamiento social", "spread",
                      "asymptomatic", "epidemic", "outbreak", "epidemia",
                      "vacina", "vaccine", "wuhan", "Wuhan", "herd immunity",
                      "imunidade de rebanho", "imunidade coletiva", "lockdown",
                      "blood clot", "coágulo", "AstraZeneca", "Astrazeneca",
                      "astrazeneca", "Coronovac", "CoronoVac", "coronavac",
                      "Janssen", "janssen", "Sputnik", "sputnik",
                      "máscara", "mascara", "mask")

        # Filter tweets with at least one of the keywords
        trends <- filter(trends,
                         str_detect(text, paste(keywords, collapse = "|")))

      })

      ##################################################
      ##### COVID-19 TRENDS

      ##########
      ### MOST ACTIVE USERS
      output$active_users <- renderTable({

        trends_dataset() %>%
          active_users()

        }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### MOST TWEETED HASHTAGS
      output$hashtags <- renderTable({

        # Exclude common hashtags
        exclude_hashtags <- c("coronavirus", "coronaviruses", "covid", "covid__19",
                              "covid_19", "covidー19", "covid-19", "covid19",
                              "covid19brasil", "covid19ma", "covid2019",
                              "covid9", "pandemia", "pandemias", "pandemic",
                              "pandemic2020", "sars_cov_2", "sarscov2")

        # Extract hashtags
        hashtags <- trends_dataset() %>%
          .$hashtags
        lista_hashtags <- unlist(str_split(hashtags, ","))
        lista_hashtags <- tibble(hashtag = tolower(
          str_trim(stri_trans_general(lista_hashtags, "Latin-ASCII"), "both")))
        # Exclude common and count the rest
        lista_hashtags %>%
          filter(!hashtag %in% exclude_hashtags) %>%
          most_hashtags()

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

      ##########
      ### OVERPERFORMING
      output$overperform <- renderTable({

        trends_dataset() %>%
          overperforming()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR WITHIN PULSE
      output$own_sample <- renderTable({

        # DISCOVERY (RT-RATIO)
        if(input$mode == "Discovery"){

          trends_dataset() %>%
            rising_popularity()
        }
        # POPULARITY
        else {

          trends_dataset() %>%
            popular_within_pulse() }

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

      ##########
      ### POPULAR AMONG SCIENTISTS
      output$rts12h_sample <- renderTable({

        trends_dataset() %>%
          popular_among_scientists()

      }, sanitize.text.function = function(x) x, striped = FALSE, target="_blank")

    # Closes module
    })

}

###################################################################################################

### UI

mod_covid_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### PAGE TITLE AND DESCRIPTION
    tags$div(class = "sheet_topper",
             img(src = "header-pulse.svg", height = "", width = "100%"),

             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("COVID-19 SPECIAL", style = "text-align:center"),
                      tags$p(" Tables in this tab show trends on tweets, hashtags and active users regarding Covid-19 keywords topics in the last 12h. You can",
                             tags$a("click here", href="https://sciencepulse.org/eng/methodology", target="_blank"),
                             "to read the methodology."),
                      tags$br(),
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Choose tweets\' language'),
                                  c("English" = "en",
                                    "Portuguese" = "pt",
                                    "Spanish" = "es"))
             )),

    tags$div(class = "container-fluid", style = "text-align:center",

             ### LEFT-SIDE TABLE
             column(2,

                    tags$p("Most active users and hashtags in the last 12h."),

                    # MOST ACTIVE USERS
                    include_spinner_thin_column(ns("active_users")),
                    tags$br(),

                    # MOST USED HASHTAGS
                    include_spinner_thin_column(ns("hashtags")),

                    # Tables' caption
                    tags$em("Common hashtags, such as #COVID or #COVID19, were not considered.")),

             ### MAIN TABLES
             column(1),

             column(3,

                    tags$div(tags$b("TRENDING"), icon("chart-line")),
                    tags$br(),
                    tags$p("Shows tweets with the highest interactions, compared to what each account would generally have. This measure is designed to find content which stands out from a profile's average."),
                    include_spinner_large_column(ns("overperform"))

             ),

             column(3,

                    tags$div(tags$b("ON FIRE"), icon("fire")),
                    tags$br(),
                    tags$style(HTML(".control-label{float:left;margin-left:30px;margin-top:3px}")),
                    tags$p("The most popular tweets. Choose between",
                           tags$b("Popularity,"), "which shows the most shared posts, and", tags$b("Discovery"),
                           "which shows posts with the highest RTs:followers ratio."),
                    radioButtons(inputId = ns("mode"),
                                 label = tags$div(style="font-weight:200",
                                                  icon("stream", class = "icons")),
                                 inline = TRUE,
                                 c("Popularity", "Discovery")),
                    include_spinner_large_column(ns("own_sample"))

                    ),

             column(3,

                    tags$div(tags$b("POPULAR AMONG SCIENTISTS"), icon("retweet")),
                    tags$br(),
                    tags$p("Contains tweets most shared by Science Pulse's monitored profiles, including retweets from profiles not included in our list of tracked accounts."),
                    tags$br(),
                    include_spinner_large_column(ns("rts12h_sample"))

                    )

             # Closes tags$div
             )

    # Closes tagList
    )

}

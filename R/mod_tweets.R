#########################################################################################
#########################################################################################
###################                                               #######################
###################           MODULE - TWITTER - TABLE            #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_tweets_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      ##### PREPARE MAIN DATASET

      tweets_dataset <- reactive({

        # Load tweets procedure
        sql <- "select * from p_mod_tweets_1(?datum, ?datum2, ?language)"
        query  <- sqlInterpolate(base, sql, language = input$language,
                                 datum = input$date[1], datum2 = input$date[2])
        tweets <- as_tibble(dbGetQuery(base, query))

        # Update tweets time-zone
        tweets$created_at <- tweets$created_at - hours(3)

        # Create is_replied column
        tweets$is_replied <- ifelse(is.na(tweets$reply_to_screen_name), "FALSE", "TRUE")

        # Order by created_at column (most recent)
        tweets <- tweets %>% arrange(desc(created_at))

      })

      ##########
      # MESSAGE DESCRIBING THE SELECTION

      output$basics <- renderText({

        user_n <- tweets_dataset()

        # Filters: verified, RTs and replies
        if (input$verified != "All profiles") {
          user_n <- user_n[user_n$verified == input$verified,]
        }
        if (input$rts != "Show") {
          user_n <- user_n[user_n$is_retweet == input$rts,]
        }
        if (input$reply != "Show") {
          user_n <- user_n[user_n$is_replied == input$reply,]
        }

        # Main description and prints message
        n_profiles <- user_n %>%
          summarise(count = n_distinct(screen_name))
        n_tweets <- user_n %>%
          summarise(count = n_distinct(status_id)) %>%
          mutate(count = format(count, big.mark = ',', scientific = FALSE))

        paste("Your filters resulted in", n_tweets, "tweets from", n_profiles$count, "profiles.")

      })

      ##########
      # TABELA PRINCIPAL

      output$table <- DT::renderDataTable(DT::datatable({

        main_table <- tweets_dataset()

        # Filters: verified, RTs and replies
        if (input$verified != "All profiles") {
          main_table <- main_table[main_table$verified == input$verified,]
        }
        if (input$rts != "Show") {
          main_table <- main_table[main_table$is_retweet == input$rts,]
        }
        if (input$reply != "Show") {
          main_table <- main_table[main_table$is_replied == input$reply,]
        }

        # Generates main table
        main_table <- main_table %>%
          # Creates columns linking to tweets and profiles
          mutate(link_tweet = if_else(is_retweet == "TRUE",
                                      paste0("<i class='fas fa-retweet fa-lg' style='color:#231f20'></i> ",
                                             text, "<a href='https://twitter.com/", screen_name,"/status/",
                                             status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>"),
                                      if_else(is_replied == "TRUE",
                                              paste0("<i class='fas fa-reply fa-lg' style='color:#231f20'></i> ",
                                                     text, "<a href='https://twitter.com/", screen_name,"/status/",
                                                     status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a>"),
                                              paste0(text, "<a href='https://twitter.com/", screen_name, "/status/",
                                                     status_id,"' target='_blank'> <i class='fas fa-link fa-xs'></i></a></a>")))
                 ) %>%
          mutate(handle = if_else(verified == "TRUE",
                                  paste0(name,
                                         " <i class='fas fa-certificate fa-xs' style='color:rgba(29,161,242,1.00)'></i> <br><a href='https://twitter.com/",
                                         screen_name,"' target='_blank'>@", screen_name, "</a>"),
                                  paste0(name, "<br><a href='https://twitter.com/", screen_name,
                                         "' target='_blank'>@", screen_name, "</a>"))
                 ) %>%
          # Adjust formats
          mutate(retweet_count = format(retweet_count, big.mark = ',', scientific = FALSE)) %>%
          # Select columns and rename those that should appear
          select(name, verified, is_retweet, handle, screen_name, retweet_count, language,
                 created_at, text, link_tweet, status_id, reply_to_screen_name) %>%
          rename("Profile" = handle, "Date" = created_at, "RTs" = retweet_count, "Tweet" = link_tweet)

        main_table

      }, escape = FALSE,

      # TABLE GENERAL CONFIG
      options = list(
        language = list(searchPlaceholder = "Search for keyword...", sSearch = ""),
        pageLength = 100,
        dom = "ftipr",
        #fixedHeader= TRUE,
        searchHighlight = TRUE,
        info = FALSE,
        lengthMenu = list(c(10, 50, 100, 1000), c('10', '50', '100', '1000')),
        columnDefs = list(
          list(visible = FALSE,    targets = c(1,2,3,5,7,9,11,12)),
          list(width   = c("8%"),  targets = c(6)),
          list(width   = c("22%"), targets = c(4))
        ))

      # Closes DT::datatable
      ) %>%

        # Correct date format
        formatDate("Date", "toLocaleString"))

    # Closes module
    }

# Closes server
)}

###################################################################################################

### UI

mod_tweets_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### PAGE TITLE AND DESCRIPTION
    tags$div(class = "sheet_topper",
             #img(src = "header-pulse.svg", height = "", width = "100%"),
             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("FIND TWEETS", style = "text-align:center"),
                      tags$p("In this page you can search for tweets in Science Pulse's database. You can apply several filters to narrow your discovery. Table is updated with new tweets and counts every 20 minutes. Since for now we stop updating older tweets, exact counts may vary from this table to actual tweets."),
                      tags$br(),
                      selectInput(inputId = ns("language"),
                                  label = tags$div(icon("language", class = "icons"), 'Choose tweets\' language'),
                                  c("English" = "en",
                                    "Portuguese" = "pt",
                                    "Spanish" = "es"))
             )),

    ### LEFT-SIDE COLUMNS: filters
    tags$div(class="filters",

             column(3,

                    dateRangeInput(inputId = ns("date"),
                                   label = tags$div(icon("calendar", class = "icons"),
                                                    'Choose dates (d/m/y)',
                                                    tags$p("Searches are limited to 90 days",
                                                           style="font-weight:300")),
                                   start = Sys.Date()-3,  end = Sys.Date(),
                                   min = Sys.Date()-90,   max = Sys.Date(),
                                   format = "mm/dd/yyyy", weekstart = 0,
                                   language = "en",       separator = " to ",
                                   width = NULL,          autoclose = TRUE),
                    tags$br(),

                    selectInput(inputId = ns("verified"),
                                label = tags$div(icon("certificate", class = "icons"),
                                                 'Filter profiles verified by Twitter'),
                                c("All profiles",
                                  "Only verified" = "TRUE")),

                    selectInput(inputId = ns("rts"),
                                label = tags$div(icon("retweet", class = "icons"),
                                                 'Filter retweets'),
                                c("Show",
                                  "Hide" = "FALSE")),

                    selectInput(inputId = ns("reply"),
                                label = tags$div(icon("reply", class = "icons"),
                                                 'Filter replies to posts'),
                                c("Show",
                                  "Hide" = "FALSE"))
             # Closes columns
             )

    # Closes filters
    ),

    ### MAIN TABLE
    column(5,
           tags$p(include_spinner_small(ns("basics"))),
           include_spinner_tables(ns("table"))
           )

    # Closes TagList
    )

}

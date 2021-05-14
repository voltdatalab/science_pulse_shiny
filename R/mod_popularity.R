#########################################################################################
#########################################################################################
###################                                               #######################
###################       MODULE - TWITTER - POPULARITY           #######################
###################                                               #######################

###################################################################################################

### SERVER

mod_popularity_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Add font for graph
      font_add_google("Barlow", "barlow")

      ##################################################
      ##### PREPARE MAIN DATASET

      selectedData <- reactive({

        sql_t <- "SELECT * from v_tx
                  WHERE created_at >= NOW() - INTERVAL '181 days' "
        query_t <- sqlInterpolate(base, sql_t)
        dados_grafico <- as_tibble(dbGetQuery(base, query_t))
        return(dados_grafico)

      })


      # Create selectizeInput with screen_names from the dataset
      output$name_choices <- renderUI({

        ns <- session$ns

        nomes <- selectedData()

        nomes <- nomes %>%
          arrange(desc(tx_engajamento)) %>%
          select(screen_name) %>%
          unique() %>%
          .$screen_name

        selectizeInput(inputId = ns("screen_name"),
                       label = "Choose up to 4 users",
                       selected = "WHO",
                       multiple = TRUE,
                       width = "80%",
                       choices = nomes,
                       options = list(maxItems = 4))

      })

      ####################
      # CHART WITH USER POPULARITY

      output$graf_engajamento <- renderPlot({

        grafico <- selectedData()

        grafico %>%
          # Filter by date and screen_name
          filter(created_at >= input$created_at[1] & created_at <= input$created_at[2],
                 str_detect(screen_name, paste(input$screen_name, collapse = "|"))) %>%
          # Calculate rolling average by screen_name
          group_by(screen_name) %>%
          mutate(ma = movavg(round(tx_engajamento, 1), input$medmovel, "e")) %>%
          ungroup() %>%
          # Change created_at to as.POSIXct and generate graph
          mutate(created_at = as.POSIXct(created_at)) %>%
          ggplot(aes(created_at, get(input$metrica), colour = screen_name)) +
          geom_step() +
          geom_smooth(alpha = 0.4,  se = FALSE, span = 1) +
          scale_x_datetime(breaks = scales::pretty_breaks(n = 6),
                           labels = date_format("%m/%d/%Y")) +
          scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE),
                             breaks = scales::pretty_breaks(n = 6)) +
          labs(x="",
               y = ifelse(input$metrica == "qtd_tweets", "n. of tweets",
                         ifelse(input$metrica == "tx_engajamento", "interactions/tweet",
                                ifelse(input$metrica == "ma", "interactions/tweet",
                                       ifelse(input$metrica == "interacoes", "likes + RTs"))))) +
          sp_theme() +
          scale_color_manual(values = c("#4b31dd", "#f4346f", "#ffb14e", "#159e84",
                                        "#231f20", "#e2a805", "#000000", "#ad7cc8",
                                        "#ad0b13","#2283aa", "#f47018", "#1a6429")) +
          guides(colour = guide_legend(nrow = 2))

      }, height = 400)

      # Creates output for changes in rolling average days
      output$display_media_movel <- renderText({
        paste("", input$medmovel,"days.")
      })

    # Closes module
    })

}

###################################################################################################

### UI

mod_popularity_ui <- function(id){

  ns <- NS(id)

  tagList(
    # Page config
    tags$div(class = "container-fluid", style = "text-align:center;padding-bottom: 50px",

             # INPUTS
             column(3,

                    tags$div(class = "sheet_header", style = "",
                             tags$h1("PROFILES' POPULARITY", style = "text-align:center"),
                             tags$p("In this page you can see the popularity of certain users monitored by Science Pulse. You can search for up to 4 Twitter handles in an 180-days interval.",
                                    style = "font-family: 'Roboto Mono', monospace"),
                             tags$br()),

                    # Date range input
                    dateRangeInput(inputId = ns("created_at"),
                                   label = tags$div(icon("calendar", class = "icons"),
                                                    'Choose dates (m/d/y)',
                                                    tags$p("Searches are limited to 180 days",
                                                           style="font-weight:300")),
                                   start = Sys.Date()-30,  end = Sys.Date(),
                                   min = Sys.Date()-180,   max = Sys.Date(),
                                   format = "mm/dd/yyyy", weekstart = 0,
                                   language = "en",       separator = " to ",
                                   width = NULL,          autoclose = TRUE),

                    # Metric input
                    selectInput(inputId = ns("metrica"),
                                label = tags$div(icon("tachometer-alt", class = "icons"),
                                                 'Choose one index'),
                                choices = c("Engagement trend"         = "ma",
                                            "Engagement rate"          = "tx_engajamento",
                                            "Interactions' total"      = "interacoes",
                                            "Number of tweets"         = "qtd_tweets"),
                                selected = "ma", multiple = FALSE),

                    # Conditional panel for rolling average
                    conditionalPanel("input.metrica == 'ma'", ns = ns,
                                     sliderInput(inputId = ns("medmovel"),
                                                 label = "Rolling average (days)",
                                                 min = 2, max = 15,
                                                 value = 7, step = 1)
                    ),

                    # Caption
                    tags$div(class = "explain"),
                    tags$div(class = "explain"),
                    tags$h5(tags$b("Source:"), "Twitter API and analysis by Science Pulse.")
                    ),

             ### GRAPH
             column(5,

                    # selectizeInput with screen_names
                    uiOutput(ns('name_choices')),

                    ### Conditional panels describing each metric

                    # Engagement trend
                    conditionalPanel("input.metrica == 'ma'", ns = ns,
                                     class="header",
                                     tags$head(tags$style("
                                   #display_media_movel{
                                   display:inline;
                                   text-decoration: underline;
                                   }")),
                                     tags$h1("ENGAGEMENT TREND"),
                                     tags$div(tags$h4("The smoothed line represents the daily engagement trend, according to the selected rolling average of", textOutput(ns("display_media_movel")), style="display:inline"))
                    ),

                    # Engagement rate
                    conditionalPanel("input.metrica == 'tx_engajamento'", ns = ns,
                                     class="header",
                                     tags$h1("ENGAGEMENT RATE"),
                                     tags$h4("The smoothed line represents the daily engagement trend, considering a simple average.")
                    ),

                    # Interactions
                    conditionalPanel("input.metrica == 'interacoes'", ns = ns,
                                     class="header",
                                     tags$h1("TWEETS' INTERACTIONS"),
                                     tags$h4("The smoothed line represents the number of interactions' daily trend, considering a profile's sum of interactions in a day.")
                    ),

                    # Posted tweets
                    conditionalPanel("input.metrica == 'qtd_tweets'", ns = ns,
                                     class="header",
                                     tags$h1("N. OF POSTED TWEETS"),
                                     tags$h4("The smoothed line represents the trend of tweets posted daily, considering a profile's sum in a day.")
                    ),

                    # PLOT GRAPH
                    withSpinner(plotOutput(ns('graf_engajamento'), width = "100%"),
                                type = getOption("spinner.type", default = 6),
                                color = getOption("spinner.color", default = "#d91c5c"),
                                size = getOption("spinner.size", default = 1),
                                color.background = getOption("spinner.color.background", default = "#d91c5c"),
                                custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", plotOutput(ns('graf_engajamento')))) NULL else "300px")

             # Closes column
             ),

    # Closes tags$div container-fluid
    )
  # Closes TagList
  )

}

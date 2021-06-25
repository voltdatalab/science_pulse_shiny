#########################################################################################
#########################################################################################
###################                                               #######################
###################            MODULE - TWITTER - PROFILES        #######################
###################                                               #######################

#########################################################################################

### SERVER

mod_profile_server <- function(id, base) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ##################################################
      # LOAD EXPERTS DATA
      experts <- as_tibble(dbGetQuery(base, "select name_clean,screen_name,user_id,
                                                    gender,type_ontology,
                                                    google_scholar,known_affiliations,
                                                    group1, group2,
                                                    main_general_field, 
                                                    ver_location,verified,description,
                                                    below_median,followers_count from v_experts_form_data_1"))

      # Remove emojis and trim name column
      experts$name <- gsub('\\p{So}|\\p{Cn}', trimws(''),
                           experts$name_clean, perl = TRUE)

      # Creates filtro_tipo_genero column to be used to filter
      experts <- experts %>%
        mutate(filtro_tipo_genero = ifelse(type_ontology == "Person",
                                           gender, type_ontology))

      ##########
      ### PROFILES TABLE
      output$profiles <- DT::renderDataTable(DT::datatable({

        # FILTERS: apply filters on main table
        experts_tbl <- experts %>%
          filter(followers_count > input$followers,
                 str_detect(filtro_tipo_genero, paste(c(input$type, input$gender), collapse = "|")),
                 str_detect(main_general_field, paste(input$field, collapse = "|")))

        if(input$verified == "TRUE"){

          experts_tbl <- experts_tbl %>%
            filter(verified == "TRUE")

          }

        # CREATES MAIN TABLE
        experts_tbl <- experts_tbl %>%
          # Transforms name column to show verified stamp and link the account
          mutate(name = if_else(str_detect(verified, "TRUE"),
                                paste0(name, " <i class='fas fa-certificate fa-xs' style='color:rgba(29,161,242,1.00)'></i> <br><a href='https://twitter.com/",
                                       screen_name,"' target='_blank'>@", screen_name, "</a>"),
                                paste0(name, "<br><a href='https://twitter.com/", screen_name,"' target='_blank'>@", screen_name, "</a>"))
          ) %>%
          # Transform affiliation column to include Google Scholar link, when it exists
          mutate(known_affiliations = if_else(!is.na(google_scholar),
                                              paste0("<a href='", google_scholar, "' target='_blank'>", known_affiliations, "</a>"),
                                              known_affiliations)) %>%
          # Order by alphabetical order
          arrange(name) %>%
          # Format followers_count column
          mutate(followers_count = format(followers_count, big.mark = ',', scientific = FALSE)) %>%
          # Select and rename final columns
          select(verified, google_scholar, below_median, screen_name, type_ontology,
                 name, gender, main_general_field, known_affiliations,
                 description, ver_location, followers_count) %>%
          rename(type = type_ontology,
                 `field of study` = main_general_field,
                 `institutional affiliation` = known_affiliations,
                 location = ver_location,
                 followers = followers_count)

        experts_tbl

      }, escape = FALSE,

      # TABLE GENERAL CONFIG.
      options = list(
        language = list(searchPlaceholder = "Search for keyword...", sSearch = ""),
        dom = "ftipr",
        pageLength = 100,
        searchHighlight = TRUE,
        info = FALSE,
        lengthMenu = list(c(10, 50, 100, 1000), c('10', '50', '100', '1000')),
        columnDefs = list(
          list(visible = FALSE, targets = c(1:5)),
          list(width = '150px', targets = c(2,7))

        # Closes options
        ))

      # Closes renderDataTable
      ))

      ##########
      ### COUNT NUMBER OF SELECTED PROFILES

      output$experts_count <- renderText({

        # Filtra a base principal
        experts_count <- experts %>%
          filter(followers_count > input$followers,
                 str_detect(filtro_tipo_genero, paste(c(input$type, input$gender), collapse = "|")),
                 str_detect(main_general_field, paste(input$field, collapse = "|")))

        if(input$verified == "TRUE"){

          experts_count <- experts_count %>%
            filter(verified == "TRUE")

        }

        # Count the number of profiles
        experts_count <- experts_count %>%
          count()

        # Prints message
        paste("There's a total of", experts_count$n, "selected Twitter profiles.")

      })

      ##########
      ### FIND NEW EXPERTS
      #   Random sample from 5 of the least followed profiles

      output$new_users <- renderTable({

        experts %>%
          # Filter only pages below the median of followers
          filter(below_median == T) %>%
          # Creates strata column binding group characteristics
          mutate(group3 = paste(group2, group1)) %>%
          # Sample one profile from each category
          group_by(group3) %>%
          sample_n(size = case_when(
            group3 == "TRUE TRUE"   ~ 1,
            group3 == "TRUE FALSE"  ~ 1,
            group3 == "FALSE TRUE"  ~ 1,
            group3 == "FALSE FALSE" ~ 1,
            group3 == "NA NA"       ~ 1)
            ) %>%
          ungroup() %>%
          # Order by showing non-male non-whites first
          arrange(desc(group3)) %>%
          select(name, description, screen_name) %>%
          mutate(value_display = paste0("<h3 style='margin-top:0'><p><b>", name, "</b></p>",
                                        "@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>",
                                        screen_name, "</a></h3>",
                                        "<p style='margin-botom:10px'><em>", description, "</em></p>")) %>%
          select(value_display) %>%
          rename(" " = value_display)

      }, sanitize.text.function = function(x) x, striped = TRUE, target="_blank")

  # Closes module
  }

# Closes server
)}


###################################################################################################

### UI

mod_profile_ui <- function(id){

  ns <- NS(id)

  tagList(

    ### PAGE TITLE AND DESCRIPTION
    tags$div(class = "sheet_topper",
             #img(src = "header-pulse.svg", height = "", width = "100%"),
             tags$div(class = "sheet_header", style = "font-family: 'Roboto Mono', monospace",
                      tags$h1("WHO WE FOLLOW ON TWITTER", style = "text-align:center"),
                      tags$p("In this page you can search for the profiles of scientists, experts, universities, organizations and scientific initiatives that are the core of the",
                             tags$b("Science Pulse."), " You can download this table",
                      tags$a(href="https://docs.google.com/spreadsheets/d/11W4Sw3M4pJ12yolY03UsU_lUDjnuaOYLYcldy4xZPfg/edit?usp=sharing",
                             target="_blank", "here."),
                      tags$br()
                      ))),

    ### LEFT-SIDE COLUMNS: filters and find new specialists
    tags$div(class="filters",
             column(3,

                    # Type
                    selectInput(inputId = ns("type"),
                                label = tags$div(icon("university", class = "icons"),
                                                 'Filter by type'),
                                choices = c("Institution", "Person"),
                                multiple = T),

                    # Gender
                    conditionalPanel(
                      condition = "input.type.indexOf('Person') > -1", ns = ns,

                      selectInput(inputId = ns("gender"),
                                  label = tags$div(icon("user", class = "icons"),
                                                   'Filter by gender'),
                                  choices = c("Female", "Male", "Other", "Not specified"),
                                  multiple = T)

                      ),

                    # Field of study
                    selectInput(inputId = ns("field"),
                                label = tags$div(icon("graduation-cap", class = "icons"),
                                                 'Filter by research field'),
                                choices = c("Life Sciences", "Exact Sciences",
                                            "Human Sciences", "Not specified"),
                                multiple = T),

                    # Verified
                    selectInput(inputId = ns("verified"),
                                label = tags$div(icon("certificate", class = "icons"),
                                                 'Filter profiles verified by Twitter'),
                                choices = c("All profiles",
                                            "Only verified" = "TRUE")),

                    # Minimum n. of followers
                    numericInput(inputId = ns("followers"),
                                 label = tags$div(icon("users", class = "icons"),
                                                  'Minimum number of followers'),
                                 value = 0, step = NA,
                                 min = 0, max = NA,
                                 width = "50px"),

                    # Find new specialists
                    tags$br(),
                    tags$p(tags$b("Find New Experts"), style = "text-align:center"),
                    include_spinner_thin_column(ns("new_users"))
                    )
             ),

    ### MAIN TABLE
    column(5,
           include_spinner_small(ns("experts_count")),
           tags$br(),
           include_spinner_tables(ns("profiles"))
           )

  )
}

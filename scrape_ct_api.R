#########################################################################################
#########################################################################################
########################                                          #######################
########################       SCRAPING POSTS - CROWDTANGLE       #######################
########################                                          #######################

# Scrapes overperfoming posts, according to CrowdTangle.

#########################################################################################

### Packages
suppressMessages(library(httr))

### Functions

# Extract data from the API
ct_request <- function(link){

  get_request <- httr::GET(link,
                           httr::add_headers(`x-api-token` = "8Nml6fseY4d7uZzzo9WCCDWyil1XnE1icSk2eCE9"))

  ct_request_data <- httr::content(get_request, as = "text", encoding = "utf8")

  banco <- jsonlite::fromJSON(ct_request_data)

}

### CT ID lists
# BR science publications and universities = 1424716
# US science publications and universities = 1424712

#########################################################################################

#############################
##### SCRAPING FUNCTIONS

### Scrapes pages based on a date

scrape_ct_last24h <- function(){

  # Object which will contain all posts
  posts_final <- NULL

  ## FIRST REQUEST

  # We have to do it before the loop, because it gives our first dataset and,
  # more importantly, the information for the next page of the search, which
  # is fundamental for the loop.
  # Since each page has a maximum of 100 results, we need the loop to collect
  # info from many pages. Since we do not have the complete n. of posts, we
  # also use a repeat loop until it breaks.

  # Set a start date for data collection
  startDate = as.Date(lubridate::now()-lubridate::hours(24))

  # API request with the first 100 results and link for next results
  posts_request <- ct_request(paste0('https://api.crowdtangle.com/posts?listIds=1424716,1424712&startDate=',
                                     startDate, '&count=100'))[[2]]

  repeat{

    Sys.sleep(0.135)
    # Extract nested datasets with actual statistics from each post
    posts_statistics <- posts_request[[1]]$statistics$actual

    Sys.sleep(0.135)
    # Extract dataset with information from the account who authored the post
    posts_account    <- posts_request[[1]]$account %>%
      rename(account_id = id,
             account_platform_id = platformId,
             account_subscriberCount = subscriberCount) %>%
      select(-platform)

    Sys.sleep(0.135)
    # Extract dataset of posts and add the statistics and account info
    posts_pagina <- posts_request[[1]] %>%
      rename(ct_id = id) %>%
      select(-c(statistics, account, expandedLinks, media)) %>%
      cbind.data.frame(posts_statistics, posts_account) %>%
      # Detect posts' language
      mutate(message_detect_language = ifelse(is.na(message), description, message),
             language = cld2::detect_language(message_detect_language))

    Sys.sleep(0.135)
    # Bind datasets from each loop iteration
    posts_final <- bind_rows(posts_final, posts_pagina)

    Sys.sleep(0.135)
    # Create an object with the link for the next page of the search
    link_nextpage <- posts_request$pagination$nextPage

    Sys.sleep(0.135)
    # If there's no link for a next page, break the loop
    # When the loop breaks, it means that we extracted all data.
    if (is.null(link_nextpage)){
      break
    }

    Sys.sleep(0.8)
    # Request info from next page
    posts_request <- ct_request(link_nextpage)[[2]]

  }

  # Returns the complete dataset, after scraping all posts
  return(posts_final)

}

#########################################################################################

#############################
##### LEADERBOARD FUNCTION

# Scrape data on pages included on Science Pulse

scrape_leaderboard_1wk <- function(){

  # To scrape the list's leaderboard, we cannot scrape more than 1 list at a time
  # (as we can do for the posts). So, I nested a repeat loop inside a for loop.
  # The 'for' loop defines the list and the 'repeat' loop, the iterations
  # inside each of them.
  lists_ids <- c(1424716, 1424712)

  # Object which will contain the full leaderboard
  leaderboard_final <- NULL

  for(lista in lists_ids){

    # Set a start date for data collection
    startDate = as.Date(lubridate::now()-lubridate::hours(168))

    # Request leaderboard of public pages
    leaderboard_request <- ct_request(paste0('https://api.crowdtangle.com/leaderboard?listId=',
                                             lista, '&startDate=', startDate,
                                             '&count=100&orderBy=desc'))[[2]]

    # Scrape leaderboard from the iteration list
    repeat{

      Sys.sleep(0.135)
      # Extract dataset with general info, summary and subscribe counts from accounts
      leaderboard_account    <- leaderboard_request[[1]]$account
      leaderboard_summary    <- leaderboard_request[[1]]$summary
      leaderboard_subscriber <- leaderboard_request[[1]]$subscriberData

      Sys.sleep(0.135)
      # Bind columns of each dataset to form the full leaderboard
      leaderboard_dataset <- cbind.data.frame(leaderboard_account,
                                              leaderboard_summary,
                                              leaderboard_subscriber)

      # Bind the iteration dataset with the full leaderboard dataset
      leaderboard_final <- bind_rows(leaderboard_final, leaderboard_dataset)

      Sys.sleep(0.135)
      # Create an object with the link for the next page of the search
      link_nextpage <- leaderboard_request$pagination$nextPage

      Sys.sleep(0.135)
      # If there's no link for a next page, break the loop
      # When the loop breaks, it means that we extracted all data.
      if (is.null(link_nextpage)){
        break
      }

      Sys.sleep(0.5)
      # Request info from next page
      leaderboard_request <- ct_request(link_nextpage)[[2]]

    }

  }

  # Returns the full leaderboard, after scraping all data
  return(leaderboard_final)

}


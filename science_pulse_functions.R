#########################################################################################
#########################################################################################
###########################                                   ###########################
###########################     SCIENCE PULSE'S FUNCTIONS     ###########################
###########################                                   ###########################

#########################################################################################

### POPULAR WITHIN PULSE

# Among tweets in the sample, which ones were more RT (by the whole universe
# of twitter users)? Count includes RTs by users outside Pulse. However,
# original tweets were posted only by monitored accounts.

popular_within_pulse <- function(dataset){

  show_dataset <- dataset %>%
    filter(is_retweet == F)

  # Not enough tweets message, if necessary
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_en()

  } else {

    show_dataset %>%
      # Slice only the tweet with the most RTs from each user
      group_by(screen_name) %>%
      arrange(desc(retweet_count), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Slice those 5 with the most RTs
      arrange(desc(retweet_count), created_at) %>%
      slice(1:5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
                           )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### RT-RATIO

# Among tweets in the sample (posted in the sample), which ones have the highest
# RT/followers ratio? Count includes RTs by users outside Pulse. However, original
# tweets were posted only by monitored accounts. Excludes tweets with less than 2 RTs,
# to avoid irrelevant tweets from users with a low number of followers from appearing.

rising_popularity <- function(dataset){

  show_dataset <- dataset %>%
    # Filter tweets with 1+ RTs and creates ratio column
    filter(is_retweet == F,
           retweet_count > 1) %>%
    mutate(followers_count = as.numeric(followers_count),
           retweet_count   = as.numeric(retweet_count),
           ratio = retweet_count/followers_count)

  # Not enough tweets message, if necessary
  if(nrow(show_dataset) == 0) {
    not_enough_tweets_en()

  } else {

    show_dataset %>%
      # Slice only the tweet with the highest ratio from each user
      group_by(screen_name) %>%
      arrange(desc(ratio), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Slice those 5 with the highest ratios
      arrange(desc(ratio), created_at) %>%
      slice(1:5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
                           )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### OVERPERFORMING

# Identify posts with the highest number of interactions, considering the usual n.
# of interactions of that account along a large period of time. Thus, it considers
# the weigthed sum of RTs and likes (a RT is more "valuable" than a like), from
# each post and compares it with an average from the last posts from that account.
# There's also a "penalty" for accounts with less followers, to avoid that the
# final score is only a result from a small number of tweets.
# This measure is inspired by CrowdTangle's overperforming metric.

overperforming <- function(dataset){

  show_dataset <- dataset %>%
    # Filter tweets with more than 2 RTs
    filter(is_retweet == F,
           retweet_count > 2)

  # Not enough tweets message, if necessary
  if(nrow(show_dataset) == 0) {

    not_enough_tweets_pt()

  } else {

    show_dataset %>%
      # Slice only the tweet with the highest final_score from each user
      group_by(screen_name) %>%
      arrange(desc(final_score), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Slice those 5 with the highest final_scores
      arrange(desc(final_score), created_at) %>%
      slice(1:5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
                           )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### SAMPLE TWEETS WITH 1+ RTS

# Random sample of tweets with more than 1 RT from 3 profiles' strata:
# (1) non-male; (2) instituions; (3) males.

sample_more_than_one <- function(dataset){

  dataset %>%
    # Filter tweets with more than 1 RT
    filter(is_retweet == F,
           retweet_count > 1) %>%
    group_by(group3) %>%
    sample_n(size = case_when(
      group3 == "TRUE"   ~ 2,
      group3 == "FALSE"  ~ 2,
      is.na(group3) ~ 1)) %>%
    ungroup() %>%
    # Arrange column to show not-male first
    arrange(desc(group3)) %>%
    # Create and select only column with embed tweet code
    mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                         language, '" dir="ltr">',
                         text, '</p>&mdash;',
                         name, '(@',
                         screen_name, ') <a href="https://twitter.com/',
                         screen_name, '/status/',
                         status_id, '">',
                         created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
    )) %>%
    select(text) %>%
    mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
    rename(" " = text)

}

#########################################################################################

### MOST ACTIVE USERS

# Monitored accounts with the highest n. of posts in the last 12h

active_users <- function(dataset){

  dataset %>%
    group_by(screen_name) %>%
    count(sort = T) %>%
    ungroup() %>%
    slice(1:5) %>%
    # Create and select only column with twitter hyperlynk
    mutate(screen_name = paste0("@<a href='https://twitter.com/", screen_name, "' target='_blank' style='color: #d91c5c'>", screen_name, "</a>")) %>%
    select(screen_name) %>%
    rename("<i class='fas fa-users'></i>" = screen_name)

}

#########################################################################################

### MOST USED HASHTAGS

# Most used hashtags on posts monitored by Science Pulse over the last 12h

most_hashtags <- function(dataset){

  dataset %>%
    mutate(hashtag = toupper(hashtag)) %>%
    filter(hashtag != "NA") %>%
    count(hashtag, sort = T) %>%
    slice(1:5) %>%
    # Create and select only column with twitter hyperlynk
    select(hashtag) %>%
    mutate(hashtag = paste0("#<a href='https://twitter.com/hashtag/", hashtag, "' target='_blank' style='color: #d91c5c'>", hashtag, "</a>")) %>%
    rename("<i class='fas fa-hashtag'></i>" = hashtag)

}

#########################################################################################

### ALSO POPULAR ON PULSE

# Among tweets in the sample (posted in the sample), which ones were more RT
# (by the whole universe of twitter users)? Count includes RTs by users outside
# Pulse. However, original tweets were posted only by members.

also_popular <- function(dataset){

  # Filter only non-retweet posts
  own_sample_trends <- dataset %>%
    filter(is_retweet == F)

  # Uses a 4 group k-means clustering to identify the 2nd group with the most RTs
  set.seed(12345) # set.seed to keep the same results
  # Extract cluster names so that they are in order
  centers <- sort(kmeans(as.numeric(own_sample_trends$retweet_count),
                         centers = 4, nstart = 1000)$centers)
  # K-means cluster observations with the ordered names
  own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count),
                                      centers = centers)$cluster

  # Selects only the 2nd group with the most RTs
  own_sample_trends <- own_sample_trends %>%
    filter(cluster == 2)

  # Not enough tweets message, if necessary
  if(nrow(own_sample_trends) == 0){
    not_enough_tweets_en()

  } else {

    own_sample_trends %>%
      # Slice only the tweet with the most RTs from each user
      group_by(screen_name) %>%
      arrange(desc(retweet_count), created_at) %>%
      slice(1) %>%
      ungroup() %>%
      # Slice those 5 with the most RTs
      arrange(desc(retweet_count), created_at) %>%
      slice(1:5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### PULSE RADAR

# A random sample of 5 tweets among popular ones on Pulse. This group of popular
# tweets do not reach the highest-ranked in absolute number of RTs.

pulse_radar <- function(dataset){

  # Filter only non-retweet posts
  own_sample_trends <- dataset %>%
    filter(is_retweet == F)

  # Uses a 4 group k-means clustering to identify the 2nd group with the most RTs
  set.seed(12345) # set.seed to keep the same results
  # Extract cluster names so that they are in order
  centers <- sort(kmeans(as.numeric(own_sample_trends$retweet_count),
                         centers = 4, nstart = 1000)$centers)
  # K-means cluster observations with the ordered names
  own_sample_trends$cluster <- kmeans(as.numeric(own_sample_trends$retweet_count),
                                      centers = centers)$cluster

  # Selects only the 2nd group with the most RTs
  cluster2 <- own_sample_trends %>%
    filter(cluster == 2)

  # Not enough tweets message, if necessary
  if(nrow(cluster2) < 5){
    not_enough_tweets_pt()

  } else {

    cluster2 %>%
      # Random sample 5 tweets
      sample_n(5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           name, '(@',
                           screen_name, ') <a href="https://twitter.com/',
                           screen_name, '/status/',
                           status_id, '">',
                           created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')) %>%
      select(text) %>%
      mutate(text = paste0("<strong>//</strong> ", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### POPULAR AMONG SCIENTISTS

# Among posts retweet by Pulse's list members, which ones had the highest n. of RTs,
# considering only RT by accounts monitored by Science Pulse.
# They include tweets from any Twitter account.

popular_among_scientists <- function(dataset){

  show_dataset <- dataset %>%
    # Filter retweets that were RTed more than once
    filter(is_retweet == T) %>%
    group_by(retweet_status_id) %>%
    mutate(numero = n()) %>%
    ungroup() %>%
    filter(numero > 1)

  # Not enough tweets message, if necessary
  if(nrow(show_dataset) == 0) {

    not_enough_tweets_pt()

  } else {

    show_dataset %>%
      select(language, text, retweet_name, retweet_screen_name,
             retweet_status_id, retweet_created_at, numero) %>%
      distinct() %>%
      # Slices only the most RTed RT of each user
      group_by(retweet_screen_name) %>%
      arrange(desc(numero), retweet_status_id) %>%
      slice(1) %>%
      ungroup() %>%
      # Slices the 5 tweets most RTed
      arrange(desc(numero), retweet_status_id) %>%
      slice(1:5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           retweet_name, '(@',
                           retweet_screen_name, ') <a href="https://twitter.com/',
                           retweet_screen_name, '/status/',
                           retweet_status_id, '">',
                           retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong> ", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### OTHER POPULAR TWEETS

# Among RTs that appear in the sample, which ones have the highest overall n. of
# RTs? This count includes RTs from all Twitter users, but they need to have
# been RTed at least once by a Pulse's list member.

other_popular_tweets <- function(dataset){

  show_dataset <- dataset %>%
    # Filters 1+ RTs
    filter(is_retweet == T,
           retweet_count > 1)

  # Not enough tweets message, if necessary
  if(nrow(show_dataset) == 0) {

    not_enough_tweets_pt()

  } else {

    show_dataset %>%
      select(language, text, retweet_name, retweet_screen_name,
             retweet_status_id, retweet_created_at, retweet_count) %>%
      distinct() %>%
      # Slices only the highest RTed RT from each user
      group_by(retweet_screen_name) %>%
      arrange(desc(retweet_count), retweet_status_id) %>%
      slice(1) %>%
      ungroup() %>%
      # Slices the 5 most RTed RTs
      arrange(desc(retweet_count), retweet_status_id) %>%
      slice(1:5) %>%
      # Create and select only column with embed tweet code
      mutate(text = paste0('<blockquote class="twitter-tweet"><p lang="',
                           language, '" dir="ltr">',
                           text, '</p>&mdash;',
                           retweet_name, '(@',
                           retweet_screen_name, ') <a href="https://twitter.com/',
                           retweet_screen_name, '/status/',
                           retweet_status_id, '">',
                           retweet_created_at, '</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
      )) %>%
      select(text) %>%
      mutate(text = paste0("<strong>", 1:n(), "º //</strong>", text)) %>%
      rename(" " = text)

  }

}

#########################################################################################

### NOT ENOUGH TWEETS

# Message to explain that there are not enough tweets to create the column

not_enough_tweets_en <- function(){
  data.frame(variable = "Sorry, right now there are not enought tweets for this metric.\nPlease check again soon!") %>%
    rename(" " = variable)
}

#########################################################################################

### SPINNERS

# Spinners to appear while data is loading. Different types:

# Large column: round spinner
include_spinner_large_column <- function(output){

  withSpinner(tableOutput(output),
              type = getOption("spinner.type", default = 6),
              color = getOption("spinner.color", default = "#d91c5c"),
              size = getOption("spinner.size", default = 1),
              color.background = getOption("spinner.color.background", default = "#d91c5c"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(output))) NULL else "300px")

}

# Thin column: rectangular spinner
include_spinner_thin_column <- function(output){

  withSpinner(tableOutput(output),
              type  = getOption("spinner.type",  default = 1),
              color = getOption("spinner.color", default = "#d91c5c"),
              size  = getOption("spinner.size",  default = 1),
              color.background = getOption("spinner.color.background", default = "#d91c5c"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", tableOutput(output))) NULL else "300px")

}

# Texts: small and circular spinner
include_spinner_small <- function(output){

  withSpinner(textOutput(output),
              type = getOption("spinner.type", default = 7),
              color = getOption("spinner.color", default = "#d91c5c"),
              size = getOption("spinner.size", default = 0.4),
              color.background = getOption("spinner.color.background", default = "#d91c5c"),
              custom.css = FALSE, proxy.height = "20px")

}

# Tables: large and round spinner
include_spinner_tables <- function(output){

  withSpinner(DT::dataTableOutput(output),
              type = getOption("spinner.type", default = 6),
              color = getOption("spinner.color", default = "#d91c5c"),
              size = getOption("spinner.size", default = 1),
              color.background = getOption("spinner.color.background", default = "#d91c5c"),
              custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", DT::dataTableOutput(output))) NULL else "300px")

}

#########################################################################################

## GGPLOT THEMES

sp_theme <- function(base_size = 14, base_family = "Barlow") {

  (theme_foundation(base_size = base_size, base_family = base_family) +

     theme(
       plot.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
       panel.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
       text = element_text(colour = "#231f20"),

       axis.text = element_text(size = rel(0.8), margin=margin(0,40,0,0)),
       axis.ticks = element_blank(),
       axis.line = element_blank(),
       axis.title = element_text(size = rel(0.9), colour = "#999999"),

       legend.text = element_text(size=rel(0.9), angle = 0),
       legend.title = element_blank(),
       legend.key = element_rect(fill = "#eeeeee", colour = "#eeeeee", size = 0.5, linetype='dashed'),
       legend.key.width = unit(0.6, "cm"),
       legend.position = NULL,
       legend.justification = c(-0.05, 0),
       legend.background = element_blank(),
       legend.direction = "horizontal",
       legend.margin = (margin=margin(0,0,0,0)),
       legend.box = NULL,

       panel.border = element_rect(colour = "#eeeeee", fill=NA, size=2),
       panel.grid.major = element_line(colour = "#e4e4e4"),
       panel.grid.minor = element_line(colour = "#e6e6e6"),
       panel.grid.minor.x = element_line(colour = "#e4e4e4"),

       plot.title = element_text(hjust = 0, size = rel(1.3), face = "bold", colour = "#231f20"),
       plot.title.position = "plot",
       strip.background = element_rect(colour="#eeeeee", fill="#eeeeee"),
       plot.subtitle = element_text(hjust = 0, margin=margin(0,0,40,0),size = rel(1), lineheight = 1),
       plot.caption = element_text(size = rel(0.75), hjust = 1, margin=margin(20,0,0,0), colour = "#555555", lineheight = 1),
       plot.margin = unit(c(1, 2, 1, 1), "lines")
     )
  )
}


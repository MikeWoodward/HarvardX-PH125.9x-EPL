# Harvard PH128.9x Capstone project: English Premiership prediction
# =================================================================

# Mike Woodward - January/February 2021

# This code predicts the results of English Premier League (EPL) soccer
# matches. The project is complex and involves web scraping, data clean up, 
# data analysis, and multiple machine learning models. Because of course
# requirements, I've presented the R code as single file. 

# WARNING!
# Because of the web scraping and intensive computational burden of the
# machine learning models, this file will take 30+ hours to run to completion.

# The Github repository for this project is: 
# https://github.com/MikeWoodward/HarvardX-PH125.9x-EPL

# I've split the code into sections for easier review. Each section
# is a function and the functions may themselves contain functions
# 1. Housekeeping. Some tidying up of the R environment and loading of the 
#    required libraries. This must always be run.
# 2. Download. This is where the fields are downloaded from the internet and
#    some initial cleaning is done. Note, I provide all the downloaded
#    files in my Github project.
# 3. Cleaning. I re-load the data, merge the files, and clean the data.
# 4. Data analysis. This is where I explore the data and plot my findings.
# 5. Model. This is where I do the machine learning. Note this section may take
#    24 hours to run.
# 6. Execution. This section is at the very end of the file and runs the 
#    complete script end-to-end. This is a good place to start following the
#    code.

# Notes: 
# The team abbreviations list I created by hand - to run this project you
# *must* download the list manually from the Github project.
# All the steps in the process (download, clean etc.) are timed.
# All files are loaded using relative paths. 
# All data files and rda files are available from the Github project.

################################################################################
# Housekeeping                                                                 #
################################################################################
housekeeping <- function() {
  # Loading packages, clears out workspace.
  
  print("=================")  
  print("= Housekeeping  =")
  print("=================")
  
  # Clears out the r workspace each time this file is run. 
  rm(list=ls())
  # Clears graphics settings
  while (!is.null(dev.list())) dev.off()
  
  # Install packages if necessary
  if(!require(tidyverse)) install.packages(
    "tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages(
    "caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages(
    "data.table", repos = "http://cran.us.r-project.org")
  if(!require(lubridate)) install.packages(
    "lubridate", repos = "http://cran.us.r-project.org")
  if(!require(rvest)) install.packages(
    "rvest", repos = "http://cran.us.r-project.org")
  if(!require(doParallel)) install.packages(
    "doParallel", repos = "http://cran.us.r-project.org")
  if(!require(elasticnet)) install.packages(
    "elasticnet", repos = "http://cran.us.r-project.org")
  if(!require(xgboost)) install.packages(
    "xgboost", repos = "http://cran.us.r-project.org")
  
  library(tidyverse)
  library(caret)
  library(data.table)
  library(lubridate)
  library(rvest)
  library(doParallel)
  library(xgboost)
}

################################################################################
# Download                                                                     #
################################################################################
download <- function() {
  # This code performs multiple downloads from different sites. I organized it
  # as functions to make it easier to just download one set of data at a time.
  # IMPORTANT NOTE: Do not call this code too often. Repeated data downloads
  # may result in a ban from the website owners.

  # Function definitions follow, then code that calls the functions.
  
  # ----------------------------------------------------------------------------
  # Match results by season
  # ----------------------------------------------------------------------------
  match_results <- function() {
    # Downloads match results by season from football-data
    
    # Remove existing files if they exist
    f <- list.files(results_folder, 
                    include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
    file.remove(f)
    rm(f)
    
    # Go through every available season and download the data.  
    for (year_ in seq(1993, 2020)) {
      season <- paste(substr(toString(year_), 3, 4), 
                      substr(toString(year_ +1), 3, 4), 
                      sep="")
      print(sprintf("Downloading match data for season %s", season))
      url <- paste("https://www.football-data.co.uk/mmz4281/", 
                   season, 
                   "/E0.csv",
                   sep="")
      filename <- file.path(
        results_folder,
        paste("results", "-",
              "premierleague", "-",
              sprintf("%s-%s", year_, year_+1), '.csv', sep=''))
      download.file(url, filename)
    }
  }
  
  # ----------------------------------------------------------------------------
  # Team values
  # ----------------------------------------------------------------------------
  team_values <- function() {
    # Scrapes transfer values from TransferMarkt website.
    
    # Remove existing files if they exist
    f <- list.files(values_folder, 
                    include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
    file.remove(f)
    rm(f)
    
    # Build up list of dates to request from site. The site has values updates 
    # on the 1st and 15th of the month, but there are exceptions.
    days <- c("01", "15")
    months <- sprintf("%02d", seq(1, 12))
    years <- sprintf("%4d", seq(2010, 2021))
    temp <- expand.grid(list(years, months, days))
    dates <- paste(temp[,1], temp[,2], temp[,3], sep="-")
    # These are dates that don't fit the pattern
    odd_dates <- c("2014-07-10", "2014-10-23", "2017-06-19")
    dates <- sort(c(dates, odd_dates))
    # Select dates after "2010-10-31" - no data before then
    dates <- dates[dates > "2010-10-31"] 
    
    # Function to scrape the data.
    download_value_league <- function(base_url, league, date) {
      print(sprintf("Downloading team values for league %s date %s", 
                    league, date))
      url <- sprintf(base_url, date)
      # Retrieve the data
      page <- read_html(url)
      # Remove table header and footer nodes
      head_nodes <- page %>% html_nodes("thead")
      foot_nodes <- page %>% html_nodes("tfoot")
      xml_remove(head_nodes)
      xml_remove(foot_nodes)
      # Get the table
      tables <- page %>% html_nodes("table")
      value_table <- tables[[4]]
      df <- value_table %>% html_table()
      # Select only the columns of interest
      df <- df %>% select(X3, X5, X6)
      # Rename them and add the date
      names(df) <- c("Team", "Value", "Squad size")
      
      # If the squad size column is all NAs, there is no data, so
      # nothing to save. If the data isn't NAs, save it.
      if (df['Squad size'] %>% nrow() != sum(is.na(df['Squad size']))) {
        df['Date'] <- date
        filename <- file.path(
          values_folder,
          paste('values', '-',
                str_remove_all(league, "-"), '-',
                date, '.csv', sep=''))
        write.csv(df, filename, row.names=FALSE, fileEncoding = "UTF-8")
      }     
    }
    
    # Due to a quirk in the TransferMarket website, team values are sometimes
    # listed in a lower league. For example, if a team is promoted, their
    # value might still be in a lower league page. We therefore have to query
    # the transfer value of all the major English leagues.
    premier_url <- "https://www.transfermarkt.co.uk/premier-league/marktwerteverein/wettbewerb/GB1/stichtag/%s/plus/1"
    championship_url <- "https://www.transfermarkt.co.uk/championship/marktwerteverein/wettbewerb/GB2/stichtag/%s/plus/1"
    leagueone_url <- "https://www.transfermarkt.co.uk/leagueone/marktwerteverein/wettbewerb/GB3/stichtag/%s/plus/1"
    leaguetwo_url <- "https://www.transfermarkt.co.uk/leaguetwo/marktwerteverein/wettbewerb/GB4/stichtag/%s/plus/1"
    
    # Get the team values for each of the major leagues.
    sapply(dates, function(date) {
      download_value_league(premier_url, 'premierleague', date)    
      download_value_league(championship_url, 'championship', date)   
      download_value_league(leagueone_url, 'leagueone', date)
      download_value_league(leaguetwo_url, 'leaguetwo', date)    
    })
  }

  # ----------------------------------------------------------------------------
  # Foreign players and age
  # ----------------------------------------------------------------------------
  foreign_age <- function() {
    # Scrapes foreign player numbers and age from TransferMarkt website
    # Note this requires some manipulation of the HTML tables
    
    # Remove existing files if they exist
    f <- list.files(foreign_folder, 
                    include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
    file.remove(f)
    rm(f)  
    
    # Seasons to request - no data available prior to 2000
    years <- seq(2000, 2020)
    # Leagues to request
    leagues <- c('premier-league')
    # Base url to build actual URL from
    base_url <- 'https://www.transfermarkt.com/%s/startseite/wettbewerb/GB1/plus/?saison_id=%s'
    
    for (year_ in years) {
      for (league_ in leagues) {
        print(sprintf("Downloading foreign and mean age for league %s year %s", 
                      league_, year_))        
        url <- sprintf(base_url, league_, year_)
        
        # Retrieve the data
        page <- read_html(url)
        
        # Remove table header and footer nodes - this is a 'hack' from
        # StackOverflow to get round the limitations of rvest
        head_nodes <- page %>% html_nodes("thead")
        foot_nodes <- page %>% html_nodes("tfoot")
        xml_remove(head_nodes)
        xml_remove(foot_nodes)
        # Get the table
        tables <- page %>% html_nodes("table")
        foreign_table <- tables[[4]]
        df <- foreign_table %>% html_table()
        # Select and rename fields
        df <- df %>% select(X3, X5, X6)
        names(df) <- c("Team", "MeanAge", "ForeignPlayers")
        # Add season field
        df['Season'] <- sprintf("%s-%s", year_, year_+1)
        # Save data
        filename <- file.path(
          foreign_folder,
          paste("foreignage", "-", 
                str_remove_all(league_, "-"), "-", 
                year_, "-", year_+1, 
                '.csv', sep=''))
        write.csv(df, filename, row.names=FALSE)      
      }
    }  
    
  }

  # ----------------------------------------------------------------------------
  # Abbreviations file
  # ----------------------------------------------------------------------------  
  abbreviations <- function() {
    # Download the abbreviations file from my Github page for this project.
    url <- paste("https://raw.githubusercontent.com/",
                 "MikeWoodward/HarvardX-PH125.9x-EPL/",
                 "main/data_download/miscellaneous/",
                 "TeamAbbreviations.csv", sep="")
    filename <- file.path(miscellaneous_folder, "TeamAbbreviations.csv")
    download.file(url, filename)
  }
  
  # ----------------------------------------------------------------------------
  # Code that calls the functions to download data
  # ----------------------------------------------------------------------------

  print("====================")  
  print("= Downloading data =")
  print("====================")
  
  # Start the clock to time the download execution
  ptm <- proc.time()
  
  # Create the download folder structure
  data_folder = 'data_download'
  if (!dir.exists(data_folder)) {dir.create(data_folder)}
  results_folder = file.path(data_folder, 'match_results')
  if (!dir.exists(results_folder)) {dir.create(results_folder)}
  values_folder = file.path(data_folder, 'team_values')
  if (!dir.exists(values_folder)) {dir.create(values_folder)}
  foreign_folder = file.path(data_folder, 'foreign_age')
  if (!dir.exists(foreign_folder)) {dir.create(foreign_folder)}
  miscellaneous_folder = file.path(data_folder, 'miscellaneous')
  if (!dir.exists(miscellaneous_folder)) {dir.create(miscellaneous_folder)}
    
  # Call functions to download the data files
  match_results()
  team_values()
  foreign_age()
  abbreviations()
  
  # Download duration
  script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
  script_duration <- sprintf('%d minutes %.1f seconds', 
                             script_duration%/%60, 
                             script_duration%%60)
  print(paste("Download duration was", script_duration))  
   
}

################################################################################
# clean                                                                        #
################################################################################
clean <- function() {
  # Cleans the downloaded data and prepares an rda file for use by other 
  # functions. The rda file is match_results.rda and is used by the dataanalysis
  # and model functions.

  # Start the clock to time the clean execution
  ptm <- proc.time()  
  
  print("=================")  
  print("= Cleaning data =")
  print("=================")
  
  # Check download folders exist
  data_folder = 'data_download'
  if (!dir.exists(data_folder)) {
    stop(message(sprintf("No %s folder - run download first", data_folder)))
  }
  
  # Data loading
  # ============
  
  # Load team abbreviations
  # ------------------------
  # Team abbreviations list created by hand
  miscellaneous_folder = file.path(data_folder, 'miscellaneous')
  if (!dir.exists(miscellaneous_folder)) {
    stop(message(sprintf("No %s folder - check setup", miscellaneous_folder)))
  }
  team_abbreviations <- read.csv(file.path(miscellaneous_folder, 
                                           'TeamAbbreviations.csv'))
  
  # Load match results
  # ------------------
  # Get files from the downloads folders
  results_folder = file.path(data_folder, 'match_results')
  if (!dir.exists(results_folder)) {
    stop(message(sprintf("No %s folder - run download first", results_folder)))
  }
  results_files <- list.files(results_folder, 
                              include.dirs = FALSE, 
                              full.names = TRUE, 
                              recursive = TRUE)
  # Check match results files exist, if not, stop execution. 
  if (length(results_files) == 0) {
    stop(message("No results files - run downloads first"))  
  }
  
  # List of fields we want to keep for analysis - other fields are related to
  # betting.
  fields <- c('Date', 'HomeTeam', 'AwayTeam', 
              'FTHG', 'FTAG', 'FTR', 
              'HR', 'AR', 'HY', 'AY')
  
  # Function to read in match results for a season
  read_match <- function(result) {
    # Read in season data
    season <- read.csv(result)
    # Some seasons do not have all of the fields, which causes problems for
    # merging and analysis.
    # Add missing fields and fill with NA. This makes the data frame the same
    # for all seasons.
    missing_fields <- fields[!(fields %in% names(season))]
    for (missing_field in missing_fields) {
      season[,`missing_field`] <- NA
    }
    # Select just the fields we want
    season <- season %>% select(fields)
    
    # Add the season
    # Get the file name, which contains the season
    basename_result <- basename(result)
    # Strip the season from the file name
    basename_result_length <- nchar(basename_result)
    # Add the season field
    season['Season'] <- sprintf("%s", 
                                substr(basename_result, 
                                       basename_result_length - 12, 
                                       basename_result_length - 4))
    # Remove empty rows - some files have extraneous rows
    season <- season[!is.na(season$FTHG),]
    # Properly format the date - if the length is 8 characters, then the date
    # is a two digit year
    if (mean(nchar(season$Date)) ==8) {
      season$Date = as.Date(season$Date, tryFormats = c("%d/%m/%y"))
    } else {
      season$Date = as.Date(season$Date, tryFormats = c("%d/%m/%Y"))    
    }
    # Return the data
    season
  }
  
  # Load all match results - call read_match for every file
  match_results <- do.call(rbind, lapply(results_files, read_match))
  
  # Add the team abbreviation to the results
  match_results <- match_results %>% 
    left_join(team_abbreviations, by = c("HomeTeam" = "TeamName")) %>%
    rename(c("HomeTeamAbbreviation"="TeamAbbreviation")) %>%
    left_join(team_abbreviations, by = c("AwayTeam" = "TeamName")) %>%
    rename(c("AwayTeamAbbreviation"="TeamAbbreviation"))
  
  # Load team values
  # ----------------
  values_folder = file.path(data_folder, 'team_values')
  # If the folder doesn't exist, stop execution
  if (!dir.exists(values_folder)) {
    stop(message(sprintf("No %s folder - run download first", team_values)))
  }
  # Get all the value files
  values_files <- list.files(values_folder, 
                             include.dirs = FALSE, 
                             full.names = TRUE, 
                             recursive = TRUE)

  # If there are no value files, stop execution  
  if (length(values_files) == 0) {
    stop(message("No results files - run downloads first"))  
  }
  
  # Read in all the value csv files
  values <- do.call(rbind,lapply(values_files, function(file_) {
    read.csv(file=file_, encoding="UTF-8")}))
  values <- values %>% select(Date, Team, Value, Squad.size)
  # Calculate the team values in GBP millions
  # This code produces two spurious warnings about NAs - ignore them
  values$Value <- ifelse(
    grepl('bn', values$Value), 
    1000*as.numeric(str_remove_all(values$Value, '[£bn]')), 
    as.numeric(str_remove_all(values$Value, '[£m]')))
  
  # Remove FC suffix on team names for better matching
  values$Team <- str_remove_all(values$Team, " FC")
  # Convert the date field to date format
  values['Date'] <- as.Date(values$Date)
  
  # We need to match values to match dates. First step is finding the season and
  # match date to start with, then we resample to get team values on every
  # single day after that. This is over-sampling, but it's more efficient
  # to oversample than try and match to match dates in one step.
  
  # Find the start date for matching value to match_results
  start_value_date <- min(values$Date)
  season_dates <- match_results %>% 
    group_by(Season) %>% 
    summarize(start_date = min(Date), end_date=max(Date)) %>%
    mutate(start_bool = ifelse(end_date > start_value_date, TRUE, FALSE)) %>%
    filter(start_bool == TRUE)
  start_date <- min(season_dates$start_date)
  
  # Resample to cover full range of dates in seasons
  values <- values %>% 
    complete(Date = seq.Date(start_date, 
                             max(match_results$Date), 
                             by="day"),
             Team) %>%
    arrange(Team, Date) %>%
    group_by(Team) %>% 
    fill(Value, Squad.size, .direction="downup") %>%
    ungroup()
  
  # Add in team abbreviations
  values <- values %>% left_join(team_abbreviations, 
                                 by = c("Team" = "TeamName"))
  
  # Load foreign and age
  # --------------------
  foreign_folder = file.path(data_folder, 'foreign_age')
  # If the folder doesn't exist, halt execution
  if (!dir.exists(foreign_folder)) {
    stop(message(sprintf("No %s folder - run download first", foreign_folder)))
  }
  foreign_files <- list.files(foreign_folder, 
                              include.dirs = FALSE, 
                              full.names = TRUE, 
                              recursive = TRUE)
  # If there are no values files, halt execution. 
  if (length(foreign_files) == 0) {
    stop(message("No foreign files - run downloads first"))  
  }
  
  # Read in all the csv data
  foreign <- do.call(rbind, lapply(foreign_files, read.csv))
  
  # Add in the team names
  foreign <- foreign %>% left_join(team_abbreviations, 
                                   by = c("Team" = "TeamName"))
  
  # Merge to create team and values data frame
  # ==========================================
  match_results <- match_results %>%
    # Merging in values data
    left_join(values %>% select(Date, Value, Squad.size, TeamAbbreviation), 
              by = c("HomeTeamAbbreviation" = "TeamAbbreviation",
                     "Date")) %>%
    rename(c("HomeTeamValue"="Value", "HomeTeamSquadSize"="Squad.size")) %>%
    left_join(values %>% select(Date, Value, Squad.size, TeamAbbreviation), 
              by = c("AwayTeamAbbreviation" = "TeamAbbreviation",
                     "Date")) %>%
    rename(c("AwayTeamValue"="Value", "AwayTeamSquadSize"="Squad.size")) %>%
    # Merging foreign players
    left_join(foreign %>% select(TeamAbbreviation, Season, 
                                 ForeignPlayers, MeanAge),
              by = c("HomeTeamAbbreviation" = "TeamAbbreviation",
                     "Season")) %>%
    rename(c("HomeTeamMeanAge"="MeanAge", 
             "HomeTeamForeignPlayers"="ForeignPlayers")) %>%
    left_join(foreign %>% select(TeamAbbreviation, Season, 
                                 ForeignPlayers, MeanAge),
              by = c("AwayTeamAbbreviation" = "TeamAbbreviation",
                     "Season")) %>%
    # Merging team age
    rename(c("AwayTeamMeanAge"="MeanAge", 
             "AwayTeamForeignPlayers"="ForeignPlayers"))
  
  # We now have a single data frame that contains all the data we
  # need for analysis and modeling.
  
  # Tidying up
  # ==========
  # Script duration
  script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
  script_duration <- sprintf('%d minutes %.1f seconds', 
                             script_duration%/%60, 
                             script_duration%%60)
  print(paste("Clean duration was", script_duration))
  
  # Save match_results 
  match_folder = 'data_match'
  if (!dir.exists(match_folder)) {dir.create(match_folder)}
  save(match_results, 
       file=file.path(match_folder, "match_results.rda"))  
  
}

################################################################################
# DataAnalysis                                                                 #
################################################################################
dataanalysis <- function() {
  # Exploratory data analysis. Plots various charts.
  
  # Start the clock to time the script execution
  ptm <- proc.time()
  
  print("==================")  
  print("= Data analysis  =")
  print("==================")
  
  # Check data_match folder exists
  match_folder = 'data_match'
  if (!dir.exists(match_folder)) {
    stop(message(
      sprintf("No %s folder - run download first, then clean",
              match_folder)
    ))
  }
  
  # Load the match result data
  load(file.path(match_folder, 'match_results.rda'))
  
  # Table key - only non-obvious fields explained
  # FTHG - Full Time Home Goals
  # FTAG - Full Time Away Goals
  # FTR - Full time result (H=Home win, A=Away win, D=Draw)
  # WeekNumber - week number of season
  # HomeMPP - Home Mean prior Points - the mean number of points per game for  
  # the home team prior to the current game
  # HomeMPCR - Home Mean Prior Red Cards - the mean number of red cards per game
  # for the home team prior to the current match
  # HomeMPCY - Home Mean Prior Red Cards - the mean number of yellow cards per
  # game for the home team prior to the current match
  # HomeMPFG - Home Mean Prior For Goals - the mean number of goals per match
  # the home team scored prior to the current game
  # HomeMPAG - Home Mean Prior For Goals - the mean number of goals per match
  # the home team conceded prior to the current game
  
  # Score distributions
  # ===================
  # Work out what scores are more frequent than others. Create a heatmap for the
  # results.
  goal_distribution <- match_results %>%
    group_by(FTHG, FTAG) %>%
    summarize(Matches = n())
  
  plt_goal_heatmap <- goal_distribution %>%
    ggplot(aes(FTHG, FTAG, fill = Matches)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdPu") +
    ggtitle("Score distribution")
  print(plt_goal_heatmap)
  
  # Home team advantage
  # ===================
  # Group matches by season and work out the number of home and away wins.
  # The home win proportion is count of home wins/count of all win.
  # m_HGD - mean home goal difference
  # se_HGD - standard error of home goal difference
  # proportion_home - proportion of wins that are home wins
  # se_proportion_home - standard error of proportion_home
  home_advantage <- match_results %>%
    group_by(Season) %>%
    summarize(
      home_wins = sum(FTR == 'H'),
      away_wins = sum(FTR == 'A'),
      draws = sum(FTR == 'D'),
      matches = n(),
      proportion_home = home_wins / (home_wins + away_wins),
      se_proportion_home = sqrt(proportion_home * (1 - proportion_home) /
                                  (home_wins + away_wins)),
      m_HGD = mean(FTHG - FTAG),
      se_HGD = sd(FTHG - FTAG) / matches,
      .groups = 'keep'
    ) %>%
    # Every season prior to 2020-2021 show as blue, 2020-20201 as red
    mutate(color = ifelse(Season == "2020-2021", "red", "blue"))
  
  # This number is quoted in the final report for the 2020-2021 home
  # win proportion
  home_wins_proportion_2020_2021 <- home_advantage %>%
    filter(Season == '2020-2021') %>% .$proportion_home
  
  # Plot the fraction of home wins by season. 1.96 is the z-score for a 95%
  # confidence interval
  plt_home_wins <- home_advantage %>%
    ggplot(aes(x = Season,
               y = proportion_home,
               ymin = proportion_home - 1.96 * se_proportion_home,
               ymax = proportion_home + 1.96 * se_proportion_home)) +
    geom_point(aes(color = color)) +
    geom_errorbar(aes(color = color)) +
    scale_colour_identity() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          aspect.ratio = 0.4,
          legend.position = "none") +
    ggtitle("Fraction of home wins / all wins vs. season") +
    ylab("Fraction home wins / wins") +
    xlab("Season")
  print(plt_home_wins)
  
  # Plot the home team goal difference. If there were no home team advantage,
  # we would expect this goal difference to be zero. 1.96 is z-score for 95%
  # confidence interval
  plt_home_goal_difference <- home_advantage %>%
    ggplot(aes(x = Season,
               y = m_HGD,
               ymin = m_HGD - 1.96 * se_HGD,
               ymax = m_HGD + 1.96 * se_HGD)) +
    geom_point(aes(color = color)) +
    geom_errorbar(aes(color = color)) +
    scale_colour_identity() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          aspect.ratio = 0.4,
          legend.position = "none") +
    ggtitle("Home team goal difference by season") +
    ylab("Home team goal difference") +
    xlab("Season")
  print(plt_home_goal_difference)
  
  # Team value advantage
  # ====================
  temp <- match_results %>%
    # Get rid of NA values
    filter(!is.na(HomeTeamValue) | !is.na(AwayTeamValue)) %>%
    # deltaV is the difference in value, deltaG is the goal difference. Always
    # oriented so most expensive team is first (so value difference always
    # positive)
    mutate(deltaV = ifelse(HomeTeamValue > AwayTeamValue,
                           HomeTeamValue - AwayTeamValue,
                           AwayTeamValue - HomeTeamValue),
           deltaG = ifelse(HomeTeamValue > AwayTeamValue,
                           FTHG - FTAG,
                           FTAG - FTHG))
  plt_team_value <- temp %>% ggplot(aes(x = deltaV, y = deltaG)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 6) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = "red",
                fill = 'red') +
    ggtitle("Goal difference vs. team value difference") +
    ylab("Goal difference") +
    xlab("Difference in team value (£ millions)")
  print(plt_team_value)
  
  # Foreign player advantage
  # ========================
  temp <- match_results %>%
    # Get rid of NA values
    filter(!is.na(HomeTeamForeignPlayers) |
             !is.na(AwayTeamForeignPlayers)) %>%
    # deltaF is the difference in foreign players, deltaG is the goal 
    # difference. Always oriented so team with the most foreign players is 
    # first (so foreign difference always positive)
    mutate(deltaF = ifelse(HomeTeamForeignPlayers > AwayTeamForeignPlayers,
                           HomeTeamForeignPlayers - AwayTeamForeignPlayers,
                           AwayTeamForeignPlayers - HomeTeamForeignPlayers),
           deltaG = ifelse(HomeTeamForeignPlayers > AwayTeamForeignPlayers,
                           FTHG - FTAG,
                           FTAG - FTHG))
  plt_foreign <- temp %>% ggplot(aes(x = deltaF, y = deltaG)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 6) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = "red",
                fill = 'red')  +
    ggtitle("Goal difference vs. foreign player difference") +
    ylab("Goal difference") +
    xlab("Difference in foreign players")
  print(plt_foreign)
  
  # Mean age advantage
  # ==================
  temp <- match_results %>%
    # Get rid of NA values
    filter(!is.na(HomeTeamMeanAge) | !is.na(AwayTeamMeanAge)) %>%
    # deltaA is the difference in age, deltaG is the goal 
    # difference. Always oriented so team with the oldest players is 
    # first (so age difference always positive)
    mutate(deltaA = ifelse(HomeTeamMeanAge > AwayTeamMeanAge,
                           HomeTeamMeanAge - AwayTeamMeanAge,
                           AwayTeamMeanAge - HomeTeamMeanAge),
           deltaG = ifelse(HomeTeamMeanAge > AwayTeamMeanAge,
                           FTHG - FTAG,
                           FTAG - FTHG))
  plt_age <- temp %>% ggplot(aes(x = deltaA, y = deltaG)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 6) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Goal difference vs. mean age difference") +
    ylab("Goal difference") +
    xlab("Difference in mean age")
  print(plt_age)
  
  # Squad size advantage
  # ====================
  temp <- match_results %>%
    # Get rid of NA values
    filter(!is.na(HomeTeamSquadSize) | !is.na(AwayTeamSquadSize)) %>%
    # deltaA is the difference in squad size, deltaG is the goal 
    # difference. Always oriented so team with the largest squad is 
    # first (so size difference always positive)
    mutate(deltaS = ifelse(HomeTeamSquadSize > AwayTeamSquadSize,
                           HomeTeamSquadSize - AwayTeamSquadSize,
                           AwayTeamSquadSize - HomeTeamSquadSize),
           deltaG = ifelse(HomeTeamSquadSize > AwayTeamSquadSize,
                           FTHG - FTAG,
                           FTAG - FTHG))
  plt_size <- temp %>% ggplot(aes(x = deltaS, y = deltaG)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 6) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Goal difference vs. squad size difference") +
    ylab("Goal difference") +
    xlab("Difference in squad size")
  print(plt_size)
  
  # Time of season effects
  # ======================
  # Calculate the start date for each season. Work out a date offset so we can
  # assign week numbers to the games.
  start_date <- match_results %>%
    select(Season, Date) %>%
    unique() %>%
    group_by(Season) %>%
    summarize(Season = Season[1],
              start_date = min(Date))
  
  # Draw proportion etc. by season week
  temp <- match_results %>% 
    left_join(start_date, by = 'Season') %>%
    mutate(season_week = as.integer((Date - start_date)/7) + 1) %>%
    group_by(Season, season_week) %>%
    summarize(draw_proportion = sum(FTR == 'D') / n(),
              magd = mean(abs(FTHG - FTAG)),
              gc = mean(FTHG + FTAG))
  
  # Note 2019-2020 season is different due to COVID - season ended much
  # later than usual.
  plt_season <-
    temp %>% ggplot(aes(x = season_week, y = draw_proportion)) +
    geom_point(color = 'blue',
               alpha = 0.5,
               size = 3) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Draw proportion vs. week in season") +
    ylab("Draw proportion") +
    xlab("Week in season")
  print(plt_season)
  
  # Mean absolute goal difference by week
  plt_season_magd <- temp %>% ggplot(aes(x = season_week, y = magd)) +
    geom_point(color = 'blue',
               alpha = 0.5,
               size = 3) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Mean absolute goal difference vs. week in season") +
    ylab("Mean absolute goal difference") +
    xlab("Week in season")
  print(plt_season_magd)
  
  # Goal count by week
  plt_season_gc <- temp %>% ggplot(aes(x = season_week, y = gc)) +
    geom_point(color = 'blue',
               alpha = 0.5,
               size = 3) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Mean goal count vs. week in season") +
    ylab("Goal count") +
    xlab("Week in season")
  print(plt_season_gc)
  
  # Discipline effects
  # ==================
  # Note we're calculating the mean prior quantity, which is the mean
  # just prior to the start of the match.
  # Home red and yellow cards
  home_discipline <- match_results %>%
    select(Season, Date, HomeTeamAbbreviation, HR, HY) %>%
    mutate(TeamAbbreviation = HomeTeamAbbreviation) %>%
    rename(c("R" = "HR", "Y" = "HY")) %>%
    select(Season, Date, TeamAbbreviation, R, Y)
  
  away_discipline <- match_results %>%
    select(Season, Date, AwayTeamAbbreviation, AR, AY) %>%
    mutate(TeamAbbreviation = AwayTeamAbbreviation) %>%
    rename(c("R" = "AR", "Y" = "AY")) %>%
    select(Season, Date, TeamAbbreviation, R, Y)
  
  # Mean number of cards prior to current match
  discipline <- rbind(home_discipline, away_discipline) %>%
    arrange(Season, Date) %>%
    group_by(Season, TeamAbbreviation) %>%
    mutate(
      r = rowid(TeamAbbreviation),
      MPCR = ifelse(r > 1, (cumsum(R) - R) / (r - 1), 0),
      MPCY = ifelse(r > 1, (cumsum(Y) - Y) / (r - 1), 0)
    ) %>%
    arrange(Season, TeamAbbreviation, Date) %>%
    select(Season, Date, TeamAbbreviation, MPCR, MPCY)
  tail(discipline)
  
  # Join data back to main table
  temp <- match_results %>%
    left_join(discipline,
              by = c("Season", "Date",
                     "HomeTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("HomeMPCR" = "MPCR", "HomeMPCY" = "MPCY"))  %>%
    left_join(discipline,
              by = c("Season", "Date",
                     "AwayTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("AwayMPCR" = "MPCR", "AwayMPCY" = "MPCY"))
  
  # deltaMPCR is the difference in mean red cards, deltaGR is the goal 
  # difference for red card differences. Always oriented so team with the  
  # largest number of mean red cards first (so red card difference always 
  # positive)
  temp <- temp %>%
    # Remove any nulls.
    filter(!is.na(HomeMPCR) | !is.na(AwayMPCR)) %>%
    filter(!is.na(HomeMPCY) | !is.na(AwayMPCY)) %>%
    mutate(deltaMPCR = ifelse(HomeMPCR > AwayMPCR,
                              HomeMPCR - AwayMPCR,
                              AwayMPCR - HomeMPCR),
           deltaGR = ifelse(HomeMPCR > AwayMPCR,
                            FTHG - FTAG,
                            FTAG - FTHG),
           deltaMPCY = ifelse(HomeMPCY > AwayMPCY,
                              HomeMPCY - AwayMPCY,
                              AwayMPCY - HomeMPCY),
           deltaGY = ifelse(HomeMPCY > AwayMPCY,
                            FTHG - FTAG,
                            FTAG - FTHG))
  # Plot the results
  plt_red <- temp %>% ggplot(aes(x = deltaMPCR, y = deltaGR)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 6) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Goal difference vs. mean prior red cards") +
    ylab("Goal difference") +
    xlab("Difference in mean prior red cards")
  print(plt_red)
  plt_yellow <- temp %>% ggplot(aes(x = deltaMPCY, y = deltaGY)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 6) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Goal difference vs. mean prior yellow cards") +
    ylab("Goal difference") +
    xlab("Difference in mean prior yellow cards")
  print(plt_yellow)
  
  # Previous points
  # ===============
  # Note we're calculating the mean prior points just before the match
  # 3 points for a win, 1 for a draw
  temp <- match_results %>%
    mutate(HomeTeamPoints = 3 * (FTR == 'H') + (FTR == 'D'),
           AwayTeamPoints = 3 * (FTR == 'A') + (FTR == 'D'))
  
  # Work out points prior to the match
  home_points <- temp %>%
    select(Season, Date, HomeTeamAbbreviation, HomeTeamPoints) %>%
    rename(c("TeamPoints" = "HomeTeamPoints",
             "TeamAbbreviation" = "HomeTeamAbbreviation"))
  
  away_points <- temp %>%
    select(Season, Date, AwayTeamAbbreviation, AwayTeamPoints) %>%
    rename(c("TeamPoints" = "AwayTeamPoints",
             "TeamAbbreviation" = "AwayTeamAbbreviation"))
  
  # Work out mean prior points
  points <- rbind(home_points, away_points) %>%
    arrange(Season, TeamAbbreviation, Date) %>%
    group_by(Season, TeamAbbreviation) %>%
    mutate(r = rowid(TeamAbbreviation),
           MPP =ifelse(rowid(TeamAbbreviation) > 1,
                       (cumsum(TeamPoints) - TeamPoints) / (r - 1),
                       0)) %>%
    select(Season, Date, TeamAbbreviation, MPP)
  
  # Join the results back to the main table
  temp <- match_results %>%
    left_join(points,
              by = c("Season", "Date",
                     "HomeTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("HomeMPP" = "MPP"))  %>%
    left_join(points,
              by = c("Season", "Date",
                     "AwayTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("AwayMPP" = "MPP"))
  
  # Work out a goal difference and a points difference
  temp <- temp %>%
    mutate(deltaPoints = ifelse(HomeMPP > AwayMPP,
                                HomeMPP - AwayMPP,
                                AwayMPP - HomeMPP),
           deltaGD = ifelse(HomeMPP > AwayMPP,
                            FTHG - FTAG,
                            FTAG - FTHG))
  # Plot the results
  plt_points <- temp %>% ggplot(aes(x = deltaPoints, y = deltaGD)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 5) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Goal difference vs. mean prior points difference") +
    ylab("Goal difference") +
    xlab("Difference in mean prior points")
  print(plt_points)
  
  # Mean prior goals
  # ================
  # Note we're calculating the mean prior goals just before the match
  
  # Work out number of goals in total
  home_goals <- match_results %>%
    select(Season, Date, HomeTeamAbbreviation, FTHG, FTAG) %>%
    rename(c("GoalsFor" = "FTHG",
             "GoalsAgainst" = "FTAG",
             "TeamAbbreviation" = "HomeTeamAbbreviation"))
  
  away_goals <- match_results %>%
    select(Season, Date, AwayTeamAbbreviation, FTHG, FTAG) %>%
    rename(c("GoalsFor" = "FTAG",
             "GoalsAgainst" = "FTHG",
             "TeamAbbreviation" = "AwayTeamAbbreviation"))
  
  # Work out the mean prior to the match
  goals <- rbind(home_goals, away_goals) %>%
    arrange(Season, TeamAbbreviation, Date) %>%
    group_by(Season, TeamAbbreviation) %>%
    mutate(r = rowid(TeamAbbreviation),
          MPFG = ifelse(rowid(TeamAbbreviation) > 1,
                        (cumsum(GoalsFor) - GoalsFor) / (r - 1),
                        0),
          MPAG = ifelse(rowid(TeamAbbreviation) > 1,
                        (cumsum(GoalsAgainst) - GoalsAgainst) / (r - 1),
                        0)) %>%
    select(Season, Date, TeamAbbreviation, MPFG, MPAG)
  
  # Add the results back to the main table
  temp <- match_results %>%
    left_join(goals,
              by = c("Season",
                     "Date",
                     "HomeTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("HomeMPFG" = "MPFG",
             "HomeMPAG" = "MPAG"))  %>%
    left_join(goals,
              by = c("Season",
                     "Date",
                     "AwayTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("AwayMPFG" = "MPFG",
             "AwayMPAG" = "MPAG"))
  
  # Work out goal differences
  temp <- temp %>%
    mutate(deltaMGD = ifelse((HomeMPFG - AwayMPAG) > (AwayMPFG - AwayMPAG),
                             (HomeMPFG - AwayMPAG) - (AwayMPFG - AwayMPAG),
                             -(HomeMPFG - AwayMPAG) + (AwayMPFG - AwayMPAG)),
           deltaGD = ifelse((HomeMPFG - AwayMPAG) > (AwayMPFG - AwayMPAG),
                            FTHG - FTAG,
                            FTAG - FTHG))
  # Plot the results
  plt_goals <- temp %>% ggplot(aes(x = deltaMGD, y = deltaGD)) +
    geom_point(color = 'blue',
               alpha = 0.05,
               size = 5) +
    geom_smooth(method = "lm",
                se = TRUE,
                level = 0.95,
                formula = y ~ x,
                color = 'red',
                fill = 'red')  +
    ggtitle("Goal difference vs. mean prior goal difference") +
    ylab("Goal difference") +
    xlab("Difference in mean prior goals")
  print(plt_goals)
  
  # Tidying up
  # ==========
  # Save results
  analysis_folder = 'analysis'
  if (!dir.exists(analysis_folder)) {
    dir.create(analysis_folder)
  }
  save(plt_goal_heatmap,
       home_wins_proportion_2020_2021,
       plt_home_wins,
       plt_home_goal_difference,
       plt_team_value,
       plt_foreign,
       plt_age,
       plt_size,
       plt_season,
       plt_season_magd,
       plt_red,
       plt_yellow,
       plt_points,
       plt_goals,
       file = file.path(analysis_folder, "analysis.rda"))  
  
  # Script duration
  script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
  script_duration <- sprintf('%d minutes %.1f seconds',
                             script_duration %/% 60,
                             script_duration %% 60)
  print(paste("Data analysis duration was", script_duration))
}

################################################################################
# Model                                                                        #
################################################################################
model <- function() {
  # Function to run multiple machine learning models:
  # 1. Baseline naive means
  # 2. GLM
  # 3. GLM Net
  # 4. Support Vector Machine
  # 5. k-nearest neighbors
  # 6. XGB Linear
  # 7. XGB Tree
  # 8. Random Forest

  # Start the clock to time the script execution
  ptm <- proc.time()  
  
  print("==========")  
  print("= Model  =")
  print("==========")

  # Check data match folder exists
  match_folder = 'data_match'
  if (!dir.exists(match_folder)) {
    stop(message(sprintf("No %s folder - run download and cleaning", 
                         match_folder)))
  }  
      
  # Make and start the cluster. This allows parallel processing and greatly 
  # speeds up modeling 
  cores <- detectCores() - 1
  cl <- makePSOCKcluster(cores)
  registerDoParallel(cl)
  
  # Load and prepare data
  # =====================
  load(file=file.path(match_folder, "match_results.rda"))
  
  # Select just the seasons we can analyze,
  # * before 2010-2011, we have missing data
  # * 2020-2021 is COVID
  match_results <- match_results %>% 
    filter(Season > '2009-2010'& Season != '2020-2021')
  
  # Season week number
  # ------------------
  # Add the season week number field. Seasons start mid year and go to the next 
  # year, so we're using an offset (start_date).
  start_date <- match_results %>%
    select(Season, Date) %>%
    unique() %>%
    group_by(Season) %>%
    summarize(Season = Season[1], start_date = min(Date)) 
  match_results <- match_results %>% 
    left_join(start_date, by='Season') %>%
    mutate(WeekNumber=as.integer((Date - start_date)/7) + 1)
  
  # Points
  # ------
  # We need to calculate mean prior points
  # Three points for a win, 1 for a draw, 0 for  loss
  temp <- match_results %>% 
    mutate(HomePoints=3*(FTR=='H') + (FTR=='D'),
           AwayPoints=3*(FTR=='A') + (FTR=='D'))
  
  home_points <- temp %>% 
    select(Season, Date, HomeTeamAbbreviation, HomePoints) %>%
    rename(c("TeamPoints"="HomePoints",
             "TeamAbbreviation"="HomeTeamAbbreviation"))
  
  away_points <- temp %>% 
    select(Season, Date, AwayTeamAbbreviation, AwayPoints) %>%
    rename(c("TeamPoints"="AwayPoints",
             "TeamAbbreviation"="AwayTeamAbbreviation"))
  
  # Work out the sum total of points per team, *prior* to the current match.
  # We want the average over the prior matches. That's why we have 
  # cumsum(TeamPoints)-TeamPoints)/(r-1) where r is the current number of
  # matches and r - 1 is the number of matches before the current match.
  points <- rbind(home_points, away_points) %>%
    arrange(Season, TeamAbbreviation, Date) %>%
    group_by(Season, TeamAbbreviation) %>%
    mutate(r=rowid(TeamAbbreviation),
           MPP=ifelse(rowid(TeamAbbreviation) > 1, 
                      (cumsum(TeamPoints)-TeamPoints)/(r-1),
                      0)) %>%
    select(Season, Date, TeamAbbreviation, MPP)
  head(points)
  
  # Merge the points data back in to match_results
  match_results <- match_results %>% 
    left_join(points,
              by=c("Season", "Date", 
                   "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
    rename(c("HomeMPP"="MPP"))  %>% 
    left_join(points,
              by=c("Season", "Date", 
                   "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
    rename(c("AwayMPP"="MPP"))
  
  # Red and yellow cards
  # --------------------
  # Work out the number of red/yellow cards per team
  home_discipline <- match_results %>% 
    select(Season, Date, HomeTeamAbbreviation, HR, HY) %>%
    mutate(TeamAbbreviation=HomeTeamAbbreviation) %>%
    rename(c("R"="HR", "Y"="HY")) %>%
    select(Season, Date, TeamAbbreviation, R, Y)
  
  away_discipline <- match_results %>% 
    select(Season, Date, AwayTeamAbbreviation, AR, AY) %>%
    mutate(TeamAbbreviation=AwayTeamAbbreviation) %>%
    rename(c("R"="AR", "Y"="AY")) %>%
    select(Season, Date, TeamAbbreviation, R, Y)
  
  # Work out the mean number of red/yellow cards per team per match prior to the
  # current match.
  discipline <- rbind(home_discipline, away_discipline) %>%
    arrange(Season, Date) %>%
    group_by(Season, TeamAbbreviation) %>%
    mutate(r=rowid(TeamAbbreviation),
           MPCR=ifelse(r>1, (cumsum(R) - R)/(r-1), 0),
           MPCY=ifelse(r>1, (cumsum(Y) - Y)/(r-1), 0)) %>%
    arrange(Season, TeamAbbreviation, Date) %>%
    select(Season, Date, TeamAbbreviation, MPCR, MPCY)
  # Show the top of the frame as a check
  head(discipline)
  
  # Add the red and yellow card data back to match_results
  match_results <- match_results %>% 
    left_join(discipline,
              by=c("Season", "Date", 
                   "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
    rename(c("HomeMPCR"="MPCR",
             "HomeMPCY"="MPCY"))  %>% 
    left_join(discipline,
              by=c("Season", "Date", 
                   "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
    rename(c("AwayMPCR"="MPCR", 
             "AwayMPCY"="MPCY"))
  
  # Mean prior goals
  # ----------------
  # Note we're calculating the mean prior goals just before the match
  home_goals <- match_results %>%
    select(Season, Date, HomeTeamAbbreviation, FTHG, FTAG) %>%
    rename(c("GoalsFor" = "FTHG",
             "GoalsAgainst" = "FTAG",
             "TeamAbbreviation" = "HomeTeamAbbreviation"))
  
  away_goals <- match_results %>%
    select(Season, Date, AwayTeamAbbreviation, FTHG, FTAG) %>%
    rename(c("GoalsFor" = "FTAG",
             "GoalsAgainst" = "FTHG",
             "TeamAbbreviation" = "AwayTeamAbbreviation"))
  
  # Work out mean prior goals
  goals <- rbind(home_goals, away_goals) %>%
    arrange(Season, TeamAbbreviation, Date) %>%
    group_by(Season, TeamAbbreviation) %>%
    mutate(r = rowid(TeamAbbreviation),
           MPFG = ifelse(rowid(TeamAbbreviation) > 1,
                         (cumsum(GoalsFor) - GoalsFor) / (r - 1),
                         0),
           MPAG = ifelse(rowid(TeamAbbreviation) > 1,
                         (cumsum(GoalsAgainst) - GoalsAgainst) / (r - 1),
                         0),) %>%
    select(Season, Date, TeamAbbreviation, MPFG, MPAG)
  
  # Add the mean prior goal data back
  match_results <- match_results %>%
    left_join(goals,
              by = c("Season",
                     "Date",
                     "HomeTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("HomeMPFG" = "MPFG",
             "HomeMPAG" = "MPAG"))  %>%
    left_join(goals,
              by = c("Season",
                     "Date",
                     "AwayTeamAbbreviation" = "TeamAbbreviation")) %>%
    rename(c("AwayMPFG" = "MPFG",
             "AwayMPAG" = "MPAG"))
  
  # Clean up
  # --------
  # Remove unnecessary variables
  rm(away_discipline, home_discipline, discipline,
     away_goals, home_goals, goals,
     away_points, home_points, points,
     start_date, temp)
  
  # Remove unnecessary fields
  match_results <- match_results %>% 
    select(-start_date,
           -AwayTeamSquadSize, -HomeTeamSquadSize,
           -AwayTeamSquadSize, -HomeTeamSquadSize,
           -FTR,
           -HR, -AR, -HY, -AY)
  
  # Function to calculate RMSE. 
  # ==========================
  fn_RMSE <- function(actual_goals, forecast_goals){
    sqrt(mean((actual_goals - forecast_goals)^2))
  }
  
  # Test, train and validation
  # ==========================
  # Seed set for repeatability
  set.seed(72, sample.kind="Rounding") 
  
  # EPL and holdout
  # ---------------
  # Training and holdout data sets
  holdout <- match_results %>% filter(Season=='2019-2020')
  epl <- match_results %>% filter(Season!='2019-2020')
  
  # Machine learning
  # ================
  
  # Definitions for machine learning models
  # ---------------------------------------
  # Define standard arguments for the train function. This ensures every
  # model trained with the same features.
  train_formula_home <- as.formula(FTHG ~ WeekNumber + 
                                   AwayTeamValue + HomeTeamValue +
                                   AwayTeamForeignPlayers + 
                                   HomeTeamForeignPlayers +
                                   AwayTeamMeanAge + HomeTeamMeanAge +  
                                   AwayMPP + HomeMPP +
                                   AwayMPCR + HomeMPCR + 
                                   AwayMPCY + HomeMPCY +
                                   AwayMPAG + AwayMPFG + 
                                   HomeMPAG + HomeMPFG)
  
  train_formula_away <- as.formula(FTAG ~ WeekNumber + 
                                   AwayTeamValue + HomeTeamValue +
                                   AwayTeamForeignPlayers + 
                                   HomeTeamForeignPlayers +
                                   AwayTeamMeanAge + HomeTeamMeanAge +  
                                   AwayMPP + HomeMPP +
                                   AwayMPCR + HomeMPCR + 
                                   AwayMPCY + HomeMPCY +
                                   AwayMPAG + AwayMPFG + 
                                   HomeMPAG + HomeMPFG)
  
  # Train control uses cross validation. Define standard train control
  # so we have consistency across models.
  train_control <- trainControl(method = "repeatedcv", 
                                number = 10, 
                                repeats = 3, 
                                p = 0.9,
                                allowParallel = TRUE,
                                predictionBounds = c(0, 9))
  
  # Baseline
  # --------
  # Use mean scores to calculate baseline RMSEs I need to improve upon. 
  RMSE_mean_home <- fn_RMSE(holdout$FTHG, mean(epl$FTHG))
  print(sprintf("RMSE mean home %s", RMSE_mean_home))
  RMSE_mean_away <- fn_RMSE(holdout$FTAG, mean(epl$FTAG))
  print(sprintf("RMSE away mean %s", RMSE_mean_away))
  
  # Generalized linear model
  # ------------------------
  # All models follow the same pattern. The function is run for both home
  # and away. The function calculates the minimum RMSE and returns the plot.
  fn_RMSE_glm <- function(formula_) {
    fit_glm <- train(form=formula_, 
                     method = "glm",
                     data = epl,
                     trControl = train_control,
                     preProcess = c("center", "scale", "nzv"),
                     metric='RMSE',
                     maximize=FALSE)
    predict_glm <- predict(fit_glm, newdata=holdout)  
    RMSE_glm <- RMSE(holdout[,as.character(formula_[[2]])], predict_glm) 
    RMSE_glm
  }
  # Get home results
  RMSE_glm_home <- fn_RMSE_glm(train_formula_home)
  print(sprintf("RMSE home glm %s", RMSE_glm_home))
  # Get away results
  RMSE_glm_away <- fn_RMSE_glm(train_formula_away)
  print(sprintf("RMSE away glm %s", RMSE_glm_away))
  
  # glmnet
  # ------
  # Follow standard pattern - see comments for glm model
  fn_RMSE_glmnet <- function(formula_) {
    fit_glmnet <- train(form=formula_, 
                        method = "glmnet",
                        data = epl,
                        trControl = train_control,
                        preProcess = c("center", "scale", "nzv"),
                        metric='RMSE',
                        maximize=FALSE,
                        tuneGrid = expand.grid(alpha = 0:1, 
                                               lambda = seq(0, 0.40, by=0.01)))
    predict_glmnet <- predict(fit_glmnet, newdata=holdout)  
    RMSE_glmnet <- RMSE(holdout[,as.character(formula_[[2]])], predict_glmnet) 
    RMSE_glmnet
  }
  RMSE_glmnet_home <- fn_RMSE_glmnet(train_formula_home)
  print(sprintf("RMSE home glmnet %s", RMSE_glmnet_home))
  RMSE_glmnet_away <- fn_RMSE_glmnet(train_formula_away)
  print(sprintf("RMSE away glmnet %s", RMSE_glmnet_away))
  
  # SVM
  # ---
  # Follow standard pattern - see comments for glm model
  fn_RMSE_svm <- function(formula_) {
    fit_svm <- train(form=formula_, 
                     method = "svmRadial",
                     data = epl,
                     metric='RMSE',
                     maximize=FALSE,
                     trControl = train_control,
                     preProcess = c("center", "scale", "nzv"),
                     tuneGrid = expand.grid(
                       C = seq(from=0.2, to=8, by=0.1),
                       sigma = seq(0.0001, 0.010, 0.001)))
    
    svm_plot <- fit_svm$results %>% 
      ggplot(aes(x=C, y=RMSE, color=factor(sigma))) +
      geom_point() +
      ggtitle(sprintf("SVM for %s. rmse vs. C", as.character(formula_[[2]]))) +
      ylab("RMSE") +
      xlab("C")
    
    predict_svm <- predict(fit_svm, newdata=holdout)  
    RMSE_svm <- RMSE(holdout[,as.character(formula_[[2]])], predict_svm) 
    list(RMSE_svm, svm_plot)
  }
  svm_home <- fn_RMSE_svm(train_formula_home)
  # use of [[]] to get to underlying numeric value or plot
  RMSE_svm_home <- svm_home[[1]]
  plt_svm_home <- svm_home[[2]]
  print(sprintf("RMSE home svm %s", RMSE_svm_home))
  print(plt_svm_home)
  
  svm_away <- fn_RMSE_svm(train_formula_away)
  RMSE_svm_away <- svm_away[[1]]
  plt_svm_away <- svm_away[[2]]
  print(sprintf("RMSE away svm %s", RMSE_svm_away))
  print(plt_svm_away)
  
  # knn
  # ---
  # Follow standard pattern - see comments for glm model
  fn_RMSE_knn <- function(formula_) {
    fit_knn <- train(form=formula_, 
                     method = "knn",
                     data = epl,
                     metric='RMSE',
                     maximize=FALSE,
                     trControl = train_control,
                     preProcess = c("center", "scale", "nzv"),
                     tuneGrid = expand.grid(k = 1:40))
    
    knn_plot <- fit_knn$results %>% ggplot(aes(x=k, y=RMSE)) +
      geom_point(color='blue') +
      ggtitle(sprintf("knn for %s. rmse vs. k", as.character(formula_[[2]]))) +
      ylab("RMSE") +
      xlab("k")
    
    predict_knn <- predict(fit_knn, newdata=holdout)  
    RMSE_knn <- RMSE(holdout[,as.character(formula_[[2]])], predict_knn)
    list(RMSE_knn, knn_plot)
  }
  knn_home <- fn_RMSE_knn(train_formula_home)
  RMSE_knn_home <- knn_home[[1]]
  plt_knn_home <- knn_home[[2]]
  print(sprintf("RMSE home knn %s", RMSE_knn_home))
  print(plt_knn_home)
  
  knn_away <- fn_RMSE_knn(train_formula_away)
  RMSE_knn_away <- knn_away[[1]]
  plt_knn_away <- knn_away[[2]]
  print(sprintf("RMSE away knn %s", RMSE_knn_away))
  print(plt_knn_away)
  
  # xgbLinear
  # ---------
  # Follow standard pattern - see comments for glm model
  fn_RMSE_xgbLinear <- function(formula_) {
    xgbTuningGrid = expand.grid(nrounds = seq(4, 9, 1), 
                                lambda = seq(0.1, 2, 0.2), # No change
                                alpha = seq(0.1, 0.5, 0.1), # No change
                                eta = c(0.3, 0.4)) # No change
    
    fit_xgbLinear <- train(form=formula_, 
                           method = "xgbLinear",
                           data = epl,
                           metric='RMSE',
                           maximize=FALSE,
                           trControl=train_control,
                           preProcess = c("center", "scale", "nzv"),
                           tuneGrid=xgbTuningGrid)
    
    plot_xgbLinear <- fit_xgbLinear$results %>% ggplot(aes(x=nrounds, y=RMSE)) +
      geom_point(color='blue') +
      ggtitle(sprintf("xgbLinear for %s. rmse vs. nrounds", 
                      as.character(formula_[[2]]))) +
      ylab("RMSE") +
      xlab("nrounds")
    
    predict_xgbLinear <- predict(fit_xgbLinear, newdata=holdout)  
    RMSE_xgbLinear <- RMSE(holdout[,as.character(formula_[[2]])], 
                           predict_xgbLinear)
    list(RMSE_xgbLinear, plot_xgbLinear)
  }
  xgbLinear_home <- fn_RMSE_xgbLinear(train_formula_home)
  RMSE_xgbLinear_home <- xgbLinear_home[[1]]
  plt_xgbLinear_home <- xgbLinear_home[[2]]
  print(sprintf("RMSE home xgbLinear %s", RMSE_xgbLinear_home))
  print(plt_xgbLinear_home)
  
  xgbLinear_away <- fn_RMSE_xgbLinear(train_formula_away)
  RMSE_xgbLinear_away <- xgbLinear_away[[1]]
  plt_xgbLinear_away <- xgbLinear_away[[2]]
  print(sprintf("RMSE away xgbLinear %s", RMSE_xgbLinear_away))
  print(plt_xgbLinear_away)
  
  # xgbTree
  # -------
  # Follow standard pattern - see comments for glm model
  fn_RMSE_xgbTree <- function(formula_) {
    xgbTuningGrid <- expand.grid(nrounds = seq(from=1, to=250, by=20),
                                 eta = c(0.01, 0.025, 0.05, 0.1, 0.3),
                                 max_depth = c(1, 2, 3, 4, 5),
                                 gamma = c(0, 1),
                                 colsample_bytree = seq(0.5, 0.9, 0.1),
                                 min_child_weight = 1,
                                 subsample = 1)
    
    fit_xgbTree <- train(form=formula_, 
                         method = "xgbTree",
                         data = epl,
                         metric='RMSE',
                         maximize=FALSE,
                         trControl=train_control,
                         preProcess = c("center", "scale", "nzv"),
                         tuneGrid=xgbTuningGrid)
    
    plot_xgbTree <- fit_xgbTree$results %>% ggplot(aes(x=nrounds, y=RMSE)) +
      geom_point(color='blue') +
      ggtitle(sprintf("xgbTree for %s. rmse vs. nrounds", 
                      as.character(formula_[[2]]))) +
      ylab("RMSE") +
      xlab("nrounds")
    
    predict_xgbTree <- predict(fit_xgbTree, newdata=holdout)  
    RMSE_xgbTree <- RMSE(holdout[,as.character(formula_[[2]])], 
                         predict_xgbTree)
    list(RMSE_xgbTree, plot_xgbTree)
  }
  xgbTree_home <- fn_RMSE_xgbTree(train_formula_home)
  RMSE_xgbTree_home <- xgbTree_home[[1]]
  plt_xgbTree_home <- xgbTree_home[[2]]
  print(sprintf("RMSE home xgbTree %s", RMSE_xgbTree_home))
  print(plt_xgbTree_home)
  
  xgbTree_away <- fn_RMSE_xgbTree(train_formula_away)
  RMSE_xgbTree_away <- xgbTree_away[[1]]
  plt_xgbTree_away <- xgbTree_away[[2]]
  print(sprintf("RMSE away xgbTree %s", RMSE_xgbTree_away))
  print(plt_xgbTree_away)
  
  # Neural network
  # --------------
  # Follow standard pattern - see comments for glm model
  fn_RMSE_nnet <- function(formula_) {
    fit_nnet <- train(form=formula_, 
                      method = "nnet",
                      data = epl,
                      metric='RMSE',
                      maximize=FALSE,
                      linout = TRUE,
                      trControl = train_control,
                      preProcess = c("center", "scale", "nzv"),
                      tuneGrid = expand.grid(.size = seq(1, 10, 1), 
                                             .decay = seq(0.1, 5, 0.05)))
    
    nnet_plot <- fit_nnet$results %>% ggplot(aes(x=decay, y=RMSE)) +
      geom_point(color='blue') +
      ggtitle(sprintf("nnet for %s. rmse vs. k", as.character(formula_[[2]]))) +
      ylab("RMSE") +
      xlab("k")
    
    predict_nnet <- predict(fit_nnet, newdata=holdout)  
    RMSE_nnet <- RMSE(holdout[,as.character(formula_[[2]])], predict_nnet)
    list(RMSE_nnet, nnet_plot)
  }
  nnet_home <- fn_RMSE_nnet(train_formula_home)
  RMSE_nnet_home <- nnet_home[[1]]
  plt_nnet_home <- nnet_home[[2]]
  print(sprintf("RMSE home nnet %s", RMSE_nnet_home))
  print(plt_nnet_home)
  
  nnet_away <- fn_RMSE_nnet(train_formula_away)
  RMSE_nnet_away <- nnet_away[[1]]
  plt_nnet_away <- nnet_away[[2]]
  print(sprintf("RMSE away nnet %s", RMSE_nnet_away))
  print(plt_nnet_away)
  
  # Random Forest - calculate ntree etc.
  # ------------------------------------
  # First step is to find the optimum number of trees. I create a test and
  # train data set to do this.
  epl_train <- match_results %>% filter(Season!='2018-2019')
  epl_test <- match_results %>% filter(Season=='2018-2019')
  
  # Function calculates RMSE for an ntree value
  fn_rf_model <- function(ntree, formula_) {
    fit_rf <- train(form=formula_,
                    method = "rf",
                    data = epl_train,
                    metric='RMSE',
                    maximize=FALSE,
                    trControl = train_control,
                    preProcess = c("center", "scale", "nzv"),
                    tuneGrid = expand.grid(.mtry=seq(1:20)),
                    ntree=ntree)
    
    predict_rf <- predict(fit_rf, newdata=epl_test)  
    RMSE_rf <- RMSE(epl_test[,as.character(formula_[[2]])], predict_rf)
    print(sprintf("branch=%s, ntree=%d, RMSE=%f", 
                  as.character(formula_[[2]]), ntree, RMSE_rf))
    
    RMSE_rf
  }
  
  # Here are the number of trees to try, I found them by hand experimentation.
  ntrees <- sort(c(seq(from=1, to=150, by=1),
                   seq(from=160, to=250, by=10)))
  
  # Get the RMSE for each tree value
  rf_home <- sapply(ntrees, fn_rf_model, formula_=train_formula_home)
  rf_away <- sapply(ntrees, fn_rf_model, formula_=train_formula_away)
  
  # The optimal number of trees for home and away
  df_home <- data.frame(ntrees=ntrees, rmse=rf_home)
  df_away <- data.frame(ntrees=ntrees, rmse=rf_away)
  
  ntree_home <- df_home[which.min(df_home$rmse), 'ntrees']
  ntree_away <- df_home[which.min(df_away$rmse), 'ntrees']
  
  # Build a chart of RMSE vs. ntrees
  plt_rf_home <- df_home %>% ggplot(aes(x=ntrees, y=rmse)) +
    geom_point(color='blue') +
    ggtitle("Random forest for Home version. rmse vs. ntrees.") +
    ylab("rmse") +
    xlab("ntrees")
  print(plt_rf_home)
  
  plt_rf_away <- df_away %>% ggplot(aes(x=ntrees, y=rmse)) +
    geom_point(color='blue') +
    ggtitle("Random forest for Away version. rmse vs. ntrees.") +
    ylab("rmse") +
    xlab("ntrees")
  print(plt_rf_away)
  
  # Random Forest - final model
  # ---------------------------
  # We now the number of trees. Code follows same format at glm model.
  fn_rf_model_final <- function(ntree, formula_) {
    fit_rf <- train(form=formula_,
                    method = "rf",
                    data = epl,
                    metric='RMSE',
                    maximize=FALSE,
                    trControl = train_control,
                    preProcess = c("center", "scale", "nzv"),
                    tuneGrid = expand.grid(.mtry=seq(1:20)),
                    ntree=ntree)
    
    predict_rf <- predict(fit_rf, newdata=holdout)  
    RMSE_rf <- RMSE(holdout[,as.character(formula_[[2]])], predict_rf)
    
    list(RMSE_rf, varImp(fit_rf), fit_rf$bestTune$mtry)
  }
  
  rf_home <- fn_rf_model_final(ntree_home, train_formula_home)
  RMSE_rf_home <- rf_home[[1]]
  varImp_rf_home <- rf_home[[2]]
  
  rf_away <- fn_rf_model_final(ntree_away, train_formula_away)
  RMSE_rf_away <- rf_away[[1]]
  varImp_rf_away <- rf_away[[2]]
  
  print(sprintf("RMSE home rf %s", RMSE_rf_home))
  print("Home varImp")
  print(varImp_rf_home)
  print(sprintf("RMSE away rf %s", RMSE_rf_away))
  print("Away varImp")
  print(varImp_rf_away)
  
  # Tidying up
  # ==========
  # Build the final model table for use in the report
  rmse_table <-data.frame(Method=c('naive mean',
                                   'glm',
                                   'glmnet',
                                   'xgbLinear',
                                   'xgbTree',
                                   'svm',
                                   'knn',
                                   'nnet',
                                   'random forest'), 
                          `RMSE.away`=c(RMSE_mean_away,
                                      RMSE_glm_away,
                                      RMSE_glmnet_away,
                                      RMSE_xgbLinear_away,
                                      RMSE_xgbTree_away,
                                      RMSE_svm_away,
                                      RMSE_knn_away,
                                      RMSE_nnet_away,
                                      RMSE_rf_away),
                          `RMSE.home`=c(RMSE_mean_home,
                                      RMSE_glm_home,
                                      RMSE_glmnet_home,
                                      RMSE_xgbLinear_home,
                                      RMSE_xgbTree_home,
                                      RMSE_svm_home,
                                      RMSE_knn_home,
                                      RMSE_nnet_home,
                                      RMSE_rf_home))
  
  modeling_folder = 'model'
  if (!dir.exists(modeling_folder)) {dir.create(modeling_folder)}
  
  # Save the variables to disk - we'll read them in in the report Rmd.
  save(RMSE_mean_away, RMSE_mean_home,
       RMSE_glm_away, RMSE_glm_home,
       RMSE_glmnet_away, RMSE_glmnet_home,
       RMSE_svm_away, RMSE_svm_home,
       RMSE_knn_away, RMSE_knn_home,
       RMSE_nnet_away, RMSE_nnet_home,
       RMSE_rf_away, RMSE_rf_home,
       RMSE_xgbLinear_away, RMSE_xgbLinear_home,
       RMSE_xgbTree_away,RMSE_xgbTree_home,
       ntree_away, ntree_home,
       varImp_rf_away, varImp_rf_home,
       plt_svm_home, plt_svm_away,
       plt_xgbLinear_home, plt_xgbLinear_away,
       plt_xgbTree_home, plt_xgbTree_away,
       plt_knn_home, plt_knn_away,
       plt_nnet_home, plt_nnet_away,
       plt_rf_home, plt_rf_away,
       rmse_table,
       file=file.path(modeling_folder, "model.rda"))
  
  # Stop the cluster
  stopCluster(cl)
  
  # Script duration
  script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
  script_duration <- sprintf('%d minutes %.1f seconds', 
                             script_duration%/%60, 
                             script_duration%%60)
  print(paste("Model duration was", script_duration))
}

################################################################################
# Execution                                                                    #
################################################################################

# Housekeeping loads the relevant libraries - it must *always* be run.
housekeeping()

# Download downloads the files from the internet. It can take 25 minutes to run.
# Only call this function if you haven't already downloaded the data.
#download()

# Data cleaning. Can be run without running download if the data has already
# been downloaded. Writes results to an rda file.
clean()

# Data analysis. Can be run on its own, provided download and clean have been
# run previously. Reads in cleaned data from an rda file. Outputs charts
# and variables for use in the Rmd report.
dataanalysis()

# Model. Download must have been run previously.Clean must be run prior to 
# running model. Note: does not depend on dataanalysis having been run.
model()

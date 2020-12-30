# Harvard PH128.9x Capstone project: English Premiership prediction
# =================================================================

# Loads the downloaded data, cleans it, and outputs a cleaned data frame,
# match_results, for analysis. Script runs in 3-4 seconds.

# Housekeeping
# ============
# Loading packages and defining functions etc. Basic error checking.

# Clears out the r workspace each time this file is run. 
rm(list=ls(all=TRUE))
# Clears graphics settings
while (!is.null(dev.list())) dev.off()

# Install packages if necessary
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages(
  "caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages(
  "data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(rvest)

# Start the clock to time the script execution
ptm <- proc.time()

# Check download folders exist
data_folder = 'data_download'
if (!dir.exists(data_folder)) {
  stop(message(sprintf("No %s folder - run download first", data_folder)))
}

# Load data
# =========
# Load team abbreviations
# ------------------------
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
if (length(results_files) == 0) {
  stop(message("No results files - run downloads first"))  
}

fields <- c('Date', 'HomeTeam', 'AwayTeam', 
            'FTHG', 'FTAG', 'FTR', 
            'HR', 'AR', 'HY', 'AY')

read_match <- function(result) {
  season <- read.csv(result)[ ,fields]
  # Add the season
  basename_result <- basename(result)
  basename_result_length <- nchar(basename_result)
  season['Season'] <- sprintf("%s", 
                              substr(basename_result, 
                                     basename_result_length - 12, 
                                     basename_result_length - 4))
  # Remove empty rows
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
match_results <- do.call(rbind, lapply(results_files, read_match))

match_results <- match_results %>% 
  left_join(team_abbreviations, by = c("HomeTeam" = "TeamName")) %>%
  rename(c("HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  left_join(team_abbreviations, by = c("AwayTeam" = "TeamName")) %>%
  rename(c("AwayTeamAbbreviation"="TeamAbbreviation"))

# Load team values
# ----------------
values_folder = file.path(data_folder, 'team_values')
if (!dir.exists(values_folder)) {
  stop(message(sprintf("No %s folder - run download first", team_values)))
}
values_files <- list.files(values_folder, 
                           include.dirs = FALSE, 
                           full.names = TRUE, 
                           recursive = TRUE)

if (length(values_files) == 0) {
  stop(message("No results files - run downloads first"))  
}

values <- do.call(rbind,lapply(values_files, function(file_) {
  read.csv(file=file_, encoding="UTF-8")}))
values <- values %>% select(Date, Team, Value, Squad.size)
# This code produces two spurious warnings about NAs
values$Value <- ifelse(
  grepl('bn', values$Value), 
  1000*as.numeric(str_remove_all(values$Value, '[£bn]')), 
  as.numeric(str_remove_all(values$Value, '[£m]')))

values$Team <- str_remove_all(values$Team, " FC")
values['Date'] <- as.Date(values$Date)

values <- values %>% 
  complete(Date = seq.Date(min(Date), max(match_results$Date), by="day"),
           Team) %>%
  arrange(Team, Date) %>%
  group_by(Team) %>% 
  fill(Value, Squad.size) %>%
  ungroup()

values <- values %>% left_join(team_abbreviations, 
                               by = c("Team" = "TeamName"))

# Load foreign and age
# --------------------
foreign_folder = file.path(data_folder, 'foreign_age')
if (!dir.exists(foreign_folder)) {
  stop(message(sprintf("No %s folder - run download first", foreign_folder)))
}
foreign_files <- list.files(foreign_folder, 
                            include.dirs = FALSE, 
                            full.names = TRUE, 
                            recursive = TRUE)
if (length(foreign_files) == 0) {
  stop(message("No foreign files - run downloads first"))  
}

foreign <- do.call(rbind, lapply(foreign_files, read.csv))

foreign <- foreign %>% left_join(team_abbreviations, 
                                 by = c("Team" = "TeamName"))

# Merge to create team and values data frame
# ==========================================
match_results <- match_results %>%
  left_join(values %>% select(Date, Value, Squad.size, TeamAbbreviation), 
            by = c("HomeTeamAbbreviation" = "TeamAbbreviation",
                   "Date")) %>%
  rename(c("HomeTeamValue"="Value", "HomeTeamSquadSize"="Squad.size")) %>%
  left_join(values %>% select(Date, Value, Squad.size, TeamAbbreviation), 
            by = c("AwayTeamAbbreviation" = "TeamAbbreviation",
                   "Date")) %>%
  rename(c("AwayTeamValue"="Value", "AwayTeamSquadSize"="Squad.size")) %>%
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
  rename(c("AwayTeamMeanAge"="MeanAge", 
           "AwayTeamForeignPlayers"="ForeignPlayers"))

# Add new columns
# HGD - home team goal difference, home team goals - away team goals
match_results <- match_results %>% 
  mutate(HGD = FTHG - FTAG,
         AGD = FTAG - FTHG)

match_results %>% write.csv("results_value_foreign.csv", row.names = FALSE)

# Tidying up
# ==========
# Script duration
script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
script_duration <- sprintf('%d minutes %.1f seconds', 
                           script_duration%/%60, 
                           script_duration%%60)
print(paste("Script duration was", script_duration))

# Save match_results 
match_folder = 'data_match'
if (!dir.exists(match_folder)) {dir.create(match_folder)}
save(match_results, 
     file=file.path(match_folder, "match_results.rda"))


# Harvard PH128.9x Capstone project: English Premiership prediction
# =================================================================


# Housekeeping
# ============
# Loading packages and defining functions etc.

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

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(rvest)

# Start the clock to time the script execution
ptm <- proc.time()


# Create the download folder structure
data_folder = 'data_download'
if (!dir.exists(data_folder)) {dir.create(data_folder)}
results_folder = file.path(data_folder, 'match_results')
if (!dir.exists(results_folder)) {dir.create(results_folder)}
values_folder = file.path(data_folder, 'team_values')
if (!dir.exists(values_folder)) {dir.create(values_folder)}

# Data download
# =============

# Match results by season
# -----------------------
match_results <- function(){
  
  years <- seq(1993, 2020)

  # Remove existing files if they exist
  f <- list.files(results_folder, 
                  include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
  file.remove(f)
  rm(f)
  
  for (year in years) {
    season <- paste(substr(toString(year), 3, 4), 
                    substr(toString(year +1), 3, 4), 
                    sep="")
    url <- paste("https://www.football-data.co.uk/mmz4281/", 
                 season, 
                 "/E0.csv",
                 sep="")
    filename <- file.path(
      results_folder,
      paste("match", season, '.csv', sep=''))
    download.file(url, filename)
  }
}

match_results()

# Team value
# ----------

team_values <- function() {
  
  # Remove existing files if they exist
  f <- list.files(values_folder, 
                  include.dirs = FALSE, full.names = TRUE, recursive = TRUE)
  file.remove(f)
  rm(f)  
  
  # Build up list of dates to request from site
  days <- c("01", "15")
  months <- sprintf("%02d", seq(1, 12))
  years <- sprintf("%4d", seq(2010, 2020))
  temp <- expand.grid(list(years, months, days))
  dates <- paste(temp[,1], temp[,2], temp[,3], sep="-")
  odd_dates <- c("2014-07-10", "2014-10-23", "2017-06-19")
  dates <- sort(c(dates, odd_dates))
  # Select dates after "2010-10-31"
  dates <- dates[dates > "2010-10-31"]
  
  base_url <- "https://www.transfermarkt.com/premier-league/marktwerteverein/wettbewerb/GB1/stichtag/%s/plus/1"
  for (date in dates) { 
    # Build the url
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
    # nothing to save
    if (df['Squad size'] %>% nrow() != sum(is.na(df['Squad size']))){
      df['Date'] <- date
      filename <- file.path(
        values_folder,
        paste(date, '.csv', sep=''))
      write.csv(df, filename)
    }
  }
}

team_values()

# Tidying up
# ==========
# Script duration
script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
td <- seconds_to_period(script_duration)
script_duration <- sprintf('%d minutes %.1f seconds', minute(td), second(td))
print(paste("Script duration was", script_duration))
# Harvard PH128.9x Capstone project: English Premiership prediction
# =================================================================

# Data analysis of cleaned data.

# Housekeeping
# ============
# Loading packages and defining functions etc. Basic error checking.

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

# Check download folders exist
match_folder = 'data_match'
if (!dir.exists(match_folder)) {
  stop(message(sprintf("No %s folder - run download first, then cleaning", 
                       match_folder)))
}

load(file.path(match_folder, 'match_results.rda'))

# Home team advantage
# ===================

home_advantage <- match_results %>% 
       group_by(Season) %>%
       summarize(home_wins=sum(FTR == 'H'),
                 away_wins=sum(FTR == 'A'),
                 draws=sum(FTR=='D'),
                 matches=n(),
                 proportion_home=home_wins/(home_wins+away_wins),
                 se_proportion_home=sqrt(
                   proportion_home*(1-proportion_home)/(home_wins+away_wins)),
                 m_HGD=mean(HGD),
                 se_HGD=sd(HGD)/matches,
                 .groups='keep')
home_advantage

# This number is quoted in the final report
home_wins_proportion_2020_2021 <- home_advantage %>% 
  filter(Season=='2020-2021') %>% .$proportion_home

plt_home_wins <- home_advantage %>%
  ggplot(aes(x=Season, 
             y=proportion_home,
             ymin = proportion_home - 1.96*se_proportion_home, 
             ymax = proportion_home + 1.96*se_proportion_home)) +    
  geom_point(color='blue') + 
  geom_errorbar(color='blue') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        aspect.ratio=0.4) +
  ggtitle("Fraction of home wins vs. season") +
  ylab("Fraction wins that are home wins") +
  xlab("Season")
print(plt_home_wins)

plt_home_goal_difference <- home_advantage %>%
  ggplot(aes(x=Season, 
             y=m_HGD,
             ymin = m_HGD - 1.96*se_HGD, 
             ymax = m_HGD + 1.96*se_HGD)) +    
  geom_point(color='blue') + 
  geom_errorbar(color='blue') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        aspect.ratio=0.4) +
  ggtitle("Home team goal difference by season") +
  ylab("Home team goal difference") +
  xlab("Season")
print(plt_home_goal_difference)

# Team value advantage
# --------------------
season_selection <- '2017-2018'
temp <- match_results %>% filter(Season==season_selection)
temp <- temp %>% 
  mutate(deltaV=ifelse(HomeTeamValue > AwayTeamValue,
                       HomeTeamValue - AwayTeamValue,
                       AwayTeamValue - HomeTeamValue),
         deltag=ifelse(HomeTeamValue > AwayTeamValue,
                       HGD,
                       AGD))
temp %>% ggplot(aes(x=deltaV, y=HGD)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle(sprintf("Goal difference vs. team value difference for season %s",
                  season_selection)) +
  ylab("Goal difference") +
  xlab("Difference in team value (£ millions)")

# Foreign player advantage
# ------------------------
season_selection <- '2018-2019'
temp <- match_results %>% filter(Season==season_selection)
temp <- temp %>% 
  mutate(deltaF=ifelse(HomeTeamForeignPlayers > AwayTeamForeignPlayers,
                       HomeTeamForeignPlayers - AwayTeamForeignPlayers,
                       AwayTeamForeignPlayers - HomeTeamForeignPlayers),
         deltag=ifelse(HomeTeamForeignPlayers > AwayTeamForeignPlayers,
                       HGD,
                       AGD))
temp %>% ggplot(aes(x=deltaF, y=HGD)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle(sprintf("Goal difference vs. foreign player difference for season %s",
                  season_selection)) +
  ylab("Goal difference") +
  xlab("Difference in foreign players")

# Tidying up
# ==========
# Script duration
script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
script_duration <- sprintf('%d minutes %.1f seconds', 
                           script_duration%/%60, 
                           script_duration%%60)
print(paste("Script duration was", script_duration))

# Save results 
analysis_folder = 'analysis'
if (!dir.exists(analysis_folder)) {dir.create(analysis_folder)}
save(home_wins_proportion_2020_2021, 
     plt_home_wins,
     plt_home_goal_difference,
     file=file.path(analysis_folder, "analysis.rda"))

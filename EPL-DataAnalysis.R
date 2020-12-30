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
# ====================
temp <- match_results %>% 
  mutate(deltaV=ifelse(HomeTeamValue > AwayTeamValue,
                       HomeTeamValue - AwayTeamValue,
                       AwayTeamValue - HomeTeamValue),
         deltaG=ifelse(HomeTeamValue > AwayTeamValue,
                       HGD,
                       AGD)) 
plt_team_value <- temp %>% ggplot(aes(x=deltaV, y=deltaG)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. team value difference for season %s") +
  ylab("Goal difference") +
  xlab("Difference in team value (£ millions)")
print(plt_team_value)

# Foreign player advantage
# ========================
temp <- match_results %>% 
  mutate(deltaF=ifelse(HomeTeamForeignPlayers > AwayTeamForeignPlayers,
                       HomeTeamForeignPlayers - AwayTeamForeignPlayers,
                       AwayTeamForeignPlayers - HomeTeamForeignPlayers),
         deltaG=ifelse(HomeTeamForeignPlayers > AwayTeamForeignPlayers,
                       HGD,
                       AGD))
plt_foreign <- temp %>% ggplot(aes(x=deltaF, y=deltaG)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. foreign player difference") +
  ylab("Goal difference") +
  xlab("Difference in foreign players")
print(plt_foreign)

# Mean age advantage
# ==================
temp <- match_results %>% 
  mutate(deltaA=ifelse(HomeTeamMeanAge > AwayTeamMeanAge,
                       HomeTeamMeanAge - AwayTeamMeanAge,
                       AwayTeamMeanAge - HomeTeamMeanAge),
         deltaG=ifelse(HomeTeamMeanAge > AwayTeamMeanAge,
                       HGD,
                       AGD))
plt_age <- temp %>% ggplot(aes(x=deltaA, y=deltaG)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. mean age difference") +
  ylab("Goal difference") +
  xlab("Difference in mean age")
print(plt_age)

# Squad size advantage
# ====================
temp <- match_results %>% 
  mutate(deltaA=ifelse(HomeTeamSquadSize > AwayTeamSquadSize,
                       HomeTeamSquadSize - AwayTeamSquadSize,
                       AwayTeamSquadSize - HomeTeamSquadSize),
         deltaG=ifelse(HomeTeamSquadSize > AwayTeamSquadSize,
                            HGD,
                            AGD))
plt_size <- temp %>% ggplot(aes(x=deltaA, y=deltaG)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. squad size difference") +
  ylab("Goal difference") +
  xlab("Difference in squad size")
print(plt_size)

# Time of season effects
# ======================
start_date <- match_results %>% 
  select(Season, Date) %>% 
  unique() %>% 
  group_by(Season) %>% 
  summarize(Season=Season[1],
            start_date=min(Date),
            day_offset=7*floor(yday(min(start_date)) /7))
temp <- match_results %>% left_join(start_date, by='Season') %>%
  mutate(season_week=week(Date - day_offset)) %>%
  group_by(season_week) %>%
  summarize(draw_proportion=sum(FTR=='D')/n())
            
plt_season <- temp %>% ggplot(aes(x=season_week, y=draw_proportion)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Draw proportion vs. week in season") +
  ylab("Draw proportion") +
  xlab("Week in season")
print(plt_season)

# Discipline effects
# ==================
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

discipline <- rbind(home_discipline, away_discipline) %>%
  arrange(Season, Date) %>%
  group_by(Season, TeamAbbreviation) %>%
  mutate(r=rowid(TeamAbbreviation),
         MPCR=ifelse(r>1, (cumsum(R) - R)/(r-1), 0),
         MPCY=ifelse(r>1, (cumsum(Y) - Y)/(r-1), 0)) %>%
  arrange(Season, TeamAbbreviation, Date) %>%
  select(Season, Date, TeamAbbreviation, MPCR, MPCY)
head(discipline)

temp <- match_results %>% 
  left_join(discipline,
            by=c("Season", "Date", 
                 "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("HomeMPCR"="MPCR", "HomeMPCY"="MPCY"))  %>% 
  left_join(discipline,
            by=c("Season", "Date", 
                 "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("AwayMPCR"="MPCR", "AwayMPCY"="MPCY"))

temp <- temp %>% 
  mutate(deltaMPCR=ifelse(HomeMPCR > AwayMPCR,
                          HomeMPCR - AwayMPCR,
                          AwayMPCR - HomeMPCR),
         deltaGR=ifelse(HomeMPCR > AwayMPCR,
                        HGD,
                        AGD),
         deltaMPCY=ifelse(HomeMPCY > AwayMPCY,
                          HomeMPCY - AwayMPCY,
                          AwayMPCY - HomeMPCY),
         deltaGY=ifelse(HomeMPCY > AwayMPCY,
                        HGD,
                        AGD))
plt_red <- temp %>% ggplot(aes(x=deltaMPCR, y=deltaGR)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. cumulative average red cards") +
  ylab("Goal difference") +
  xlab("Difference in cumulative average red cards")
print(plt_red)
plt_yellow <- temp %>% ggplot(aes(x=deltaMPCY, y=deltaGY)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. cumulative average yellow cards") +
  ylab("Goal difference") +
  xlab("Difference in cumulative average yellow cards")
print(plt_yellow)

# Previous points
# ===============
temp <- match_results %>% 
  mutate(HomeTeamPoints=3*(FTR=='H') + (FTR=='D'),
         AwayTeamPoints=3*(FTR=='A') + (FTR=='D'))

home_points <- temp %>% 
  select(Season, Date, HomeTeamAbbreviation, HomeTeamPoints) %>%
  rename(c("TeamPoints"="HomeTeamPoints",
           "TeamAbbreviation"="HomeTeamAbbreviation"))

away_points <- temp %>% 
  select(Season, Date, AwayTeamAbbreviation, AwayTeamPoints) %>%
  rename(c("TeamPoints"="AwayTeamPoints",
           "TeamAbbreviation"="AwayTeamAbbreviation"))

points <- rbind(home_points, away_points) %>%
  arrange(Season, TeamAbbreviation, Date) %>%
  group_by(Season, TeamAbbreviation) %>%
  mutate(PriorCumPoints=
           ifelse(rowid(TeamAbbreviation) > 1, 
                  cumsum(TeamPoints)-TeamPoints,
                  0)) %>%
  select(Season, Date, TeamAbbreviation, PriorCumPoints)
head(points)

temp <- match_results %>% 
  left_join(points,
            by=c("Season", "Date", 
                 "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("HomePriorCumPoints"="PriorCumPoints"))  %>% 
  left_join(points,
            by=c("Season", "Date", 
                 "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("AwayPriorCumPoints"="PriorCumPoints"))

temp <- temp %>% 
  mutate(deltaPoints=ifelse(HomePriorCumPoints > AwayPriorCumPoints,
                            HomePriorCumPoints - AwayPriorCumPoints,
                            AwayPriorCumPoints - HomePriorCumPoints),
         deltaGD=ifelse(HomePriorCumPoints > AwayPriorCumPoints,
                        HGD,
                        AGD))
plt_points <- temp %>% ggplot(aes(x=deltaPoints, y=deltaGD)) + 
  geom_point(color='blue', alpha=0.3) + 
  geom_smooth(method = "lm", se=TRUE, level=0.95, formula=y ~ x)  +
  ggtitle("Goal difference vs. prior points difference") +
  ylab("Goal difference") +
  xlab("Difference in prior points")
print(plt_points)


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

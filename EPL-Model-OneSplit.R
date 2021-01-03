# Harvard PH128.9x Capstone project: English Premiership prediction
# =================================================================

# Machine learning model 

# Clears out the r workspace each time this file is run. 
rm(list=ls())
# Clears graphics settings
while (!is.null(dev.list())) dev.off()

# Install packages if necessary
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages(
  "caret", repos = "http://cran.us.r-project.org")
if(!require(elasticnet)) install.packages(
  "elasticnet", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages(
  "data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# Start the clock to time the script execution
ptm <- proc.time()

# Check data folders exist
match_folder = 'data_match'
if (!dir.exists(match_folder)) {
  stop(message(sprintf("No %s folder - run download and cleaning", 
                       match_folder)))
}

# Load and prepare data
# =====================
load(file=file.path(match_folder, "match_results.rda"))

# Select just the seasons we can analyze,
# Before 2011-2012 we have missing data
# 2020-2021 is COVID
match_results <- match_results %>% 
  filter(Season > '2010-2011'& Season != '2020-2021')

# Season week number
# ------------------
# Add the season week number field
start_date <- match_results %>% 
  select(Season, Date) %>% 
  unique() %>% 
  group_by(Season) %>% 
  summarize(Season=Season[1],
            start_date=min(Date),
            day_offset=7*floor(yday(min(start_date)) /7))  
match_results <- match_results %>% 
  left_join(start_date, by='Season') %>%
  mutate(WeekNumber=week(Date - day_offset))

# Points
# ------
# Add the points
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
  mutate(r=rowid(TeamAbbreviation),
         PriorMeanCumPoints=
           ifelse(rowid(TeamAbbreviation) > 1, 
                  (cumsum(TeamPoints)-TeamPoints)/(r-1),
                  0)) %>%
  select(Season, Date, TeamAbbreviation, PriorMeanCumPoints)
head(points)

match_results <- match_results %>% 
  left_join(points,
            by=c("Season", "Date", 
                 "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("HomePriorMeanCumPoints"="PriorMeanCumPoints"))  %>% 
  left_join(points,
            by=c("Season", "Date", 
                 "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("AwayPriorMeanCumPoints"="PriorMeanCumPoints"))


# Red cards
# ---------
home_discipline <- match_results %>% 
  select(Season, Date, HomeTeamAbbreviation, HR) %>%
  mutate(TeamAbbreviation=HomeTeamAbbreviation) %>%
  rename(c("R"="HR")) %>%
  select(Season, Date, TeamAbbreviation, R)

away_discipline <- match_results %>% 
  select(Season, Date, AwayTeamAbbreviation, AR) %>%
  mutate(TeamAbbreviation=AwayTeamAbbreviation) %>%
  rename(c("R"="AR")) %>%
  select(Season, Date, TeamAbbreviation, R)

discipline <- rbind(home_discipline, away_discipline) %>%
  arrange(Season, Date) %>%
  group_by(Season, TeamAbbreviation) %>%
  mutate(r=rowid(TeamAbbreviation),
         MPCR=ifelse(r>1, (cumsum(R) - R)/(r-1), 0)) %>%
  arrange(Season, TeamAbbreviation, Date) %>%
  select(Season, Date, TeamAbbreviation, MPCR)
head(discipline)

match_results <- match_results %>% 
  left_join(discipline,
            by=c("Season", "Date", 
                 "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("HomeMPCR"="MPCR"))  %>% 
  left_join(discipline,
            by=c("Season", "Date", 
                 "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("AwayMPCR"="MPCR"))

# Function to calculate RMSE. 
# ==========================
RMSE <- function(actual_goals, forecast_goals){
  sqrt(mean((actual_goals - forecast_goals)^2))
}

# Test, train and validation
# ==========================

set.seed(1000, sample.kind="Rounding") 

# EPL and holdout
# ---------------
holdout_index <- createDataPartition(y = match_results$FTHG, 
                                     times = 1, 
                                     p = 0.1, 
                                     list = FALSE)
epl <- match_results[-holdout_index,]
temp <- match_results[holdout_index,]

holdout <- temp %>% 
  semi_join(epl, by = "AwayTeamAbbreviation") %>%
  semi_join(epl, by = "HomeTeamAbbreviation")

# Add rows removed from holdout set back into epl set
removed <- anti_join(temp, holdout)
epl <- rbind(epl, removed)

# Test and train
# --------------
test_index <- createDataPartition(y = epl$FTHG, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)
train <- epl[-test_index,]
temp <- epl[test_index,]

# Make sure AwayTeamAbbreviation and HomeTeamAbbreviation in test set are also 
# in train set
test <- temp %>% 
  semi_join(epl, by = "AwayTeamAbbreviation") %>%
  semi_join(epl, by = "HomeTeamAbbreviation")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed, temp)

# One model, split games data sets
# --------------------------------
# We've now split the data into three groups: holdout, test, and train. We need 
# to munge the data in a form suitable for modeling.

munge_frame <- function(matches_) {

  munged_home <- matches_ %>% 
    mutate(Home=1,
           Goals=FTHG,
           ValueDifference=HomeTeamValue-AwayTeamValue,
           ForeignDifference=HomeTeamForeignPlayers-AwayTeamForeignPlayers,
           MeanAgeDifference=HomeTeamMeanAge-AwayTeamMeanAge,
           PointsDifference=HomePriorMeanCumPoints-AwayPriorMeanCumPoints,
           RedCardDifference=HomeMPCR-AwayMPCR)

  munged_away <- matches_ %>% 
    mutate(Home=0,
           Goals=FTAG,
           ValueDifference=-HomeTeamValue+AwayTeamValue,
           ForeignDifference=-HomeTeamForeignPlayers+AwayTeamForeignPlayers,
           MeanAgeDifference=-HomeTeamMeanAge+AwayTeamMeanAge,
           PointsDifference=-HomePriorMeanCumPoints+AwayPriorMeanCumPoints,
           RedCardDifference=-HomeMPCR+AwayMPCR)   
  
  munged <- rbind(munged_home, munged_away) %>%
    arrange(Season, Date, HomeTeamAbbreviation)
  
  munged <- munged %>% select(Season, 
                              Date,
                              HomeTeamAbbreviation,
                              AwayTeamAbbreviation,
                              Home,
                              Goals, 
                              ValueDifference, 
                              ForeignDifference, 
                              MeanAgeDifference, 
                              WeekNumber, 
                              PointsDifference,
                              RedCardDifference)
}

holdout <- munge_frame(holdout)
train <- munge_frame(train)
test<- munge_frame(test)

# Machine learning
# ================

# Baseline
# --------
# Use mean scores to calculate baseline RMSEs I need to improve upon.
RMSE_baseline_onesplit <- RMSE(test$Goals, mean(train$Goals))
print(sprintf("Baseline RMSE %s", RMSE_baseline_onesplit))

# Generalized linear model
# ------------------------
fit_glm <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                   ValueDifference + ForeignDifference + MeanAgeDifference + 
                   RedCardDifference, 
                 method = "glm",
                 data = train,
                 trControl = trainControl(method = "cv", number = 10, p = 0.9),
                 metric='RMSE',
                 maximize=FALSE)
predict_glm <- predict(fit_glm, newdata=test)  
RMSE_glm_onesplit <- RMSE(test$Goals, predict_glm)
print(sprintf("glm %s", RMSE_glm_onesplit))

# glmnet
# ------
lambdas <- seq(0, 0.2, by=0.001)
fit_glmnet <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                      ValueDifference + ForeignDifference + MeanAgeDifference + 
                      RedCardDifference, 
              method = "glmnet",
              data = train,
              metric='RMSE',
              maximize=FALSE,
              trControl = trainControl(method = "cv", number = 10, p = 0.9),
              tuneGrid = expand.grid(alpha = 0:1, 
                                     lambda = lambdas))
print(fit_glmnet$finalModel$lambdaOpt)
predict_glmnet <- predict(fit_glmnet, newdata=test)  
RMSE_glmnet_onesplit <- RMSE(test$Goals, predict_glmnet)
print(sprintf("glmnet %s", RMSE_glmnet_onesplit))

# Random Forest
# -------------
rf_model <- function(ntree) {
  fit_rf <- train(Goals ~ Home + WeekNumber + PointsDifference + ValueDifference + 
                  ForeignDifference + MeanAgeDifference + RedCardDifference, 
                method = "rf",
                data = train,
                metric='RMSE',
                maximize=FALSE,
                trControl = trainControl(method = "cv", number = 10, p = 0.9),
                tuneGrid = data.frame(mtry = seq(10)),
                ntree=ntree)
  print(fit_rf$bestTune)
  predict_rf <- predict(fit_rf, newdata=test)  
  RMSE_rf <- RMSE(test$Goals, predict_rf)
  print(sprintf("ntree = %f, best tune %f, RMSE = %f", ntree, 
                fit_rf$bestTune, RMSE_rf))
  RMSE_rf
}
ntrees <- sort(c(seq(from=10, to=200, by=10),
                 seq(31, 39),
                 seq(41, 49),
                 seq(51, 59),
                 seq(81, 99),
                 seq(91, 99)))

RMSE_rf_trees <- sapply(ntrees, rf_model)
rf_results <- data.frame(ntrees = ntrees, 
                         rmse = RMSE_rf_trees) 
# Report the minimum RMSE
RMSE_rf_onesplit <- min(rf_results$rmse)
# Build a chart of RMSE vs. ntrees
rf_plot_onesplit <- rf_results %>% ggplot(aes(x=ntrees, y=rmse)) +
  geom_point(color='blue') +
  ggtitle("Random forest for One Split model. rmse vs. ntrees.") +
  ylab("rmse") +
  xlab("ntrees")
print(rf_plot_onesplit)
                         
# SVM
# ===
fit_svm <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                      ValueDifference + ForeignDifference + MeanAgeDifference + 
                      RedCardDifference, 
                 method = "svmLinear",
                 data = train,
                 metric='RMSE',
                 maximize=FALSE,
                 trControl = trainControl(method = "cv", number = 10, p = 0.9),
                 tuneGrid = expand.grid(C = seq(from=0.1, to=2, length=19)),
                 preProcess = c("center","scale"))
predict_svm <- predict(fit_svm, newdata=test)  
RMSE_svm_onesplit <- RMSE(test$Goals, predict_svm)
svm_plot_onesplit <- fit_svm$results %>% ggplot(aes(x=C, y=RMSE)) +
  geom_point(color='blue') +
  ggtitle("SVM for One Split model. rmse vs. C") +
  ylab("RMSE") +
  xlab("C")
print(svm_plot_onesplit)
print(sprintf("svm %s", RMSE_svm_onesplit))

# Tidying up
# ==========

modeling_folder = 'model'
if (!dir.exists(modeling_folder)) {dir.create(modeling_folder)}
save(RMSE_baseline_onesplit,
     RMSE_glm_onesplit,
     RMSE_glmnet_onesplit,
     RMSE_rf_onesplit,
     RMSE_svm_onesplit,
     rf_plot_onesplit,
     svm_plot_onesplit,
     file=file.path(modeling_folder, "model-onesplit.rda"))

# Script duration
script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
script_duration <- sprintf('%d minutes %.1f seconds', 
                           script_duration%/%60, 
                           script_duration%%60)
print(paste("Script duration was", script_duration))
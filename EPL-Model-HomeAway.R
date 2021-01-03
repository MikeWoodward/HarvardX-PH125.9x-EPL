# Harvard PH128.9x Capstone project: English Premiership prediction
# =================================================================

# Machine learning model  -  home and away model.

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
# Figures out the start of the season, creates a notional start date that starts the season from week 1
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
# Add the points.
# 3 for a win, 1 for a draw, 0 for a loss
temp <- match_results %>% 
  mutate(HomeTeamPoints=3*(FTR=='H') + (FTR=='D'),
         AwayTeamPoints=3*(FTR=='A') + (FTR=='D'))

# Work out points for home and away team
home_points <- temp %>% 
  select(Season, Date, HomeTeamAbbreviation, HomeTeamPoints) %>%
  rename(c("TeamPoints"="HomeTeamPoints",
           "TeamAbbreviation"="HomeTeamAbbreviation"))
away_points <- temp %>% 
  select(Season, Date, AwayTeamAbbreviation, AwayTeamPoints) %>%
  rename(c("TeamPoints"="AwayTeamPoints",
           "TeamAbbreviation"="AwayTeamAbbreviation"))
# Now work out mean cumulative points. The points a team has on the day of the match are the points the team have prior to the match. This is why we have cumsum(X) - X. Because we're averaging over all matches prior to the current match, we need the number of rows - 1.
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

# Join the points back to the match data
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
# Work out the red cards for the home and away team
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

# We want the average number of red cards prior to the match we want to model. The is cumsum(X) - X, but because we want the mean for all matches prior to the current one, we divide by the number of rows - 1.
discipline <- rbind(home_discipline, away_discipline) %>%
  arrange(Season, Date) %>%
  group_by(Season, TeamAbbreviation) %>%
  mutate(r=rowid(TeamAbbreviation),
         MPCR=ifelse(r>1, (cumsum(R) - R)/(r-1), 0)) %>%
  arrange(Season, TeamAbbreviation, Date) %>%
  select(Season, Date, TeamAbbreviation, MPCR)
head(discipline)

# Add the points to match_results
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
# Holdout data is 10% of data
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
# Train data is 90%, test data is 10%.
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

# Home and away data sets
# -----------------------
# We've now split the data into three groups: holdout, test, and train. We need 
# to munge the data in a form suitable for modeling - in this case, home and
# away models.

munge_frame <- function(matches_, at_home_) {

  if (at_home_)
  {
      munged <- matches_ %>%
        mutate(Goals=FTHG,
               ValueDifference=HomeTeamValue-AwayTeamValue,
               ForeignDifference=HomeTeamForeignPlayers-AwayTeamForeignPlayers,
               MeanAgeDifference=HomeTeamMeanAge-AwayTeamMeanAge,
               PointsDifference=HomePriorMeanCumPoints-AwayPriorMeanCumPoints,
               RedCardDifference=HomeMPCR-AwayMPCR)
  } else {
      munged <- matches_ %>%
        mutate(Goals=FTAG,
               ValueDifference=-HomeTeamValue+AwayTeamValue,
               ForeignDifference=-HomeTeamForeignPlayers+AwayTeamForeignPlayers,
               MeanAgeDifference=-HomeTeamMeanAge+AwayTeamMeanAge,
               PointsDifference=-HomePriorMeanCumPoints+AwayPriorMeanCumPoints,
               RedCardDifference=-HomeMPCR+AwayMPCR)
  }
  
  munged <- munged %>% select(Season, 
                              Date,
                              HomeTeamAbbreviation,
                              AwayTeamAbbreviation,
                              Goals, 
                              ValueDifference, 
                              ForeignDifference, 
                              MeanAgeDifference, 
                              WeekNumber, 
                              PointsDifference,
                              RedCardDifference)
}

holdout_home <- munge_frame(holdout, TRUE)
holdout_away <- munge_frame(holdout, FALSE)

train_home <- munge_frame(train, TRUE)
train_away <- munge_frame(train, FALSE)

test_home <- munge_frame(test, TRUE)
test_away <- munge_frame(test, FALSE)

# Machine learning
# ================

# Baseline
# --------
# Use mean scores to calculate baseline RMSEs I need to improve upon.
RMSE_baseline_home <- RMSE(test_home$Goals, mean(train_home$Goals))
RMSE_baseline_away <- RMSE(test_away$Goals, mean(train_away$Goals))

print(sprintf("Baseline home RMSE %s", RMSE_baseline_home))
print(sprintf("Baseline away RMSE %s", RMSE_baseline_away))

# Generalized linear model
# ------------------------
# Run once for home and once for away.

# Home
fit_glm_home <- train(Goals ~ WeekNumber + PointsDifference +
                              ValueDifference + ForeignDifference + MeanAgeDifference +
                              RedCardDifference,
                      method = "glm",
                      data = train_home,
                      trControl = trainControl(method = "cv", number = 10, p = 0.9),
                      metric='RMSE',
                      maximize=FALSE)
predict_glm_home <- predict(fit_glm_home, newdata=test_home)
RMSE_glm_home <- RMSE(test_home$Goals, predict_glm_home)
print(sprintf("glm home RMSE %s", RMSE_glm_home))

# Away
fit_glm_away <- train(Goals ~ WeekNumber + PointsDifference +
                              ValueDifference + ForeignDifference + MeanAgeDifference +
                              RedCardDifference,
                      method = "glm",
                      data = train_away,
                      trControl = trainControl(method = "cv", number = 10, p = 0.9),
                      metric='RMSE',
                      maximize=FALSE)
predict_glm_away <- predict(fit_glm_away, newdata=test_away)
RMSE_glm_away <- RMSE(test_away$Goals, predict_glm_away)
print(sprintf("glm away RMSE %s", RMSE_glm_away))

# glmnet
# ------
lambdas <- seq(0, 0.2, by=0.001)

# Run once for home and once for away

# Home
fit_glmnet_home <- train(Goals ~ WeekNumber + PointsDifference +
                                 ValueDifference + ForeignDifference + MeanAgeDifference +
                                 RedCardDifference,
                         method = "glmnet",
                         data = train_home,
                         metric='RMSE',
                         maximize=FALSE,
                         trControl = trainControl(method = "cv", number = 10, p = 0.9),
                         tuneGrid = expand.grid(alpha = 0:1,
                                                lambda = lambdas))
print(fit_glmnet_home$finalModel$lambdaOpt)
predict_glmnet_home <- predict(fit_glmnet_home, newdata=test_home)
RMSE_glmnet_home <- RMSE(test_home$Goals, predict_glmnet_home)
print(sprintf("glmnet home %s", RMSE_glmnet_home))

# Away
fit_glmnet_away <- train(Goals ~ WeekNumber + PointsDifference +
                                 ValueDifference + ForeignDifference + MeanAgeDifference +
                                 RedCardDifference,
                         method = "glmnet",
                         data = train_away,
                         metric='RMSE',
                         maximize=FALSE,
                         trControl = trainControl(method = "cv", number = 10, p = 0.9),
                         tuneGrid = expand.grid(alpha = 0:1,
                                                lambda = lambdas))
print(fit_glmnet_away$finalModel$lambdaOpt)
predict_glmnet_away <- predict(fit_glmnet_away, newdata=test_away)
RMSE_glmnet_away <- RMSE(test_away$Goals, predict_glmnet_away)
print(sprintf("glmnet away %s", RMSE_glmnet_away))

# Random Forest
# -------------
# The RF model work is more complex. I'm calling the model multiple times to calculate an optimal number of trees (ntree). Because of this, I've put the model in a function. Note the arguments to teh function are the number of trees and the data set to use.
rf_model <- function(dataset, ntree) {
  fit_rf <- train(Goals ~ WeekNumber + PointsDifference + ValueDifference +
                  ForeignDifference + MeanAgeDifference + RedCardDifference, 
                method = "rf",
                data = dataset,
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
# An attempt to find the optimal number of trees
ntrees <- sort(seq(from=10, to=200, by=10) +
               seq(41, 49) +
               seq(51, 59) +
               seq(81, 99) +
               seq(91, 99))
ntrees <- seq(from=10, to=20, by=10)

# Home analysis
# Call the rf_model, with the correct dataset
RMSE_rf_trees_home <- sapply(ntrees, function(ntree) {
    rf_model(train_home, ntree)
})
rf_results_home <- data.frame(ntrees = ntrees,
                              rmse = RMSE_rf_trees_home)
# Report the minimum RMSE
RMSE_rf_home <- min(rf_results_home$rmse)
print(sprintf("RF home %s", RMSE_rf_home))
# Build a chart of RMSE vs. ntrees
rf_plot_home <- rf_results_home %>% ggplot(aes(x=ntrees, y=rmse)) +
  geom_point(color='blue') +
  ggtitle("Random forest for Home & Away model - Home branch. rmse vs. ntrees.") +
  ylab("rmse") +
  xlab("ntrees")
print(rf_plot_home)

# Away analysis
RMSE_rf_trees_away <- sapply(ntrees, function(ntree) {
    rf_model(train_away, ntree)
})
rf_results_away <- data.frame(ntrees = ntrees,
                              rmse = RMSE_rf_trees_away)
# Report the minimum RMSE
RMSE_rf_away <- min(rf_results_away$rmse)
print(sprintf("RF home %s", RMSE_rf_away))
# Build a chart of RMSE vs. ntrees
rf_plot_away <- rf_results_away %>% ggplot(aes(x=ntrees, y=rmse)) +
  geom_point(color='blue') +
  ggtitle("Random forest for Home & Away model - Away branch. rmse vs. ntrees.") +
  ylab("rmse") +
  xlab("ntrees")
print(rf_plot_away)
                         
# SVM
# ---
# Presented as two sepoerate calls rather than as a fucntion. This is for simplicity.
fit_svm_home <- train(Goals ~ WeekNumber + PointsDifference +
                              ValueDifference + ForeignDifference + MeanAgeDifference +
                              RedCardDifference,
                      method = "svmLinear",
                      data = train_home,
                      metric='RMSE',
                      maximize=FALSE,
                      trControl = trainControl(method = "cv", number = 10, p = 0.9),
                      tuneGrid = expand.grid(C = seq(from=1, to=2, length=2)),
                      preProcess = c("center","scale"))
predict_svm_home <- predict(fit_svm_home, newdata=test_home)
RMSE_svm_home <- RMSE(test_home$Goals, predict_svm_home)
svm_plot_home <- fit_svm_home$results %>% ggplot(aes(x=C, y=RMSE)) +
  geom_point(color='blue') +
  ggtitle("SVM for One Split model. rmse vs. C") +
  ylab("RMSE") +
  xlab("C")
print(svm_plot_home)
print(sprintf("svm %s", RMSE_svm_home))

fit_svm_away <- train(Goals ~ WeekNumber + PointsDifference +
                              ValueDifference + ForeignDifference + MeanAgeDifference +
                              RedCardDifference,
                      method = "svmLinear",
                      data = train_away,
                      metric='RMSE',
                      maximize=FALSE,
                      trControl = trainControl(method = "cv", number = 10, p = 0.9),
                      tuneGrid = expand.grid(C = seq(from=1, to=2, length=2)),
                      preProcess = c("center","scale"))
predict_svm_away <- predict(fit_svm_away, newdata=test_home)
RMSE_svm_away <- RMSE(test_away$Goals, predict_svm_away)
svm_plot_away <- fit_svm_away$results %>% ggplot(aes(x=C, y=RMSE)) +
  geom_point(color='blue') +
  ggtitle("SVM for One Split model. rmse vs. C") +
  ylab("RMSE") +
  xlab("C")
print(svm_plot_away)
print(sprintf("svm %s", RMSE_svm_away))

# Tidying up
# ==========

modeling_folder = 'model'
if (!dir.exists(modeling_folder)) {dir.create(modeling_folder)}
save(RMSE_baseline_home,
     RMSE_baseline_away,
     RMSE_glm_home,
     RMSE_glm_away,
     RMSE_glmnet_home,
     RMSE_glmnet_away,
     RMSE_rf_home,
     RMSE_rf_away,
     RMSE_svm_home,
     RMSE_svm_away,
     rf_plot_home,
     rf_plot_away,
     svm_plot_home,
     svm_plot_away,
     file=file.path(modeling_folder, "model-homeaway.rda"))

# Script duration
script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
script_duration <- sprintf('%d minutes %.1f seconds', 
                           script_duration%/%60, 
                           script_duration%%60)
print(paste("Script duration was", script_duration))

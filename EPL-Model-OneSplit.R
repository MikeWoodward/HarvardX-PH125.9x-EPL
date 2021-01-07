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
# Add the season week number field. Seasons start mid year and go to the next 
# year. To get a season week number, I work out an offset that moves the start
# of the season to the start of the year.
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
# Three points for a win, 1 for a draw, 0 for  loss
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

# Work out the sum total of points per team, *prior* to the current match.
# We want the average over the prior matches. That's why we have 
# cumsum(TeamPoints)-TeamPoints)/(r-1) where r is the curent number of matches
# and r - 1 is the number of matches before the current match.
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

# Merge the points data back in to match_results
match_results <- match_results %>% 
  left_join(points,
            by=c("Season", "Date", 
                 "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("HomePriorMeanCumPoints"="PriorMeanCumPoints"))  %>% 
  left_join(points,
            by=c("Season", "Date", 
                 "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("AwayPriorMeanCumPoints"="PriorMeanCumPoints"))


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
head(discipline)

# Add the red card data back to match_results
match_results <- match_results %>% 
  left_join(discipline,
            by=c("Season", "Date", 
                 "HomeTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("HomeMPCR"="MPCR", "HomeMPCY"="MPCY"))  %>% 
  left_join(discipline,
            by=c("Season", "Date", 
                 "AwayTeamAbbreviation"="TeamAbbreviation")) %>%
  rename(c("AwayMPCR"="MPCR", "AwayMPCY"="MPCY"))

# Function to calculate RMSE. 
# ==========================
RMSE <- function(actual_goals, forecast_goals){
  sqrt(mean((actual_goals - forecast_goals)^2))
}

# Test, train and validation
# ==========================

set.seed(72, sample.kind="Rounding") 

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
# Note the test and train set is only used for optimizing the random forest.

# Train data is 90%, test data is 10%. p calculation ensures no data left out
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
train <- rbind(train, removed)

# One model, split games data sets
# --------------------------------
# We've now split the data into three groups: holdout, test, and train. We need 
# to munge the data in a form suitable for modeling.

munge_frame <- function(matches_) {

  # Home games - add a home flag
  munged_home <- matches_ %>% 
    mutate(Home=1,
           Goals=FTHG,
           ValueDifference=HomeTeamValue-AwayTeamValue,
           ForeignDifference=HomeTeamForeignPlayers-AwayTeamForeignPlayers,
           MeanAgeDifference=HomeTeamMeanAge-AwayTeamMeanAge,
           PointsDifference=HomePriorMeanCumPoints-AwayPriorMeanCumPoints,
           RedCardDifference=HomeMPCR-AwayMPCR,
           YellowCardDifference=HomeMPCY-AwayMPCY)

  munged_away <- matches_ %>% 
    mutate(Home=0,
           Goals=FTAG,
           ValueDifference=-HomeTeamValue+AwayTeamValue,
           ForeignDifference=-HomeTeamForeignPlayers+AwayTeamForeignPlayers,
           MeanAgeDifference=-HomeTeamMeanAge+AwayTeamMeanAge,
           PointsDifference=-HomePriorMeanCumPoints+AwayPriorMeanCumPoints,
           RedCardDifference=-HomeMPCR+AwayMPCR,
           YellowCardDifference=-HomeMPCY+AwayMPCY)   
  
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
                              RedCardDifference,
                              YellowCardDifference)
}

# Put the data in a form suitable for modeling
holdout <- munge_frame(holdout)

epl <- munge_frame(epl)
train <- munge_frame(train)
test<- munge_frame(test)

# Machine learning
# ================

# Baseline
# --------
# Use mean scores to calculate baseline RMSEs I need to improve upon. 
RMSE_baseline_onesplit <- RMSE(holdout$Goals, mean(epl$Goals))
print(sprintf("Baseline RMSE %s", RMSE_baseline_onesplit))

# Generalized linear model
# ------------------------
fit_glm <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                         ValueDifference + ForeignDifference + 
                         MeanAgeDifference + RedCardDifference +
                         YellowCardDifference, 
                 method = "glm",
                 data = epl,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10, 
                                          repeats = 3, 
                                          p = 0.9),
                 metric='RMSE',
                 maximize=FALSE)
predict_glm <- predict(fit_glm, newdata=holdout)  
RMSE_glm_onesplit <- RMSE(holdout$Goals, predict_glm)
print(sprintf("glm %s", RMSE_glm_onesplit))

# glmnet
# ------
lambdas <- seq(0, 0.4, by=0.01)
fit_glmnet <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                            ValueDifference + ForeignDifference + 
                            MeanAgeDifference + RedCardDifference + 
                            YellowCardDifference, 
              method = "glmnet",
              data = epl,
              metric='RMSE',
              maximize=FALSE,
              trControl = trainControl(method = "repeatedcv", 
                                       number = 10, 
                                       repeats = 3, 
                                       p = 0.9),
              tuneGrid = expand.grid(alpha = 0:1, 
                                     lambda = lambdas))
print(fit_glmnet$finalModel$lambdaOpt)
predict_glmnet <- predict(fit_glmnet, newdata=holdout)  
RMSE_glmnet_onesplit <- RMSE(holdout$Goals, predict_glmnet)
print(sprintf("glmnet %s", RMSE_glmnet_onesplit))

# SVM
# ---
fit_svm <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                         ValueDifference + ForeignDifference + 
                         MeanAgeDifference + RedCardDifference + 
                         YellowCardDifference, 
                 method = "svmLinear",
                 data = epl,
                 metric='RMSE',
                 maximize=FALSE,
                 trControl = trainControl(method = "repeatedcv", 
                                          number = 10, 
                                          repeats = 3, 
                                          p = 0.9),
                 tuneGrid = expand.grid(C = seq(from=0.5, to=10, by=0.5)),
                 preProcess = c("center","scale"))
predict_svm <- predict(fit_svm, newdata=holdout)  
RMSE_svm_onesplit <- RMSE(holdout$Goals, predict_svm)
svm_plot_onesplit <- fit_svm$results %>% ggplot(aes(x=C, y=RMSE)) +
  geom_point(color='blue') +
  ggtitle("SVM for One Split model. rmse vs. C") +
  ylab("RMSE") +
  xlab("C")
print(svm_plot_onesplit)
print(sprintf("svm %s", RMSE_svm_onesplit))

# Random Forest - calculate ntree etc.
# ------------------------------------
# The first step is to calculate the number of trees. This is where we'll
# use the train and holdout sets.
rf_model <- function(ntree) {
  print(sprintf("In rf_model function, ntrees=%d", ntree))
  fit_rf <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                          ValueDifference + ForeignDifference + 
                          MeanAgeDifference + RedCardDifference + 
                          YellowCardDifference, 
                  method = "rf",
                  data = train,
                  metric='RMSE',
                  maximize=FALSE,
                  trControl = trainControl(method = "repeatedcv", 
                                           number = 10, 
                                           repeats = 3, 
                                           p = 0.9),
                  tuneGrid = expand.grid(.mtry=seq(1:7)),
                  ntree=ntree)
  
  predict_rf <- predict(fit_rf, newdata=test)  
  RMSE_rf <- RMSE(test$Goals, predict_rf)
  
  c(ntree, varImp(fit_rf), fit_rf$bestTune$mtry, RMSE_rf)
}

# Here are the number of trees to try, I found them by hand experimentation.
ntrees <- sort(c(seq(from=1, to=200, by=1),
                 seq(from=210, to=250, by=10)))

# Get the RMSE for each tree value
rf_data <- sapply(ntrees, rf_model)

# Build a dataframe containing the results - note the use of transpose to 
# correctly orient the frame
rf_results <- data.frame(t(rf_data))
names(rf_results) <- c("ntrees", "varImp", "dummy", "dummy2", "mtry", "rmse")
rf_results$ntrees <- as.numeric(rf_results$ntrees)
rf_results$rmse <- as.numeric(rf_results$rmse)
rf_results$mtry <- as.numeric(rf_results$mtry)
# Report the minimum RMSE and the associated mtry and ntrees
idx <- which.min(rf_results$rmse)
RMSE_rf_onesplit <- rf_results[idx, 'rmse']
mtry_rf_onesplit <- rf_results[idx, 'mtry']
ntrees_rf_onesplit <- rf_results[idx, 'ntrees']
varImp_rf_onesplit <- rf_results[idx, 'varImp'][[1]]
print('Random forest - model building')
print(sprintf("RF RMSE %f", RMSE_rf_onesplit))
print(sprintf("RF mtry %d", mtry_rf_onesplit))
print(sprintf("RF ntree %d", ntrees_rf_onesplit))
print('Optimal Variable importance')
print(varImp_rf_onesplit)

# Build a chart of RMSE vs. ntrees
rf_plot_onesplit <- rf_results %>% ggplot(aes(x=ntrees, y=rmse)) +
  geom_point(color='blue') +
  ggtitle("Random forest for One Split model. rmse vs. ntrees.") +
  ylab("rmse") +
  xlab("ntrees")
print(rf_plot_onesplit)

# We now know the value of the ntree setting - we can calculate the final
# random forest model

# Random Forest - final model
# ---------------------------
fit_rf <- train(Goals ~ Home + WeekNumber + PointsDifference + 
                        ValueDifference + ForeignDifference + 
                        MeanAgeDifference + RedCardDifference +
                        YellowCardDifference, 
                method = "rf",
                data = epl,
                metric='RMSE',
                maximize=FALSE,
                trControl = trainControl(method = "repeatedcv", 
                                         number = 10, 
                                         repeats = 3, 
                                         p = 0.9),
                tuneGrid = expand.grid(.mtry=seq(1:7)),
                ntree=ntrees_rf_onesplit)

predict_rf <- predict(fit_rf, newdata=holdout)  
RMSE_rf_final_onesplit <- RMSE(holdout$Goals, predict_rf)
print(sprintf("RF RMSE %f", RMSE_rf_final_onesplit))

# Tidying up
# ==========

modeling_folder = 'model'
if (!dir.exists(modeling_folder)) {dir.create(modeling_folder)}

# Save the variables to disk - we'll read them in in the report file.
save(RMSE_baseline_onesplit,
     RMSE_glm_onesplit,
     RMSE_glmnet_onesplit,
     RMSE_rf_onesplit,
     RMSE_svm_onesplit,
     rf_plot_onesplit,
     svm_plot_onesplit,
     mtry_rf_onesplit,
     ntrees_rf_onesplit,
     varImp_rf_onesplit,
     RMSE_rf_final_onesplit,
     file=file.path(modeling_folder, "model-onesplit.rda"))

# Script duration
script_duration <- as.numeric((proc.time() - ptm)['elapsed'])
script_duration <- sprintf('%d minutes %.1f seconds', 
                           script_duration%/%60, 
                           script_duration%%60)
print(paste("Script duration was", script_duration))

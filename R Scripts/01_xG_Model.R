# Packages
library(tidyverse)
library(BART)
library(xgboost)
library(pROC)
library(gt)

# Data
scouting <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")

# Functions
euc.dist <- function(x1, x2){ 
  sqrt(sum((x1 - x2) ^ 2))
}
angle <- function(p1,p2,p3){
  val = acos((euc.dist(p2,p3)^2 - euc.dist(p1,p3)^2 + euc.dist(p1,p2)^2)/(2*euc.dist(p2,p3)*euc.dist(p1,p2)))*180/pi
  return(ifelse(val > 90,180 - val,val))
}

# Add a play index
scouting$index <- 1:nrow(scouting)

# Change Clock to secs remaining
scouting$secs_remaining <- sapply(scouting$Clock,function(x){as.numeric(strsplit(x,split=":")[[1]][1])*60+ as.numeric(strsplit(x,split=":")[[1]][2])})

# filter on shots / goals
xg_data <- filter(scouting,Event %in% c("Shot","Goal")) %>%
  mutate(strength_state= case_when(abs(Home.Team.Skaters - Away.Team.Skaters) == 0~"0",
                                ((Home.Team.Skaters - Away.Team.Skaters) == -1 & Team == Home.Team) | 
                                  ((Home.Team.Skaters - Away.Team.Skaters) == 1 & Team == Away.Team) ~"-1",
                                ((Home.Team.Skaters - Away.Team.Skaters) == 1 & Team == Home.Team) | 
                                  ((Home.Team.Skaters - Away.Team.Skaters) == -1 & Team == Away.Team) ~"1",
                                ((Home.Team.Skaters - Away.Team.Skaters) == -2 & Team == Home.Team) | 
                                  ((Home.Team.Skaters - Away.Team.Skaters) == 2 & Team == Away.Team) ~"-2",
                                ((Home.Team.Skaters - Away.Team.Skaters) == 2 & Team == Home.Team) | 
                                  ((Home.Team.Skaters - Away.Team.Skaters) == -2 & Team == Away.Team)~"2"),
         game_state = ifelse(Home.Team == Team,as.character(Home.Team.Goals-Away.Team.Goals),ifelse(Away.Team == Team,as.character(Away.Team.Goals-Home.Team.Goals),NA)),
         traffic = ifelse(Detail.3=="t","1","0"),
         one_timer = ifelse(Detail.4=="t","1","0"),
         home = ifelse(Home.Team == Team,"1","0"),
         goal = ifelse(Event == "Goal","1","0"),
         shot_distance = sqrt((X.Coordinate - 189)^2 + (Y.Coordinate - 42.5)^2)) %>%
  rename(shot_type = Detail.1)

# Shot Angle
xg_data$shot_angle <- apply(xg_data[,c("X.Coordinate","Y.Coordinate")],1,function(x){angle(c(x),c(189,42.5),p3 = c(189,0))})

# Get previous event and location
xg_data <- xg_data %>%
  left_join(scouting %>% 
  mutate(index = as.character(index)) %>% 
  filter(index %in% as.character(xg_data$index-1)) %>%
  rename(prev_event = Event,
         prev_event_team = Team,
         prev_x_coordinate = X.Coordinate,
         prev_y_coordinate = Y.Coordinate,
         prev_secs_remaining = secs_remaining) %>%
    mutate(index = as.numeric(index)+1) %>%
  dplyr::select(index,prev_event,prev_event_team,prev_x_coordinate,prev_y_coordinate,prev_secs_remaining),by = c("index")) %>%
  mutate(diff_in_time_prev_event = prev_secs_remaining - secs_remaining) %>%
  dplyr::select(goal,Period,secs_remaining,X.Coordinate,Y.Coordinate,shot_type,strength_state,game_state,
         traffic,one_timer,home,prev_event,prev_x_coordinate,prev_y_coordinate,diff_in_time_prev_event,
         shot_distance,shot_angle)

# Check data
#summary(xg_data)

# Scale numeric variables -- create temp df 
xg_data_ <- xg_data
xg_data[,c(3:5,13:17)] <- apply(xg_data[,c(3:5,13:17)],2,scale)

# XGBOOST Hypertuning
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0
start = Sys.time()
for (iter in 1:5) {
  param <- list(objective = "binary:logistic",
                eval.metric = "logloss",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data = model.matrix(goal ~ .,data = xg_data),
                 label = as.vector(xg_data$goal), 
                 params = param, 
                 nthread=6, 
                 nfold=cv.nfold, 
                 nrounds=cv.nround,
                 verbose = F, 
                 #early.stop.round=10, 
                 maximize=FALSE)
  
  min_logloss = min(mdcv$evaluation_log[, "test_logloss_mean"])
  min_logloss_index = which.min(mdcv$evaluation_log[, test_logloss_mean])
  
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
  
  print(iter)
  
}

best_param <- list(objective = "binary:logistic",
              eval.metric = "logloss",
              max_depth = 8,
              eta = 0.2552009,
              gamma = 0.06985038, 
              subsample = 0.7135821,
              colsample_bytree = 0.6079713, 
              min_child_weight = 9,
              max_delta_step = 6
)

best_logloss #0.1808734

# Create design matrix
mod_boost <- xgboost(data = model.matrix(goal ~ .,data = xg_data),
                     label = as.vector(xg_data$goal),
                     params = best_param, 
                     nthread=6,
                     nround = 1000,
                     verbose = F, 
                     maximize=FALSE)

min(mod_boost$evaluation_log$train_logloss)
#pred <- predict(mod_boost,newdata = model.matrix(goal ~ .,data = xg_data))

LogLoss = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  return(- (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual))
}
# Log Loss of mean prediction 
LogLoss(actual = as.numeric(xg_data$goal),predicted = rep(mean(as.numeric(xg_data$goal)),nrow(xg_data))) # 0.2174058
LogLoss(actual = as.numeric(xg_data$goal),predicted = pred) # 0.02181658

# Quick peak at a feature importance
importance_matrix <- xgb.importance(feature_names = colnames(model.matrix(goal ~ .,data = xg_data)), model = mod_boost)

##### 
# Train 75%, Test 25%
#####

xg_data <- xg_data %>%
  mutate(strength_state = ifelse(strength_state=="-2","-1",strength_state))

mod_results <- data.frame(naive_auc = rep(0,10),naive_logloss = rep(0,10),
           xgboost_auc = rep(0,10),xgboost_logloss = rep(0,10),
           bart_auc = rep(0,10),bart_logloss = rep(0,10),
           logistic_auc = rep(0,10),logistic_logloss = rep(0,10))

# WARNING -- ~3 hour runtime
for (i in 1:10){

set.seed(i*10)
indices <- sample(1:nrow(xg_data),size = nrow(xg_data),replace = F)
xg_data_train <- xg_data[indices[1:(nrow(xg_data)*.75)],]
xg_data_test <- xg_data[indices[((nrow(xg_data)*.75)+1):nrow(xg_data)],]


## Naive Model
mod_results[i,"naive_auc"] <- auc(as.numeric(xg_data_test$goal), rep(mean(as.numeric(xg_data_train$goal)),nrow(xg_data_test)))
mod_results[i,"naive_logloss"] <- LogLoss(actual = as.numeric(xg_data_test$goal),predicted = mean(as.numeric(xg_data_train$goal)))

## XGBOOST
mod_boost <- xgboost(data = model.matrix(goal ~ .,data = xg_data_train),
                     label = as.vector(xg_data_train$goal),
                     params = best_param, 
                     nthread=6,
                     nround = 1000,
                     verbose = F, 
                     maximize=FALSE)

pred <- predict(mod_boost,newdata = model.matrix(goal ~ .,data = xg_data_test))

mod_results[i,"xgboost_auc"] <- auc(as.numeric(xg_data_test$goal), pred) # 0.783
mod_results[i,"xgboost_logloss"] <- LogLoss(actual = as.numeric(xg_data_test$goal),predicted = pred) #  0.1848207


## BART
x.train <- model.matrix(goal~.,data = xg_data_train %>% mutate(Period = as.character(Period)))[,-1]
x.test <- model.matrix(goal~.,data = xg_data_test %>% mutate(Period = as.character(Period)))[,-1]

y.train = xg_data_train %>% mutate(goal = as.numeric(goal)) %>% dplyr::select(goal) %>% as.matrix()
y.test = xg_data_test %>% mutate(goal = as.numeric(goal)) %>% dplyr::select(goal) %>% as.matrix()

# Fit BART 
bart_fit1 <- lbart(x.train = x.train, y.train = y.train, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)

probs <- predict(bart_fit1, newdata = x.test)$prob.test

probs_median <- apply(probs,2,median)

mod_results[i,"bart_auc"] <- auc(as.numeric(xg_data_test$goal), probs_median) # 0.8445
mod_results[i,"bart_logloss"] <- LogLoss(actual = as.numeric(xg_data_test$goal),predicted = probs_median) # 0.1577399

# Logistic Regression
mod_logistic <- glm(goal~.,data = xg_data_train %>% mutate(goal = as.numeric(goal)))
pred_logistic <- predict(mod_logistic,newdata = xg_data_test)
mod_results[i,"logistic_auc"] <- auc(as.numeric(xg_data_test$goal), pred_logistic) # 0.7912
mod_results[i,"logistic_logloss"] <-LogLoss(actual = as.numeric(xg_data_test$goal),predicted =pred_logistic) # 0.2209055
}

write.csv(mod_results,"/Users/marcrichards/Desktop/Big Data Cup/Output/mod_results_xG_20210226.csv",row.names = F)
apply(mod_results,2,mean)

# Calc mean and sd across 10 train/test splits
mod_results_agg <- apply(mod_results,2,mean)
mod_results_agg_sd <- apply(mod_results,2,sd)

# Make xG table for paper
results_table <- data.frame(model = c("Naive","Logistic","XGBOOST","BART"),
                             auc = c("0.500 (0.000)","0.783 (0.004)","0.791 (0.004)","0.811 (0.023)"),
                             logloss = c("0.215 (0.025)","0.213 (0.042)","0.207 (0.002)","0.178 (0.002)"))

results_table  %>%
  #mutate(auc = round(auc,3),
  #       logloss = round(logloss,3)) %>%
  gt() %>%
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE
  ) %>%
  # tab_source_note(
  #    source_note = "DATA SOURCE: NEXT GEN STATS FOR 2018 NFL SEASON "
  #  ) %>%
  cols_label(
    model = "Model",
    auc = "AUC",
    logloss = "Log-Loss"
  )


## BART
x.train <- model.matrix(goal~.,data = xg_data %>% mutate(Period = as.character(Period)))[,-1]

y.train = xg_data %>% mutate(goal = as.numeric(goal)) %>% dplyr::select(goal) %>% as.matrix()

# Fit BART 
bart_fit1 <- lbart(x.train = x.train, y.train = y.train, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)

probs <- predict(bart_fit1, newdata = x.train)$prob.test

write.csv(probs,"/Users/marcrichards/Desktop/Big Data Cup/Output/predicted_goal_probs_20210226.csv",row.names = F)

probs_median <- apply(probs,2,median)

scouting_goals <- scouting %>%
  filter(Event %in% c("Shot","Goal")) %>%
  mutate(goal_prob = probs_median)

scouting_goals_wvars <- xg_data %>%
  mutate(goal_prob = probs_median)

write.csv(scouting_goals,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xG_20210226.csv",row.names = F)
write.csv(scouting_goals_wvars,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xG_wVars_20210226.csv",row.names = F)



# Packages
library(tidyverse)
library(xgboost)
library(gt)
library(pROC)
library(BART)

# Data
scouting <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")

# Functions
euc.dist <- function(x1, x2){ 
  sqrt(sum((x1 - x2) ^ 2))
}
angle <- function(p1,p2,p3){
  val = acos((euc.dist(p2,p3)^2 - euc.dist(p1,p3)^2 + euc.dist(p1,p2)^2)/(2*euc.dist(p2,p3)*euc.dist(p1,p2)))*180/pi
  return(val)
}

# Add a play index
scouting$index <- 1:nrow(scouting)

# Change Clock to secs remaining
scouting$secs_remaining <- sapply(scouting$Clock,function(x){as.numeric(strsplit(x,split=":")[[1]][1])*60+ as.numeric(strsplit(x,split=":")[[1]][2])})


# filter on shots / goals
xp_data <- filter(scouting,Event %in% c("Play","Incomplete Play")) %>%
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
         direct = ifelse(Detail.1=="Direct","1","0"),
         home = ifelse(Home.Team == Team,"1","0"),
         pass = ifelse(Event == "Play","1","0"),
         pass_distance = sqrt((X.Coordinate - X.Coordinate.2)^2 + (Y.Coordinate - Y.Coordinate.2)^2))

# Pass Angle
xp_data$pass_angle <- apply(xp_data[,c("X.Coordinate","Y.Coordinate","X.Coordinate.2","Y.Coordinate.2")],1,function(x){
  return(ifelse(x[1] > x[3], 360 - angle(c(x[1],0),c(x[1:2]),p3 = c(x[3:4])),angle(c(x[1],0),c(x[1:2]),p3 = c(x[3:4]))))})

xp_data <- xp_data %>%
  mutate(pass_angle = ifelse(is.na(pass_angle),0,pass_angle)) # NAs for when the targeted player has the same X and Y

# Get previous event and location
xp_data <- xp_data %>% 
  left_join(scouting %>% 
              mutate(index = as.character(index)) %>% 
              filter(index %in% as.character(xp_data$index-1)) %>%
              rename(prev_event = Event,
                     prev_event_team = Team,
                     prev_x_coordinate = X.Coordinate,
                     prev_y_coordinate = Y.Coordinate,
                     prev_secs_remaining = secs_remaining) %>%
              mutate(index = as.numeric(index)+1) %>%
              dplyr::select(index,prev_event,prev_event_team,prev_x_coordinate,prev_y_coordinate,prev_secs_remaining),by = c("index")) %>%
  mutate(diff_in_time_prev_event = prev_secs_remaining - secs_remaining) %>%
  dplyr::select(pass,Period,secs_remaining,X.Coordinate,Y.Coordinate,X.Coordinate.2,Y.Coordinate.2,strength_state,game_state,
         direct,home,prev_event,prev_x_coordinate,prev_y_coordinate,diff_in_time_prev_event,pass_distance,pass_angle)

# Check data
#summary(xp_data)

# Scale numeric variables
xp_data_hold <- xp_data
xp_data[,c(3:7,13:17)] <- apply(xp_data[,c(3:7,13:17)],2,scale)

# Change period to a character variable
xp_data <- xp_data %>%
  mutate(Period = as.character(Period))

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
  mdcv <- xgb.cv(data = model.matrix(pass ~ .,data = xp_data),
                 label = as.vector(xp_data$pass), 
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
                   max_depth = 6,
                   eta = 0.03370188,
                   gamma = 0.03847767, 
                   subsample = 0.6274342,
                   colsample_bytree =0.7714596, 
                   min_child_weight = 7,
                   max_delta_step = 2
)

best_logloss # 0.4974338

# Create design matrix
mod_boost <- xgboost(data = model.matrix(pass ~ .,data = xp_data),
                     label = as.vector(xp_data$pass),
                     params = best_param, 
                     nthread=6,
                     nround = 1000,
                     verbose = F, 
                     maximize=FALSE)

#min(mod_boost$evaluation_log$train_logloss)
pred <- predict(mod_boost,newdata = model.matrix(pass ~ .,data = xp_data))

LogLoss = function(actual, predicted, eps = 1e-15) {
  predicted = pmin(pmax(predicted, eps), 1-eps) 
  return(- (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual))
}
# Log Loss of mean prediction
LogLoss(actual = as.numeric(xp_data$pass),predicted = rep(mean(as.numeric(xp_data$pass)),nrow(xp_data))) # 0.5853676
LogLoss(actual = as.numeric(xp_data$pass),predicted = pred) # 0.3828871

# Variable importance
importance_matrix <- xgb.importance(feature_names = colnames(model.matrix(pass ~ .,data = xp_data)), model = mod_boost)


scouting_passes <- filter(scouting,Event %in% c("Play","Incomplete Play")) %>%
  mutate(comp_prob = pred,
         pass = ifelse(Event == "Play",1,0))

write.csv(scouting_passes,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP.csv",row.names = F)

##### 
# Train 75%, Test 25%
#####

xp_data <- xp_data %>%
  mutate(strength_state = ifelse(strength_state=="-2","-1",strength_state))

mod_xp_results <- data.frame(naive_auc = rep(0,10),naive_logloss = rep(0,10),
                          xgboost_auc = rep(0,10),xgboost_logloss = rep(0,10),
                          bart_auc = rep(0,10),bart_logloss = rep(0,10),
                          logistic_auc = rep(0,10),logistic_logloss = rep(0,10))

# WARNING -- ~10 hour runtime
for (i in 1:10){
  
  set.seed(i*10)
  indices <- sample(1:nrow(xp_data),size = nrow(xp_data),replace = F)
  xp_data_train <- xp_data[indices[1:(nrow(xp_data)*.75)],]
  xp_data_test <- xp_data[indices[((nrow(xp_data)*.75)+1):nrow(xp_data)],]
  
  
  ## Naive Model
  mod_xp_results[i,"naive_auc"] <- auc(as.numeric(xp_data_test$pass), rep(mean(as.numeric(xp_data_train$pass)),nrow(xp_data_test)))
  mod_xp_results[i,"naive_logloss"] <- LogLoss(actual = as.numeric(xp_data_test$pass),predicted = mean(as.numeric(xp_data_train$pass)))
  
  ## XGBOOST
  mod_boost <- xgboost(data = model.matrix(pass ~ .,data = xp_data_train),
                       label = as.vector(xp_data_train$pass),
                       params = best_param, 
                       nthread=6,
                       nround = 1000,
                       verbose = F, 
                       maximize=FALSE)
  
  pred <- predict(mod_boost,newdata = model.matrix(pass ~ .,data = xp_data_test))
  
  mod_xp_results[i,"xgboost_auc"] <- auc(as.numeric(xp_data_test$pass), pred) # 0.783
  mod_xp_results[i,"xgboost_logloss"] <- LogLoss(actual = as.numeric(xp_data_test$pass),predicted = pred) #  0.1848207
  
  
  ## BART
  x.train <- model.matrix(pass~.,data = xp_data_train)[,-1]
  x.test <- model.matrix(pass~.,data = xp_data_test)[,-1]
  
  y.train = xp_data_train %>% mutate(pass = as.numeric(pass)) %>% dplyr::select(pass) %>% as.matrix()
  y.test = xp_data_test %>% mutate(pass = as.numeric(pass)) %>% dplyr::select(pass) %>% as.matrix()
  
  # Fit BART 
  bart_fit1 <- lbart(x.train = x.train, y.train = y.train, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)
  
  probs <- predict(bart_fit1, newdata = x.test)$prob.test
  
  probs_median <- apply(probs,2,median)
  
  mod_xp_results[i,"bart_auc"] <- auc(as.numeric(xp_data_test$pass), probs_median) # 0.8445
  mod_xp_results[i,"bart_logloss"] <- LogLoss(actual = as.numeric(xp_data_test$pass),predicted = probs_median) # 0.1577399
  
  # Logistic Regression
  mod_logistic <- glm(pass~.,data = xp_data_train %>% mutate(pass= as.numeric(pass)))
  pred_logistic <- predict(mod_logistic,newdata = xp_data_test)
  mod_xp_results[i,"logistic_auc"] <- auc(as.numeric(xp_data_test$pass), pred_logistic) # 0.7912
  mod_xp_results[i,"logistic_logloss"] <-LogLoss(actual = as.numeric(xp_data_test$pass),predicted =pred_logistic) # 0.2209055
}


write.csv(mod_xp_results,"/Users/marcrichards/Desktop/Big Data Cup/Output/mod_results_xP_20210227.csv",row.names = F)
apply(mod_xp_results,2,mean)

mod_xp_results_agg <- apply(mod_xp_results,2,mean)
mod_xp_results_agg_sd <- apply(mod_xp_results,2,sd)
# results figure for xP Model
results_table2 <- data.frame(model = c("Naive","Logistic","XGBOOST","BART"),
                            auc = c("0.500 (0.000)","0.651 (0.003)","0.752 (0.006)","0.760 (0.004)"),
                            logloss = c("0.584 (0.005)","0.570 (0.008)","0.501 (0.006)","0.497 (0.006)"))

results_table2  %>%
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
x.train <- model.matrix(pass~.,data = xp_data)[,-1]

y.train = xp_data %>% mutate(pass = as.numeric(pass)) %>% dplyr::select(pass) %>% as.matrix()

# Fit BART 
bart_fit1 <- lbart(x.train = x.train, y.train = y.train, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)

probs <- predict(bart_fit1, newdata = x.train)$prob.test

write.csv(probs,"/Users/marcrichards/Desktop/Big Data Cup/Output/predicted_passes_probs_20210227.csv",row.names = F)

probs_median <- apply(probs,2,median)

scouting_passes <- filter(scouting,Event %in% c("Play","Incomplete Play")) %>%
  mutate(comp_prob = probs_median,
         pass = ifelse(Event == "Play",1,0))

scouting_passes_xp <-xp_data_hold %>%
  mutate(comp_prob = probs_median)

write.csv(scouting_passes,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP_20210227.csv",row.names = F)
write.csv(scouting_passes_xp,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP_wVars_20210227.csv",row.names = F)

# Games Played Datasets -- derive from just checking if a player showed up in the event set.
games_played <- scouting %>%
  group_by(game_date,Player,Team) %>%
  summarize(cnt = n()) %>% 
  ungroup() %>%
  group_by(Player,Team) %>%
  summarize(gp = n())
  
# All Situations DPP vs PPM Figure
scoutng_passes_agg <- scouting_passes_org %>%
  mutate(difficult = ifelse(comp_prob <= .3,1,0),
         completed_difficult = ifelse(pass == 1 & difficult == 1,1,0)) %>%
  group_by(Player,Team) %>%
  summarize(cpoe = sum(pass - comp_prob),
            CPOE = ((sum(pass)/n()) - mean(comp_prob))*100,
            comp_perc = sum(pass)/n(),
            xP = mean(comp_prob),
            tough_passes = sum(difficult),
            passes = n(),
            cpoe_per_pass = sum(pass - comp_prob),
            completed_difficult = sum(completed_difficult),
            completed_difficult_perc = sum(completed_difficult)/sum(difficult)) %>%
  ungroup() %>%
  left_join(games_played,by = c("Player","Team")) %>% 
  mutate(cpoe_per_60 = cpoe/gp) %>%
  filter(passes > 75) %>%
  arrange(-completed_difficult)

filter(scoutng_passes_agg,Team == "Erie Otters" &  tough_passes >= 5) %>%
  dplyr::select(cpoe_per_60,completed_difficult_perc) %>%
  cor

ggplot(data = filter(scoutng_passes_agg,Team == "Erie Otters" &  tough_passes >= 5) %>%
         mutate(named_players = ifelse(Player %in% c("Jacob Golden","Hayden Fowler","Jamie Drysdale","Maxim Golod","Brendan Kischnick"),"1","0")),
       aes(x = cpoe_per_60,y = completed_difficult_perc,label = Player)) + 
  geom_text()

ggplot(data = filter(scoutng_passes_agg,Team == "Erie Otters" &  tough_passes >= 5) %>%
         mutate(named_players = ifelse(Player %in% c("Jacob Golden","Luke Beamish","Hayden Fowler","Jamie Drysdale","Maxim Golod","Brendan Kischnick"),"1","0")),
       aes(x = cpoe_per_60,y = completed_difficult_perc,color = named_players,label = Player)) + 
  geom_point(aes(size = passes)) + 
  annotate(geom = "text",x = 0.65,y = 0.42,label = "Jacob Golden") +
  annotate(geom = "text",x = -1.26,y = 0.295,label = "Hayden Fowler") +
  annotate(geom = "text",x = 1.07,y = 0.22,label = "Jamie Drysdale") +
  annotate(geom = "text",x = 1,y = 0.277,label = "Maxim") +
  annotate(geom = "text",x = 1,y = 0.265,label = "Golod") +
  annotate(geom = "text",x = -0.6,y = 0.06,label = "Brendan Kischnick") +
  annotate(geom = "text",x = 1.2,y = 0.4,label = "Luke") +
  annotate(geom = "text",x = 1.2,y = 0.39,label = "Beamish") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(),
        legend.position = "none") + 
  xlab("Passing Plus Minus") +
  ylab("Percent of Difficult Passes") +
  ggtitle("All Situations")


# Even Stregnth DPP vs PPM Figure
scouting_passes_5v5_agg <- scouting_passes_org %>%
  mutate(difficult = ifelse(comp_prob <= .3,1,0),
         completed_difficult = ifelse(pass == 1 & difficult == 1,1,0),
         strength_state = xp_data$strength_state) %>%
  filter(strength_state == 0) %>%
  group_by(Player,Team) %>%
  summarize(cpoe = sum(pass - comp_prob),
            comp_perc = sum(pass)/n(),
            tough_passes = sum(difficult),
            passes = n(),
            cpoe_per_pass = sum(pass - comp_prob),
            completed_difficult = sum(completed_difficult),
            completed_difficult_perc = sum(completed_difficult)/sum(difficult)) %>%
  ungroup() %>%
  left_join(games_played,by = c("Player","Team")) %>% 
  mutate(cpoe_per_60 = cpoe/gp) %>%
  filter(passes > 50) %>%
  arrange(-completed_difficult) 

ggplot(data = filter(scouting_passes_5v5_agg,Team == "Erie Otters" &  tough_passes >= 5) %>%
         mutate(named_players = ifelse(Player %in% c("Jacob Golden","Hayden Fowler","Luke Beamish","Jamie Drysdale","Maxim Golod","Brendan Kischnick"),"1","0")),
       aes(x = cpoe_per_60,y = completed_difficult_perc,color = named_players,label = Player)) + 
  geom_point(aes(size = passes)) + 
  annotate(geom = "text",x = 0.48,y = 0.42,label = "Jacob Golden") +
  annotate(geom = "text",x = -.85,y = 0.253,label = "Hayden Fowler") +
  annotate(geom = "text",x = 0.85,y = 0.241,label = "Jamie Drysdale") +
  annotate(geom = "text",x = 0.44,y = 0.285,label = "Maxim Golod") +
  annotate(geom = "text",x = -0.5,y = 0.065,label = "Brendan Kischnick") +
  annotate(geom = "text",x = .96,y = 0.415,label = "Luke") +
  annotate(geom = "text",x = .96,y = 0.405,label = "Beamish") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(),
        legend.position = "none") + 
  xlab("Passing Plus Minus") +
  ylab("Percent of Difficult Passes") +
  ggtitle("Even Strength Situations")





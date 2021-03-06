# Packages
library(tidyverse)
library(BART)

# Data
scouting_passes_wgoals <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_passes_w_xG.csv")
scouting_passes  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP_wVars_20210227.csv")
scouting_passes_org  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP_20210227.csv")
xg_data  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xG_wVars.csv")
xg_data_off_passes <- xg_data %>% 
  filter(prev_event %in% c("Play","Incomplete Play"))

x.train <- model.matrix(goal~.,data = xg_data %>% mutate(Period = as.character(Period)))[,-1]

y.train = xg_data %>%
  mutate(goal = as.numeric(goal)) %>%
  select(goal) %>%
  as.matrix()

# Fit BART 
bart_fit1 <- lbart(x.train = x.train, y.train = y.train, sparse = TRUE, ndpost = 500, nskip = 2500, keepevery = 5, printevery = 500)

probs <- predict(bart_fit1, newdata = x)$prob.test

probs_median <- apply(probs,2,median)

pb <- txtProgressBar(min = 1, max = nrow(scouting_passes), style = 3)
# empirical data
for (i in 1:nrow(scouting_passes)){

  # Obersved Variables
  xg_test <- data.frame(goal = rep(0,nrow(xg_data_off_passes)),
                        Period = rep(scouting_passes$Period[i],nrow(xg_data_off_passes)),
                        strength_state = rep(scouting_passes$strength_state[i],nrow(xg_data_off_passes)),
                        game_state = rep(scouting_passes$game_state[i],nrow(xg_data_off_passes)),
                        home = rep(scouting_passes$home[i],nrow(xg_data_off_passes)),
                        prev_x_coordinate = rep(scouting_passes$X.Coordinate[i],nrow(xg_data_off_passes)),
                        prev_y_coordinate = rep(scouting_passes$Y.Coordinate[i],nrow(xg_data_off_passes)),
                        prev_event = rep("Play",nrow(xg_data_off_passes)))
  
  # Unobserved Variables
  xg_test$diff_in_time_prev_event <- xg_data_off_passes$diff_in_time_prev_event
  xg_test$secs_remaining <-scouting_passes$secs_remaining[i] - xg_test$diff_in_time_prev_event
  xg_test$shot_type <- xg_data_off_passes$shot_type
  xg_test$one_timer <- xg_data_off_passes$one_timer
  xg_test$traffic <- xg_data_off_passes$traffic
  xg_test$shot_type <- xg_data_off_passes$shot_type
  xg_test$X.Coordinate <- xg_test$prev_x_coordinate + (xg_data_off_passes$X.Coordinate - xg_data_off_passes$prev_x_coordinate)
  xg_test$Y.Coordinate <- xg_test$prev_y_coordinate + (xg_data_off_passes$Y.Coordinate - xg_data_off_passes$prev_y_coordinate)
  
  # Align Columns
  xg_test <- xg_test %>%
    select(goal,Period,secs_remaining,X.Coordinate,Y.Coordinate,shot_type,strength_state,game_state,traffic,
           one_timer,home,prev_event,prev_x_coordinate,prev_y_coordinate,diff_in_time_prev_event) 
  
  # Create Design Matrix
  x.test <- model.matrix(goal~.,data = rbind(xg_data %>% select(-goal_prob),xg_test) %>% 
                           mutate(Period = as.character(Period),
                                  strength_state = as.character(ifelse(strength_state == -2,-1,strength_state)),
                                  game_state = as.character(game_state)))[(nrow(xg_data)+1):(nrow(xg_data)+nrow(xg_test)),-1]
  
  # Predict on Test Data
  probs_test <- predict(bart_fit1, newdata = x.test)$prob.test
  
  if (i == 1){
    
    # mean of unobserved covariate empirical distribution
    pred_xHA <- matrix(apply(probs_test,1,mean),ncol = 500)
    
  } else {
    
    # mean of unobserved covariate empirical distribution -- rbind with previous results
    pred_xHA <- rbind(pred_xHA,
                      matrix(apply(probs_test,1,mean),ncol = 500))
    
  }
  
  # Update Progress Bar
  setTxtProgressBar(pb, i)
  
}

# Predicted values
mean_values <- apply(pred_xHA,1,mean)

# Add predicted values to data frame
scouting_passes_xHA <- scouting_passes_org %>%
  mutate(xHA = mean_values)

scouting_goals_wpasses_xHA_agg <- scouting_passes_org %>%
  group_by(Player,Team) %>%
  summarize(xHA = sum(xHA),
            #A1 = sum(goal),
            passes = n(),
            XA_per_pass = sum(xHA)/n()) %>%
  ungroup() %>%
  left_join(games_played,by = c("Player","Team")) %>%
  mutate(xHA_per_60 = xHA/gp) %>%
  filter(gp > 5) %>%
  arrange(-xHA_per_60)

# Aggregate Player by xA
scouting_goals_wpasses_5v5_xHA_agg <- scouting_passes_org %>%
  left_join(scouting_passes_wgoals %>% select(index,xA),by = c("index")) %>%
  mutate(xHA = ifelse(is.na(xA),xHA,xA)) %>%
  filter(Home.Team.Skaters == Away.Team.Skaters) %>%
  group_by(Player,Team) %>%
  summarize(xHA = sum(xHA),
            #A1 = sum(goal),
            passes = n(),
            XA_per_pass = sum(xHA)/n()) %>%
  ungroup() %>%
  left_join(games_played,by = c("Player","Team")) %>%
  mutate(xHA_per_60 = xHA/gp,
         passes_per_game = passes/gp) %>%
  filter(gp > 5) %>%
  arrange(-xHA_per_60)

# write out results xHA
write.csv(scouting_passes_xHA,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xHA.csv",row.names = F)



# Packages
library(tidyverse)

# Data
scouting <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
scouting_goals <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xG.csv")
scouting_passes2  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP.csv")

# find shots that where the previous event was a pass
scouting_passes_wgoals <- scouting_passes %>%
  mutate(index_goals = index + 1) %>%
  left_join(scouting_goals %>% mutate(goal = ifelse(Event == "Goal",1,0)) %>% select(index,goal_prob,goal,Team) %>% rename(Team_goal = Team),by = c("index_goals"="index")) %>%
  filter(!is.na(goal_prob)) %>%
  filter(Team == Team_goal) %>%
  rename(xA = goal_prob)

# Group by xA
scouting_goals_wpasses_agg <- scouting_passes_wgoals %>%
  group_by(Player,Team) %>%
  summarize(xA = sum(xA),
            A1 = sum(goal),
            passes = n(),
            XA_per_pass = sum(xA)/n()) %>%
  ungroup() %>%
  left_join(games_played,by = c("Player","Team")) %>%
  mutate(xA_per_60 = xA/gp,
         A1_per_60 = A1/gp) %>%
  filter(gp > 5) %>%
  arrange(-xA_per_60)

# All Situations -- xA vs Primary Assists
ggplot(data = scouting_goals_wpasses_agg %>% filter(passes > 10 & Team == "Erie Otters"),aes(x = A1_per_60,y = xA_per_60,label = Player)) + 
  geom_text() +
  geom_segment(x = 0,y = 0,xend = 0.5,yend = 0.5)

ggplot(data = scouting_goals_wpasses_agg %>% filter(passes > 10 & Team == "Erie Otters") %>%
         mutate(named_players = ifelse(Player %in% c("Jacob Golden","Hayden Fowler","Jamie Drysdale","Maxim Golod","Brendan Kischnick"),"1","0")),
       aes(x = A1_per_60,y = xA_per_60,color = named_players,label = Player)) + 
  geom_point(aes(size = passes)) + 
  geom_segment(x = 0,y = 0,xend = 0.5,yend = 0.5,color = "black") +
  annotate(geom = "text",x = 0.02,y = 0.12,label = "Jacob Golden") +
  annotate(geom = "text",x = 0.16,y = 0.21,label = "Hayden Fowler") +
  annotate(geom = "text",x = 0.35,y = 0.23,label = "Jamie Drysdale") +
  annotate(geom = "text",x = 0.44,y = 0.39,label = "Maxim") +
  annotate(geom = "text",x = 0.44,y = 0.38,label = "Golod") +
  #annotate(geom = "text",x = -0.6,y = 0.14,label = "Brendan Kischnick") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(),
        legend.position = "none") + 
  xlab("Primary Assists Per Game") +
  ylab("Expected Assists Per Game") +
  ggtitle("All Situations")

  
# Group by xA -- Even Strength Situations
scouting_goals_wpasses_5v5_agg <- scouting_passes_wgoals %>%
  filter(Home.Team.Skaters == Away.Team.Skaters) %>%
  group_by(Player,Team) %>%
  summarize(xA = sum(xA),
            A1 = sum(goal),
            passes = n(),
            XA_per_pass = sum(xA)/n(),
            A1_per_pass = sum(A1)/n()) %>%
  ungroup() %>%
  left_join(games_played,by = c("Player","Team")) %>%
  mutate(xA_per_60 = xA/gp,
         A1_per_60 = A1/gp) %>%
  filter(gp > 5) %>%
arrange(-xA_per_60) 

ggplot(data = scouting_goals_wpasses_5v5_agg  %>% filter(passes > 10 & Team == "Erie Otters") %>%
         mutate(named_players = ifelse(Player %in% c("Jacob Golden","Hayden Fowler","Jamie Drysdale","Maxim Golod","Brendan Kischnick"),"1","0")),
       aes(x = A1_per_60,y = xA_per_60,color = named_players,label = Player)) + 
  geom_point(aes(size = passes)) + 
  geom_segment(x = 0,y = 0,xend = 0.5,yend = 0.5,color = "black") +
  annotate(geom = "text",x = 0.01,y = 0.06,label = "Jacob Golden") +
  annotate(geom = "text",x = 0.1,y = 0.165,label = "Hayden Fowler") +
  annotate(geom = "text",x = 0.225,y = 0.12,label = "Jamie Drysdale") +
  annotate(geom = "text",x = 0.24,y = 0.195,label = "Maxim Golod") +
  #annotate(geom = "text",x = -0.6,y = 0.14,label = "Brendan Kischnick") +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.title = element_text(),
        legend.position = "none") + 
  xlab("Primary Assists Per Game") +
  ylab("Expected Assists Per Game") +
  ggtitle("Even Strength Situations")


# write out results to csv
write.csv(scouting_passes_wgoals,"/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_passes_w_xG.csv",row.names = F)







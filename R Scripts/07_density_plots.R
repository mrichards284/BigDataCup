# Packages
library(tidyverse)

# Data
scouting_passes_wgoals <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_passes_w_xG.csv")
scouting_passes  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP_wVars.csv")
scouting_passes_org  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xP.csv")
xg_data  <- read_csv("/Users/marcrichards/Desktop/Big Data Cup/Output/scouting_xG_wVars_20210226.csv")
xg_data_off_passes <- xg_data %>% 
  filter(prev_event %in% c("Play","Incomplete Play"))


### Various plots density plots explored below.

yy<-seq(-40,40,length.out=30)
xx<-seq(-98,98,length.out=30)
breaks_data <-list(xx=xx, yy=yy)
x <- y <- ylo <- yup  <- NULL
outbox <- data.frame(x=c(-98,-98,98,98,-98),
                     y=c(-40,40,40,-40,-40))


rink + stat_summary_2d(data = scouting_passes_xHA %>% 
                    #filter(Team == "Erie Otters") %>%
                    mutate(X.Coordinate = X.Coordinate - 100,Y.Coordinate = Y.Coordinate - 42.5 ),
                  aes(x = X.Coordinate, y = Y.Coordinate, z=comp_prob),
                fun=mean,na.rm = T, breaks=breaks_data) +
  scale_fill_gradient2(low = "blue",
                       mid = "grey90",
                       high = "red",
                       midpoint = 0.75,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar")


Q <- quadratcount(rink, nx= 6, ny=3)

df <-
  tibble(
    x = rnorm(10),
    y = rnorm(10),
    z = floor(rnorm(10)+10)
  ) %>% 
  uncount(z)

ggplot(df, aes(x, y)) +
  stat_density_2d(
    aes(fill = stat(density)), 
    geom = "raster", contour = FALSE
  ) +
  geom_jitter(width = 0.1, height = 0.1) 

otters_xHA <- scouting_passes_xHA %>%
  filter(Team == "Erie Otters" & X.Coordinate > 100) %>%
  mutate(X = X.Coordinate - 100,
         Y = Y.Coordinate - 42.5 ,
         Z = round(comp_prob*100,0)) %>%
  dplyr::select(X,Y,Z) %>%
  uncount(Z)

ggplot(jamie_passes_xHA, aes(x = X, y = Y)) +
  stat_density_2d(
    aes(fill = stat(density)), 
    geom = "raster", contour = FALSE
  )

rink + stat_density_2d(data = otters_xHA,
  aes(x = X,y = Y,fill = stat(density)), 
  geom = "raster", contour = FALSE
) + 
  scale_fill_gradient2(low = "blue",
                       mid = "grey90",
                       high = "red",
                       midpoint = 0,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar")
#+
#  geom_jitter(width = 0.1, height = 0.1) 


otters_xG <- scouting_passes_wgoals %>%
  filter( X.Coordinate > 100) %>%
  mutate(X = X.Coordinate - 100,
         Y = Y.Coordinate - 42.5 ,
         Z = round(xA*100,0)) %>%
  dplyr::select(X,Y,Z) %>%
  uncount(Z)

otters_xP <- scouting_passes[1:14000,] %>%
  #filter(strength_state == 0) %>%
  #filter( X.Coordinate > 100) %>%
  mutate(X = X.Coordinate - 100,
         Y = Y.Coordinate - 42.5 ,
         Z = round(comp_prob*100,0)) %>%
  dplyr::select(X,Y,Z) %>%
  uncount(Z)

otters_xP2 <- scouting_passes[1:14000,] %>%
  #filter( X.Coordinate > 100) %>%
  mutate(X = X.Coordinate.2 - 100,
         Y = Y.Coordinate.2 - 42.5 ,
         Z = round(comp_prob*100,0)) %>%
  dplyr::select(X,Y,Z) %>%
  uncount(Z)

otters_Goals <- scouting_passes_wgoals %>%
  filter( goal == 1) %>%
  mutate(X = X.Coordinate - 100,
         Y = Y.Coordinate - 42.5) %>%
  dplyr::select(X,Y) 

otters_passes <- scouting_passes %>%
  filter( pass == 1) %>%
  mutate(X = X.Coordinate - 100,
         Y = Y.Coordinate - 42.5) %>%
  dplyr::select(X,Y) 

otters_passes2 <- scouting_passes %>%
  filter( pass == 1) %>%
  mutate(X = X.Coordinate.2 - 100,
         Y = Y.Coordinate.2 - 42.5) %>%
  dplyr::select(X,Y) 

rink + stat_density_2d(data = otters_xG,
                       aes(x = X,y = Y,fill = stat(density)), 
                       geom = "raster", contour = FALSE
) + 
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar")



ggplot() +
  stat_density_2d(data = otters_xP,aes(x = X,y = Y,fill = stat(density)), 
                geom = "raster", contour = FALSE
) + 
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0.0001,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar") +
  coord_fixed() +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) +
  labs(fill='Completion Probability') +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 16)) +
  geom_polygon(data = boards, aes(x, y), fill = '#ffcb05') +
  annotation_custom(nhl_logo, xmin = -logo_borders$xmin, xmax = logo_borders$xmax, ymin = -logo_borders$ymin, ymax = logo_borders$ymax) +
  geom_polygon(data = center_circle, aes(x, y), fill = '#0033a0') +
  geom_polygon(data = faceoff_circles_l, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = faceoff_circles_r, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_l_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_l_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_nz_l_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_nz_l_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_nz_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_nz_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_l_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_l_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_nz_l_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_nz_l_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_nz_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_nz_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_rect(data = faceoff_details, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  geom_rect(data = red_line, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  geom_rect(data = red_line_detail, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#ffffff') +
  geom_polygon(data = center_dot, aes(x, y), fill = '#0033a0') +
  geom_polygon(data = ref_crease, aes(x, y), fill = '#c8102e') +
  geom_rect(data = blue_line, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#0033a0') +
  geom_polygon(data = crease, aes(x, y), fill = '#0088ce') +
  geom_polygon(data = crease_outline, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = crease, aes(-x, y), fill = '#0088ce') +
  geom_polygon(data = crease_outline, aes(-x, y), fill = '#c8102e') +
  geom_polygon(data = left_restricted_area, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = right_restricted_area, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = goal, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = goal, aes(-x, y), fill = '#c8102e') +
  geom_polygon(data = goal_fill, aes(x, y), fill = '#939598') +
  geom_polygon(data = goal_fill, aes(-x, y), fill = '#939598') +
  geom_rect(data = goal_line, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e')

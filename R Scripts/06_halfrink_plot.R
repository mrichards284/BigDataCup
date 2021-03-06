#
#
# Dependenant on 01 through 05_rink_lots.R and zz_rink_plot.R
#
#

boards2 <- boards %>%
  filter(x >= 0) %>%
  mutate(x = ifelse(x == 0,26,x))

# Figure of Completed Pass Locations
ggplot() +
  stat_density_2d(data = filter(otters_passes, X > 25),aes(x = X,y = Y,fill = stat(density)), 
                  geom = "raster", contour = FALSE
  ) + 
  scale_fill_gradient2(low = "blue",
                       mid = "white",
                       high = "red",
                       midpoint = 0.0002,
                       space = "Lab",
                       na.value = "grey50",
                       guide = "colourbar") +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  geom_polygon(data = boards2, aes(x, y), fill = '#ffcb05') +
  geom_polygon(data = faceoff_circles_r, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_rect(data = faceoff_details[which(faceoff_details$xmin > 0),], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  geom_rect(data = blue_line[2,], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#0033a0') +
  geom_polygon(data = crease, aes(-x, y), fill = '#0088ce') +
  geom_polygon(data = crease_outline, aes(-x, y), fill = '#c8102e') +
  #geom_polygon(data = right_restricted_area, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = goal, aes(-x, y), fill = '#c8102e') +
  geom_polygon(data = goal_fill, aes(-x, y), fill = '#939598') +
  geom_rect(data = goal_line[1,], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  coord_flip()
  


# Jamie Drysdale Half rink 
jamie_logo = readPNG('/Users/marcrichards/Desktop/Big Data Cup/Images/jamie-drysdale-2020-879.png')
jamie_logo = rasterGrob(jamie_logo, interpolate = TRUE)

scouting_passes_wgoals2 <- scouting_passes_xHA %>%
  left_join(scouting_passes_wgoals %>% dplyr::select(index,xA,goal),by = c("index")) %>%
  mutate(xHA = ifelse(is.na(xA),xHA,xA))

jamie_assist <- ggplot() +
  stat_density_2d(data = filter(scouting_passes_wgoals2, X.Coordinate.2 > 125 & Player == "Jamie Drysdale") %>% 
                    mutate(X = X.Coordinate.2 - 100,Y = Y.Coordinate.2 - 42.5),aes(x = X,y = Y,fill = stat(density)), 
                  geom = "raster", contour = FALSE
  ) +
  geom_point(data = filter(scouting_passes_wgoals2, X.Coordinate > 125 & Player == "Jamie Drysdale") %>% 
               mutate(X.Coordinate = X.Coordinate - 100,
                      Y.Coordinate = Y.Coordinate - 42.5,
                      Assist = ifelse(is.na(goal),0,goal),
                      xA_high = ifelse(xHA >= .1,1,0)) %>%
               dplyr::select(X.Coordinate,Y.Coordinate,Assist,xHA,xA_high) %>%
               rbind(data.frame(X.Coordinate = c(98,94),
                                Y.Coordinate = c(-41.5,-41.5),
                                Assist = c(0,1),
                                xHA = c(0.4,0.4),
                                xA_high = c(1,1))),aes(x = X.Coordinate,y = Y.Coordinate,size = xHA,color = factor(xA_high),shape = factor(Assist))) + 
  scale_color_manual(values=c("white", "yellow")) +
  #scale_fill_distiller(palette = 'RdPu',
  #                     direction = 1,
  #limits = c(0, 4000),
  #breaks = c(0, 1000, 2000, 3000, 4000),
  #labels = c('0', '1,000', '2,000', '3,000', '4,000'),
  #name = 'Crime Count 2019',
  #                     oob = scales::squish) +
  #scale_color_gradient2(low = "blue",
  #                     mid = "white",
  #                     high = "red",
  #                     midpoint = 0.65,
#                     space = "Lab",
#                     na.value = "grey50",
#                     guide = "colourbar") +
theme(
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
) +
  geom_polygon(data = boards2, aes(x, y), fill = '#ffcb05') +
  geom_polygon(data = faceoff_circles_r, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_rect(data = faceoff_details[which(faceoff_details$xmin > 0),], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  geom_rect(data = blue_line[2,], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#0033a0') +
  geom_polygon(data = crease, aes(-x, y), fill = '#0088ce') +
  geom_polygon(data = crease_outline, aes(-x, y), fill = '#c8102e') +
  #geom_polygon(data = right_restricted_area, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = goal, aes(-x, y), fill = '#c8102e') +
  geom_polygon(data = goal_fill, aes(-x, y), fill = '#939598') +
  geom_rect(data = goal_line[1,], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  #annotation_custom(jamie_logo, xmin = 91, xmax = 101.25, ymin = -36, ymax = -43.5) +
  ggtitle("Jamie Drysdale D, Erie Otters") +
  theme(plot.title = element_text(hjust = 0.5,vjust = -2,size = 18)) +
  annotate(geom = "text",x = 98,y = -34.5,label = "xHA > 10%",color = "white") +
  annotate(geom = "text",x = 94,y = -36.5,label = "Assist",color = "white") +
  #annotate(geom = "text",x = 110,y = -42,label = "Jamie Drysdale") +
  #ggtitle("Jamie Drysdale") +
  coord_flip()


# Maxim assist half rink plot
maxim_assist <- ggplot() +
  stat_density_2d(data = filter(scouting_passes_wgoals2, X.Coordinate.2 > 125 & Player == "Maxim Golod") %>% 
                    mutate(X = X.Coordinate.2 - 100,Y = Y.Coordinate.2 - 42.5),aes(x = X,y = Y,fill = stat(density)), 
                  geom = "raster", contour = FALSE
  ) +
  geom_point(data = filter(scouting_passes_wgoals2, X.Coordinate > 125 & Player == "Maxim Golod") %>% 
               mutate(X.Coordinate = X.Coordinate - 100,
                      Y.Coordinate = Y.Coordinate - 42.5,
                      Assist = ifelse(is.na(goal),0,goal),
                      xA_high = ifelse(xHA >= .1,1,0)) %>%
               dplyr::select(X.Coordinate,Y.Coordinate,Assist,xHA,xA_high) %>%
               rbind(data.frame(X.Coordinate = c(98,94),
                                Y.Coordinate = c(-41.5,-41.5),
                                Assist = c(0,1),
                                xHA = c(0.4,0.4),
                                xA_high = c(1,1))),aes(x = X.Coordinate,y = Y.Coordinate,size = xHA,color = factor(xA_high),shape = factor(Assist))) + 
  scale_color_manual(values=c("white", "yellow")) +
  #scale_fill_distiller(palette = 'RdPu',
  #                     direction = 1,
  #limits = c(0, 4000),
  #breaks = c(0, 1000, 2000, 3000, 4000),
  #labels = c('0', '1,000', '2,000', '3,000', '4,000'),
  #name = 'Crime Count 2019',
  #                     oob = scales::squish) +
  #scale_color_gradient2(low = "blue",
  #                     mid = "white",
  #                     high = "red",
  #                     midpoint = 0.65,
#                     space = "Lab",
#                     na.value = "grey50",
#                     guide = "colourbar") +
theme(
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
) +
  geom_polygon(data = boards2, aes(x, y), fill = '#ffcb05') +
  geom_polygon(data = faceoff_circles_r, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = l_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = h_r_dot_fill, aes(x, y), fill = '#c8102e') +
  geom_rect(data = faceoff_details[which(faceoff_details$xmin > 0),], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  geom_rect(data = blue_line[2,], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#0033a0') +
  geom_polygon(data = crease, aes(-x, y), fill = '#0088ce') +
  geom_polygon(data = crease_outline, aes(-x, y), fill = '#c8102e') +
  #geom_polygon(data = right_restricted_area, aes(x, y), fill = '#c8102e') +
  geom_polygon(data = goal, aes(-x, y), fill = '#c8102e') +
  geom_polygon(data = goal_fill, aes(-x, y), fill = '#939598') +
  geom_rect(data = goal_line[1,], aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#c8102e') +
  #annotation_custom(jamie_logo, xmin = 91, xmax = 101.25, ymin = -36, ymax = -43.5) +
  ggtitle("Maxim Golod F, Erie Otters") +
  theme(plot.title = element_text(hjust = 0.5,vjust = -2,size = 18)) +
  annotate(geom = "text",x = 98,y = -34.5,label = "xHA > 10%",color = "white") +
  annotate(geom = "text",x = 94,y = -36.5,label = "Assist",color = "white") +
  #annotate(geom = "text",x = 110,y = -42,label = "Jamie Drysdale") +
  #ggtitle("Jamie Drysdale") +
  coord_flip()

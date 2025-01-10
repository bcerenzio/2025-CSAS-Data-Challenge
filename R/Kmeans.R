library(tidyverse)
library(ClusterR)
library(colorspace)
library(plotly)

elbow_swing <- Optimal_Clusters_KMeans(reframe(competitive_swing_data, scale(bat_speed), scale(swing_length), scale(rotational_acceleration), scale(time_to_contact)),
                        max_clusters = 12,
                        criterion = 'variance_explained',
                        num_init = 10,
                        max_iters = 500,
                        verbose = TRUE) %>% #7
  as_tibble() %>% 
  mutate(cluster = row_number())

elbow_swing %>% ggplot(aes(cluster, value)) +
  geom_line(color = 'blue', linetype = 'dashed') +
  scale_x_continuous(breaks = seq(0,12,by = 1))+
  labs(title = 'KMeans Elbow Plot',
       y = 'Variance Explained',
       x = 'Cluster') +
  theme_bw() +
  geom_text(aes(cluster-0.35, value-0.02, label = round(value,2)), family = 'serif', fontface = 'bold') +
  geom_vline(xintercept = 7, linetype = 'dotted')


set.seed(101);swing_clusters <- kmeans(reframe(competitive_swing_data, scale(bat_speed), scale(swing_length), scale(rotational_acceleration), scale(time_to_contact)), 
                   centers = 7,
                   nstart = 30)

competitive_swing_data$cluster <- fitted(swing_clusters, method = 'classes')

#converting scales to averages for each cluster
swing_cluster_means <- as_tibble(swing_clusters$centers) %>% 
  mutate(cluster = row_number()) %>% 
  group_by(cluster) %>% 
  reframe(
    avg_bat_speed = `scale(bat_speed)`*sd(competitive_swing_data$bat_speed) + mean(competitive_swing_data$bat_speed),
    avg_swing_length = `scale(swing_length)`*sd(competitive_swing_data$swing_length) + mean(competitive_swing_data$swing_length),
    avg_rotational_acceleration = `scale(rotational_acceleration)`*sd(competitive_swing_data$rotational_acceleration) + mean(competitive_swing_data$rotational_acceleration),
    avg_time_to_contact = `scale(time_to_contact)`*sd(competitive_swing_data$time_to_contact) + mean(competitive_swing_data$time_to_contact)
  )

write_csv(swing_cluster_means, 'swing_cluster_means.csv')

cluster_swing_plot <- competitive_swing_data %>%
  ggplot(aes(swing_length, bat_speed, color = as.factor(cluster))) +
  geom_point()+
  #geom_jitter(position = position_jitter(width = 0.1, height = 0.5)) +
  scale_color_discrete(name = 'Cluster') +
  geom_vline(xintercept = mean(competitive_swing_data$swing_length), linetype = 'dotted') +
  geom_hline(yintercept = mean(competitive_swing_data$bat_speed), linetype = 'dotted') +
  geom_point(data = swing_cluster_means, aes(avg_swing_length, avg_bat_speed), inherit.aes = FALSE) +
  theme_bw() +
  labs(y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)',
       title = 'Swing Clusters') +
  annotate('text', x = 6.3, y = 73.07, label = '4', family = 'serif',
           fontface = 'bold') +
  annotate('text', x = 5.96, y = 65.96, label = '5', family = 'serif',
           fontface = 'bold') +
  annotate('text', x = 7.05, y = 66.8, label = '7', family = 'serif',
           fontface = 'bold') +
  annotate('text', x = 7.03, y = 72.24, label = '6', family = 'serif',
           fontface = 'bold') +
  annotate('text', x = 8.24, y = 67.485, label = '1', family = 'serif',
           fontface = 'bold') +
  annotate('text', x = 8.03, y = 73.95, label = '2', family = 'serif',
           fontface = 'bold') +
  annotate('text', x = 7.49, y = 78.79, label = '3', family = 'serif',
           fontface = 'bold')

plotly::plot_ly(
  data = competitive_swing_data,
  x = ~swing_length,
  y = ~bat_speed,
  z = ~rotational_acceleration,
  color = ~factor(cluster),  # Color by cluster
  type = "scatter3d",
  mode = "markers"
) %>%
  plotly::layout(
    scene = list(
      xaxis = list(title = "Swing Length (ft)"),
      yaxis = list(title = "Bat Speed (mph)"),
      zaxis = list(title = "Rotational<br>Acceleration (g)")
    )
  )

cluster_swing_stats <- competitive_swing_data %>% 
  group_by(cluster) %>% 
  reframe(
    number_of_swings = n(),
    bat_speed = mean(bat_speed),
    swing_length = mean(swing_length),
    rotational_acceleration = mean(rotational_acceleration),
    time_to_contact = mean(time_to_contact),
   # BA = sum(hit_contact, na.rm = TRUE)/sum(AB, na.rm=TRUE),
    woba = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm=TRUE),
    squared_up_swing_pct = mean(squared_up_swing, na.rm = TRUE),
    blast_swing_pct = mean(blast_swing),
    squared_up_contact_pct = mean(squared_up_contact, na.rm = TRUE),
    blast_contact_pct = mean(blast_contact, na.rm = TRUE),
    contact_pct = mean(contact),
   gb_pct = mean(ifelse(bb_type == 'ground_ball', 1,0
   ), na.rm = TRUE),
   whiff_pct = mean(whiff),
   poor_contact_pct = mean(case_when(
     launch_speed_angle %in% 1:3 ~ 1,
     launch_speed_angle %in% 4:6 ~ 0,
     .default = NA
     ), na.rm = TRUE),
   barrel_pct_contact = mean(ifelse(launch_speed_angle == 6, 1,0), na.rm = TRUE),
   barrel_pct_swing = mean(ifelse(launch_speed_angle %in% 6, 1,0))
  )

write_csv(cluster_swing_stats, 'cluster_swing_stats.csv')


competitive_swing_data %>%
  left_join(select(cluster_swing_stats, -bat_speed,
                   -swing_length, -rotational_acceleration,
                   -time_to_contact), by = 'cluster') %>% 
  ggplot(aes(swing_length, bat_speed, color = woba, group = cluster)) +
  geom_point(size = 2.5) +
  scale_color_continuous_divergingx('RdBu', rev = TRUE,
                                    mid = mean(cluster_swing_stats$woba), n_inter = 100,
                                    name = 'wOBA') +
  geom_vline(xintercept = mean(competitive_swing_data$swing_length), linetype = 'dotted') +
  geom_hline(yintercept = mean(competitive_swing_data$bat_speed), linetype = 'dotted') +
  #geom_point(data = summarized_cluster_means, aes(avg_swing_length, avg_bat_speed), inherit.aes = FALSE) +
  theme_bw()


competitive_swing_data %>%
  left_join(select(cluster_swing_stats, -bat_speed,
                   -swing_length, -rotational_acceleration,
                   -time_to_contact), by = 'cluster') %>% 
  ggplot(aes(swing_length, bat_speed, color = squared_up_swing_pct, group = cluster)) +
  geom_point(size = 2.5) +
  scale_color_continuous_divergingx('RdBu', rev = TRUE,
                                    mid = mean(cluster_swing_stats$squared_up_swing_pct), n_inter = 100,
                                    name = 'SquaredUp%') +
  geom_vline(xintercept = mean(competitive_swing_data$swing_length), linetype = 'dotted') +
  geom_hline(yintercept = mean(competitive_swing_data$bat_speed), linetype = 'dotted') +
  #geom_point(data = summarized_cluster_means, aes(avg_swing_length, avg_bat_speed), inherit.aes = FALSE) +
  theme_bw()
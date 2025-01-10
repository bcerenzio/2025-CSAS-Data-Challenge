library(tidyverse)
library(ggridges)
library(colorspace)
library(gridExtra)
theme_set(theme_bw())

grid.arrange(competitive_swing_data %>% ggplot(aes(bat_speed)) +
  geom_density(fill = 'blue') +
    xlab('Bat Speed (MPH)'), swing_data %>% 
    filter(competitive_swing == 1) %>% ggplot(aes(swing_length)) + geom_density(fill = 'red') +
    xlab('Swing Length (ft)'))


woba_plot <- swing_data %>% 
  filter(competitive_swing == 1, woba_denom == 1) %>% 
  ggplot(aes(swing_length, bat_speed, z = woba_value)) +
  stat_summary_hex(bins = 20) +
  scale_fill_continuous_divergingx('RdBu', mid = mean(summarized_data$woba),
                                   name = 'wOBA', rev = TRUE,
                                   #breaks = seq(0.4,1,by = 0.2),
                                   limits = c(0.1,0.5),
                                   oob = scales::squish,
                                   n_inter = 51)+
  #scale_fill_continuous(low = 'royalblue1',high = 'tomato', name = 'wOBA') +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted') +
  labs(title = 'wOBA By Bat Speed & Swing Length',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')


contact_pct_plot <- swing_data %>%
  filter(competitive_swing ==1, description %in% c('foul', 'hit_into_play','swinging_strike',
                                            'swinging_strike_blocked')) %>% 
  mutate(contact = contact*100) %>% 
  ggplot(aes(swing_length, bat_speed, z = contact))+
  stat_summary_hex(fun = mean, bins= 20) +
  scale_fill_continuous_divergingx('RdBu', mid = mean(swing_data %>%
                                                        filter(competitive_swing ==1, description %in% c('foul', 'hit_into_play','swinging_strike',
                                                                                                         'swinging_strike_blocked')) %>%
                                                        pull(contact)*100),
                                   name = 'Contact%', rev = TRUE,
                                   breaks = seq(40,100,by = 20),
                                   limits = c(40,100),
                                   oob = scales::squish,
                                   n_inter = 51)+
  # scale_fill_continuous(low = 'royalblue1',high = 'tomato', name = 'Contact%',
  #                  breaks = seq(0.2,1.00, by = 0.2),
  #                  labels = seq(0.2,1.00, by = 0.2),
  #                  limits = c(0.2,1.00),
  #                  oob = scales::squish)+
  #scale_fill_continuous(low = 'lightblue', high = 'tomato2') +
  theme_bw() +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted') +
  labs(title = 'Contact% By Bat Speed & Swing Length',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

grid.arrange(woba_plot, contact_pct_plot)

summarized_data %>%
  ggplot(aes(swing_length, bat_speed, z = contact_pct))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', mid = mean(summarized_data$contact_pct),
                                   name = 'Contact%', rev = TRUE)+
  theme_bw() +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted') +
  labs(title = 'Season Contact% by Bat Speed and Swing Length',
       y = 'Bat Speed (MPH) (Season Averages)',
       x = 'Swing Length (ft) (Season Averages)')


whiff_plot <-swing_data %>%
  filter(competitive_swing == 1, description %in% c('foul', 'hit_into_play','swinging_strike',
                                            'swinging_strike_blocked')) %>% 
  ggplot(aes(swing_length, bat_speed, z = whiff*100))+
  stat_summary_hex(fun = mean, bins= 20) +
  scale_fill_continuous_divergingx('RdBu', mid = mean(swing_data %>%
                                                        filter(competitive_swing == 1, description %in% c('foul', 'hit_into_play','swinging_strike',
                                                                                                          'swinging_strike_blocked')) %>% 
                                                        mutate(whiff = whiff*100) %>% 
                                                        pull(whiff)),
                                   name = 'Whiff%', rev = TRUE, n_interp = 51,
                                   breaks = seq(0,50, by = 10),
                                   limits = c(0,50),
                                   oob = scales::squish)+
  # scale_fill_continuous(low = 'royalblue1',high = 'tomato', name = 'Whiff%',
  #                       breaks = seq(0,0.8, by = 0.2),
  #                       labels = seq(0,0.8, by = 0.2),
  #                       limits = c(0,0.8),
  #                       oob = scales::squish)+
  #scale_fill_continuous(low = 'lightblue', high = 'tomato2') +
  theme_bw() +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted')


grid.arrange(woba_plot, contact_pct_plot, whiff_plot, ncol = 2)

summarized_data %>%
  ggplot(aes(swing_length, bat_speed, z = whiff_pct))+
  stat_summary_hex(fun = mean, bins= 20) +
  scale_fill_continuous_divergingx('RdBu', mid = mean(summarized_data$whiff_pct),
                                   name = 'Whiff%', rev = TRUE)+
  #scale_fill_continuous(low = 'lightblue', high = 'tomato2') +
  theme_bw() +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted')



# creating scatter plot between swing length and bat speed to
# illustrate that swing length increases with bat speed
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed))+
  geom_jitter(color = 'skyblue')+
  theme_bw() +
  geom_smooth(method = 'lm', color = 'black') +
  labs(title = 'Bat Speed (MPH) vs Swing Length (ft)',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# Chase% Plot
summarized_data %>%
  ggplot(aes(swing_length, bat_speed, z = chase_pct*100))+
  stat_summary_hex(fun = mean, bins= 20) +
  scale_fill_continuous_divergingx('RdBu', mid = mean(summarized_data$chase_pct*100),
                                   name = 'Chase%', rev = TRUE)+
  #scale_fill_continuous(low = 'lightblue', high = 'tomato2') +
  theme_bw() +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted') +
  labs(title = 'Chase% by Bat Speed and Swing Length',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')


# Finding xBA by swing type
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = estimated_ba_using_speedangle))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'xBA', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(estimated_ba_using_speedangle), na.rm = TRUE),
                                   limits = c(0.1,0.5),
                                   oob = scales::squish) +
  geom_vline(xintercept = 7.256, linetype = 'dotted') +
  geom_hline(yintercept = 70.92, linetype = 'dotted') +
  labs(title = 'xBA based on Bat Speed (MPH) and Swing Length (ft)',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# SquaredUp Swing%
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = squared_up_swing*100))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Squared Up\nSwing%', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(squared_up_swing)*100, na.rm = TRUE),
                                   limit = c(0,50),
                                   oob = scales::squish
                                   ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'Squared Up Swing% based on Bat Speed (MPH) and Swing Length (ft)',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# SquaredUp Contact%
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = squared_up_contact*100))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Squared Up\nContact%', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(squared_up_contact)*100, na.rm = TRUE),
                                   limit = c(10,50),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted')+
  labs(title = 'Squared Up Contact% based on Bat Speed (MPH) and Swing Length (ft)',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# Blast Swing%
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = blast_swing*100))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Blast\nSwing%', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(blast_swing)*100, na.rm = TRUE),
                                   limit = c(0,25),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted')+
  labs(title = 'Blast Swing% based on Bat Speed (MPH) and Swing Length (ft)',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# Blast Contact%
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = blast_contact*100))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Blast\nContact%', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(blast_contact)*100, na.rm = TRUE),
                                   limit = c(0,25),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted')+
  labs(title = 'Blast Contact% based on Bat Speed (MPH) and Swing Length (ft)',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')


# boxplot of swing length with different baseouts situations

swing_data$matchup.splits.menOnBase <- factor(swing_data$matchup.splits.menOnBase,
                                              levels = c('Empty','Men_On','RISP','Loaded'))

swing_data %>% ggplot(aes(x = matchup.splits.menOnBase, y = swing_length, fill = matchup.splits.menOnBase)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('Empty', 'Men_On')))+
  ylim(c(5,14)) +
  geom_hline(yintercept = 7.3, linetype = 'dotted') +
  theme_bw() +
  labs(fill = 'Base Situation',
       y = 'Swing Length (ft)',
       x = 'Base Situation',
       title = 'Swing Length based on Base Situation')

swing_data %>% ggplot(aes(x = matchup.splits.menOnBase, y = swing_length, fill = matchup.splits.menOnBase)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('Empty', 'RISP')))+
  ylim(c(5,14)) +
  geom_hline(yintercept = 7.3, linetype = 'dotted') +
  theme_bw() +
  labs(fill = 'Base Situation',
       y = 'Swing Length (ft)',
       x = 'Base Situation',
       title = 'Swing Length based on Base Situation')

swing_data %>% ggplot(aes(x = matchup.splits.menOnBase, y = swing_length, fill = matchup.splits.menOnBase)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('Empty', 'Loaded')))+
  ylim(c(5,14)) +
  geom_hline(yintercept = 7.3, linetype = 'dotted') +
  theme_bw() +
  labs(fill = 'Base Situation',
       y = 'Swing Length (ft)',
       x = 'Base Situation',
       title = 'Swing Length based on Base Situation')

swing_data %>% ggplot(aes(x = matchup.splits.menOnBase, y = swing_length, fill = matchup.splits.menOnBase)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('Empty', 'Loaded')),
                        annotations = 'p = 0.86')+
  ylim(c(5,14)) +
  geom_hline(yintercept = 7.3, linetype = 'dotted') +
  theme_bw() +
  labs(fill = 'Base Situation',
       y = 'Swing Length (ft)',
       x = 'Base Situation',
       title = 'Swing Length based on Base Situation')


# density plots
competitive_swing_data %>% 
  ggplot(aes(x = swing_length, y = matchup.splits.menOnBase, fill = matchup.splits.menOnBase)) +
  geom_density_ridges(scale = 3, alpha = 0.6) +
  geom_vline(xintercept = mean(filter(swing_data, competitive_swing == 1)$swing_length), linetype = 'dotted') +
  labs(fill = 'Base Situation',
       x = 'Swing Length (ft)',
       y = 'Base Situation')


competitive_swing_data %>% 
  ggplot(aes(x = bat_speed, y = matchup.splits.menOnBase, fill = matchup.splits.menOnBase)) +
  geom_density_ridges(scale = 3, alpha = 0.6) +
  geom_vline(xintercept = 72, linetype = 'dotted') +
  labs(fill = 'Base Situation',
       x = 'Bat Speed (MPH)',
       y = 'Base Situation')


competitive_swing_data %>% 
  ggplot(aes(x = swing_length, y = as.factor(strikes), fill = as.factor(strikes))) +
  geom_density_ridges(scale = 3, alpha = 0.6) +
  geom_vline(xintercept = 7.4, linetype = 'dotted') +
  labs(fill = 'Strikes',
       x = 'Swing Length (ft)',
       y = 'Strikes')

competitive_swing_data %>% 
  ggplot(aes(x = bat_speed, y = as.factor(strikes), fill = as.factor(strikes))) +
  geom_density_ridges(scale = 3, alpha = 0.6) +
  geom_vline(data = swing_data %>% filter(competitive_swing == 1), aes(xintercept = mean(bat_speed)), linetype = 'dotted')+
  labs(fill = 'Strikes',
       x = 'Bat Speed (MPH)',
       y = 'Strikes')

# Different Swing Length based on base situation

on_base_season_stats$matchup.splits.menOnBase <- factor(on_base_season_stats$matchup.splits.menOnBase,
                                                        levels = c('Empty', 'Men_On', 'RISP', 'Loaded'))

on_base_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = matchup.splits.menOnBase, fill = matchup.splits.menOnBase)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('Empty', 'RISP'))) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  labs(fill = 'Base Situation',
       title = 'Batter Change in Swing Length Relative to Season Average',
       subtitle = '(0 is Season Average)',
       y = 'Change in Swing Length (ft)',
       x = 'Base Situation')


# different swing length based on number of outs
outs_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = as.factor(outs_when_up), fill = as.factor(outs_when_up), group = outs_when_up)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2'))) +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  labs(fill = 'Number of Outs',
       title = 'Batter Change in Swing Length Relative to Season Average',
       subtitle = '(0 is Season Average)',
       x = 'Number of Outs',
       y = 'Change in Swing Length (ft)')



outs_swing_length <- outs_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = as.factor(outs_when_up), fill = as.factor(outs_when_up), group = outs_when_up)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2')),
                        annotations = 'p = 0.0001') +
  geom_hline(yintercept = 0, linetype = 'dashed')+
  labs(fill = 'Number of Outs',
       title = 'Batter Change in Swing Length Relative to Season Average',
       subtitle = '(0 is Season Average)',
       x = 'Number of Outs',
       y = 'Change in Swing Length (ft)') +
  ylim(c(-0.25,0.35))


outs_season_stats %>% ggplot(aes(y = bat_speed_rel_avg, x = as.factor(outs_when_up), fill = as.factor(outs_when_up), group = outs_when_up)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2'))) +
  geom_hline(yintercept = 0, linetype = 'dashed')

outs_bat_speed <- outs_season_stats %>% ggplot(aes(y = bat_speed_rel_avg, x = as.factor(outs_when_up), fill = as.factor(outs_when_up), group = outs_when_up)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2')),
                        annotations = 'p = 2.5e-07') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(fill = 'Number of Outs',
       title = 'Batter Change in Bat Speed Relative to Season Average',
       subtitle = '(0 is Season Average)',
       x = 'Number of Outs',
       y = 'Change in Bat Speed (MPH)') +
  ylim(c(-1.5,1.5))

outs_season_stats %>% ggplot(aes(y = rotational_acceleration_rel_avg, x = as.factor(outs_when_up), fill = as.factor(outs_when_up), group = outs_when_up)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2')))+
  geom_hline(yintercept = 0, linetype = 'dashed')

outs_rot_accel <- outs_season_stats %>% ggplot(aes(y = rotational_acceleration_rel_avg, x = as.factor(outs_when_up), fill = as.factor(outs_when_up), group = outs_when_up)) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2')),
                        annotations = 'p = 3.2e-15')+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  labs(fill = 'Number of Outs',
       title = 'Batter Change in Rotational Acceleration Relative to Season Average',
       subtitle = '(0 is Season Average)',
       x = 'Number of Outs',
       y = 'Change in Rotational Acceleration (g)') +
  ylim(c(-1.5,1.5))

grid.arrange(outs_swing_length, outs_bat_speed, outs_rot_accel,
             ncol = 2)


# different swing length based on number of strikes
strikes_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = as.factor(strikes), fill = as.factor(strikes))) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '2'))) 

# Swings Lengths in different strike counts
# (Comparing 0 to 2 strike counts)
strikes_2 <- strikes_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = as.factor(strikes), fill = as.factor(strikes))) +
  geom_boxplot() +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('0', '2'))) +
  geom_hline(yintercept = 0, linetype = 'dotted')+
  labs(
    title = 'Batter Change in Swing Length\nRelative to Season Average',
    subtitle = '(0 is Season Average)',
    x = 'Number of Strikes',
    y = 'Change in Swing Length (ft)')

strikes_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = as.factor(strikes), fill = as.factor(strikes))) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c('0', '1'))) # p = 1.43-13

# Comparing swing lengths in 0 to 1 strike counts
strikes_1 <- strikes_season_stats %>% ggplot(aes(y = swing_length_rel_avg, x = as.factor(strikes), fill = as.factor(strikes))) +
  geom_boxplot() +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('0', '1')),
                        annotations = c('p = 1.4e-13'))+
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(
       title = 'Batter Change in Swing Length\nRelative to Season Average',
       subtitle = '(0 is Season Average)',
       x = 'Number of Strikes',
       y = 'Change in Swing Length (ft)')

# comparing rotational acceleration in different strike counts
strikes_season_stats %>% ggplot(aes(y = rotational_acceleration_rel_avg, x = as.factor(strikes), fill = as.factor(strikes))) +
  geom_boxplot() +
  guides(fill = 'none') +
  ggsignif::geom_signif(comparisons = list(c('0', '1'))) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(
    title = 'Batter Change in Rotational Acceleration Relative to Season Average',
    subtitle = '(0 is Season Average)',
    x = 'Number of Strikes',
    y = 'Change in Rotational Acceleration (g)')

grid.arrange(strikes_1, strikes_2, nrow = 1)

# Whiff Rate vs Rotational Acceleration 
summarized_data %>% ggplot(aes(rotational_acceleration, whiff_pct)) +
  geom_point(color = 'tomato3') +
  geom_smooth(method = 'lm', se = FALSE, color = 'royalblue') +
  annotate('text', x = 19.05, y = 19.4, label = round(coefficients(lm(
    whiff_pct ~ rotational_acceleration, data = summarized_data
  ))[1],2), angle = 10) + 
  annotate('text', x = 19.6, y =20, label = glue::glue('+ {round(coefficients(lm(
    whiff_pct ~ rotational_acceleration, data = summarized_data
  ))[2],2)}x'), angle = 10)

summarized_data %>% ggplot(aes(swing_length, whiff_pct)) +
  geom_point(color = 'tomato3') +
  geom_smooth(method = 'lm', se = FALSE, color = 'royalblue') +
  annotate('text', x = 6.04, y = 17.1, label = round(coefficients(lm(
    whiff_pct ~ swing_length, data = summarized_data
  ))[1],2), angle = 17) + 
  annotate('text', x = 6.23, y =18.5, label = glue::glue('+ {round(coefficients(lm(
    whiff_pct ~ swing_length, data = summarized_data
  ))[2],2)}x'), angle = 17)


summarized_data %>% ggplot(aes(swing_length,rotational_acceleration, z = whiff_pct)) +
  stat_summary_hex() +
  scale_fill_continuous_divergingx('RdBu', rev = TRUE, 
                                   mid = mean(summarized_data$whiff_pct),
                                   name = 'Whiff%')


summarized_data %>% ggplot(aes(bat_speed,rotational_acceleration, z = whiff_pct)) +
  stat_summary_hex() +
  scale_fill_continuous_divergingx('RdBu', rev = TRUE, 
                                   mid = mean(summarized_data$whiff_pct),
                                   name = 'Whiff%') +
  geom_abline(slope = 1/1.2, intercept = -36, linetype = 'dashed')# +
 # ylim(c(0, 30)) +
 # xlim(c(0,90))

# Compares whiff% across bat speed, swing length, rotational acceleration, 
# and time to contact
grid.arrange( summarized_data %>% ggplot(aes(scale(bat_speed), whiff_pct)) +
    geom_point(color = 'tomato3') +
    geom_smooth(method = 'lm') +
    ylab('Whiff%') +
    xlab('Scaled Bat Speed'),  summarized_data %>% ggplot(aes(scale(swing_length), whiff_pct)) +
    geom_point(color = 'tomato3') +
    geom_smooth(method = 'lm') +
    ylab('Whiff%') +
    xlab('Scaled Swing Length'), 
    summarized_data %>% ggplot(aes(scale(rotational_acceleration), whiff_pct)) +
      geom_point(color = 'tomato3') +
      geom_smooth(method = 'lm')+
      ylab('Whiff%') +
      xlab('Scaled Rotational Acceleration'),
    summarized_data %>% ggplot(aes(scale(time_to_contact), whiff_pct)) +
      geom_point(color = 'tomato3') +
      geom_smooth(method = 'lm')+
      ylab('Whiff%') +
      xlab('Scaled Time to Contact'),ncol = 2)

# Hit Probability (on contact)
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = hit_contact))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Hit Probability', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(hit_contact), na.rm = TRUE),
                                   limit = c(0.1,0.5),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'Hit Probability On Contact',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# Hit Probability (on swing)
competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = hit_swing))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Hit Probability', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(hit_swing), na.rm = TRUE),
                                   limit = c(0.1,0.16),
                                   labels = seq(0.1,0.16, by = 0.02),
                                   breaks = seq(0.1,0.16, by = 0.02),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'Hit Probability On Swing',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')


competitive_swing_data %>% 
  ggplot(aes(rotational_acceleration, time_to_contact, z = hit_swing))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Hit Probability', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(hit_swing), na.rm = TRUE),
                                   limit = c(0.1,0.16),
                                   labels = seq(0.1,0.16, by = 0.02),
                                   breaks = seq(0.1,0.16, by = 0.02),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
   labs(title = 'Hit Probability On Swing',
       y = 'Time to Contact (s)',
       x = 'Rotational Acceleration (g)')

competitive_swing_data %>% 
  ggplot(aes(swing_length, bat_speed, z = rotational_acceleration))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Rotational\nAcceleration (g)', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(rotational_acceleration), na.rm = TRUE),
                                   limits = c(10,40),
                                   breaks = seq(10,40, by = 10),
                                   labels = seq(10, 40, by = 10),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'Rotational Acceleration by Bat Speed and Swing Length',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

summarized_data %>% 
  ggplot(aes(swing_length, bat_speed, z = rotational_acceleration))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Rotational\nAcceleration (g)', rev = TRUE, 
                                   mid = mean(summarized_data$rotational_acceleration, na.rm = TRUE),
                                   # limit = c(0.1,0.5),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(summarized_data$swing_length, na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(summarized_data$bat_speed, na.rm = TRUE), linetype = 'dotted') 



# Season BA
summarized_data %>% 
  ggplot(aes(swing_length, bat_speed, z = BA))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'Season BA', rev = TRUE, 
                                   mid = mean(summarized_data$BA, na.rm = TRUE),
                                   limit = c(0.15,0.35),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing == 1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'Season BA',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')


# Men with scoring position runs
# Hit Probability (on contact)
competitive_swing_data %>% 
  filter(matchup.splits.menOnBase == 'RISP',
         AB == 1) %>% 
  ggplot(aes(swing_length, bat_speed, z = result.rbi))+
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'RBIs', rev = TRUE, 
                                   mid = mean(swing_data %>% 
                                                filter(competitive_swing ==1) %>% 
                                                pull(hit_contact), na.rm = TRUE),
                                   limit = c(0.1,0.5),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(swing_length), na.rm = TRUE), linetype = 'dotted') +
  geom_hline(yintercept = mean(swing_data %>% 
                                 filter(competitive_swing ==1) %>% 
                                 pull(bat_speed), na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'RBIs',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')

# K%
summarized_data %>% ggplot(aes(swing_length, bat_speed, z = k_pct)) +
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_continuous_divergingx('RdBu', name = 'K%', rev = TRUE, 
                                   mid = mean(summarized_data$k_pct, na.rm = TRUE),
                                   limit = c(0.1,0.4),
                                   n_inter = 51,
                                   oob = scales::squish
  ) +
  geom_vline(xintercept = mean(summarized_data$swing_length), linetype = 'dotted') +
  geom_hline(yintercept = mean(summarized_data$bat_speed, na.rm = TRUE), linetype = 'dotted') +
  labs(title = 'Season K% Based on Bat Speed and Swing Length',
       y = 'Bat Speed (MPH)',
       x = 'Swing Length (ft)')


summarized_data %>% ggplot(aes(rotational_acceleration, k_pct)) +
  geom_point() +
  geom_smooth(method = 'lm')

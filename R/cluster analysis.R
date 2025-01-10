library(baseballr)
library(ggplot2)
library(dplyr)
library(patchwork)

batter_clusters <- competitive_swing_data %>%
  group_by(batter, batter_name, cluster) %>%
  summarize(appearances = n(), .groups = "drop") %>%
  group_by(batter, batter_name) %>%
  mutate(total_appearances = sum(appearances),
         percentage = (appearances / total_appearances) * 100) %>%
  ungroup() %>%
  select(batter, batter_name, cluster, appearances, percentage) %>%
  arrange(batter, cluster)

# Select the highest appearance for each player
players <- batter_clusters %>%
  group_by(batter, batter_name) %>%
  filter(appearances == max(appearances)) %>%
  ungroup()

players %>%
  group_by(cluster) %>%
  summarize(player_count = n_distinct(batter)) %>%
  arrange(cluster)

# get full stats
baseball_ref <- bref_daily_batter(t1 = "2024-03-28", t2 = "2024-09-30")

write_csv(baseball_ref, 'baseball_ref.csv')

players <- players %>%
  mutate(batter_name = str_replace(batter_name, "Luis Arraez", "Luis Arráez"))


player_stats <- players %>%
  left_join(baseball_ref, by = c("batter_name" = "Name"))


# common players
selected_batters <- c(
  "Jose Altuve", "Jeimer Candelario", "Cody Bellinger", "Matt Carpenter", "Isaac Paredes",
  "Andrew McCutchen", "Carlos Santana", "Paul Goldschmidt", "Travis d’Amaud", "Jason Heyward",
  "Salvador Perez", "Nolan Arenado", "Nick Castellanos", "Aaron Judge", "J.T. Realmuto",
  "Willy Adames", "Josh Naylor", "Jeremy Peña", "Adolis García", "Giancarlo Stanton",
  "Marcell Ozuna", "Mike Trout", "Christian Walker", "Manny Machado", "Brandon Nimmo",
  "Matt Olson", "Pete Alonso", "Michael Conforto", "Jorge Soler", "Tyler O’Neill",
  "Matt Chapman", "Kyle Schwarber", "Shohei Ohtani", "Ronald Acuña Jr.", "Jesús Sánchez",
  "William Contreras", "Austin Riley", "Fernando Tatis Jr.", "Vladimir Guerrero Jr.",
  "Juan Soto", "Oneil Cruz", "Yordan Alvarez", "Triston Casas", "Julio Rodríguez",
  "Wilyer Abreu", "Elly De La Cruz", "Gunnar Henderson", "Bryce Harper", "José Abreu",
  "Tommy Pham", "Corey Seager", "Carlos Correa", "Matt Olson", "Ian Happ", "Bryan Reynolds",
  "Luke Raley", "Seiya Suzuki", "Bobby Witt Jr.", "Francisco Alvarez", "Corbin Carroll",
  "Wyatt Langford", "Justin Turner", "Freddie Freeman", "Miguel Rojas", "Anthony Rizzo",
  "Mookie Betts", "Luis Arráez", "Emmanuel Rivera", "Andrés Giménez", "Brice Turang",
  "Josh Smith", "Ha-Seong Kim", "Steven Kwan", "Brendan Donovan", "Jacob Young",
  "Max Muncy", "Eddie Rosario", "Christian Yelich", "Xander Bogaerts", "Adam Duvall",
  "Francisco Lindor", "Max Kepler", "José Ramírez", "Alex Bregman", "Dansby Swanson",
  "Taylor Ward", "Anthony Santander", "Mitch Garver", "J.P. Crawford", "Rafael Devers",
  "Gleyber Torres", "Willi Castro", "Yandy Díaz", "Connor Wong", "Matt Vierling",
  "Alec Bohm", "Jazz Chisholm Jr.", "Bo Naylor", "Mark Vientos", "Adley Rutschman",
  "Austin Wells", "Jarren Duran", "Anthony Volpe", "Brenton Doyle", "Jackson Chourio",
  "Jackson Merrill", "Jackson Holliday", "Masataka Yoshida", "Charlie Blackmon",
  "DJ LeMahieu", "Marcus Semien", "Christian Vázquez", "Mark Canha", "Gio Urshela",
  "Jeff McNeil", "Ozzie Albies", "Oswaldo Cabrera", "Josh Rojas", "Will Smith",
  "Jake Meyers", "Bryson Stott", "Javier Báez", "Kyle Tucker", "Joey Gallo"
)

big_name_players <- player_stats %>%
  filter(batter_name %in% selected_batters)

big_name_players <- big_name_players %>%
  left_join(
    summarized_data %>% select(batter, bat_speed, swing_length, rotational_acceleration, time_to_contact),
    by = "batter"
  )


##############################################################################

# Players close to cluster 3
cluster_3_shortfalls <- batter_clusters %>%
  group_by(batter, batter_name) %>%
  mutate(rank = dense_rank(desc(appearances))) %>%
  filter(rank == 2 & cluster == 3) %>%
  ungroup()

# Bryce Harper, Aaron Judge, Anthony Santander, Bobby Witt Jr., Riley Greene, Jackson Chourio


players <- c('Bryce Harper', 'Aaron Judge', 'Anthony Santander', 
             'Bobby Witt Jr.', 'Riley Greene', 'Jackson Chourio')

plots <- list()

for (player in players) {
  p <- competitive_swing_data %>%
    filter(batter_name == player) %>%
    ggplot(aes(swing_length, bat_speed, color = as.factor(cluster))) +
    geom_jitter() +
    scale_color_manual(name = 'Cluster',
                       values = c('1' = 'indianred1', '2' = 'goldenrod',
                                  '3' = 'limegreen', '4' = 'aquamarine3',
                                  '5' = 'deepskyblue2', '6' = 'mediumpurple1',
                                  '7' = 'violet')) +
    geom_vline(xintercept = mean(summarized_data$swing_length), linetype = 'dotted') +
    geom_hline(yintercept = mean(summarized_data$bat_speed), linetype = 'dotted') +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 14), 
          axis.title = element_text(size = 12), 
          axis.text = element_text(size = 10), 
          legend.title = element_text(size = 11), 
          legend.text = element_text(size = 11)) +
    labs(title = player,
         x = "Swing Length",
         y = "Bat Speed")
  
  # Add the plot to the list
  plots[[player]] <- p
}


grid_plot <- (plots[[1]] | plots[[2]] | plots[[3]]) /
  (plots[[4]] | plots[[5]] | plots[[6]])

grid_plot

##############################################################################

# Bryce Harper
bryce_harper <- competitive_swing_data %>% 
  filter(batter_name == 'Bryce Harper') %>% 
  group_by(cluster) %>% 
  reframe(
    batter_name,
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    whiff_pct = mean(whiff)*100,
    blast_swing_pct = mean(blast_swing)*100,
    barrel_swing_pct = mean(ifelse(launch_speed_angle %in% 6, 1,0)*100),
    n = n()) %>% 
  distinct() %>% 
  slice_max(n, n = 2)

# Aaron Judge
aaron_judge <- competitive_swing_data %>% 
  filter(batter_name == 'Aaron Judge') %>% 
  group_by(cluster) %>% 
  reframe(
    batter_name,
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    whiff_pct = mean(whiff)*100,
    blast_swing_pct = mean(blast_swing)*100,
    barrel_swing_pct = mean(ifelse(launch_speed_angle %in% 6, 1,0)*100),
    n = n())%>% 
  distinct() %>% 
  slice_max(n, n = 2) 

#Anthony Santander
anthony_santander <- competitive_swing_data %>% 
  filter(batter_name == 'Anthony Santander') %>% 
  group_by(cluster) %>% 
  reframe(
    batter_name,
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    whiff_pct = mean(whiff)*100,
    blast_swing_pct = mean(blast_swing)*100,
    barrel_swing_pct = mean(ifelse(launch_speed_angle %in% 6, 1,0)*100),
    n = n())%>% 
  distinct() %>% 
  slice_max(n, n = 2) 

# Bobby Witt Jr
bobby_witt <- competitive_swing_data %>% 
  filter(batter_name == 'Bobby Witt Jr.') %>% 
  group_by(cluster) %>% 
  reframe(
    batter_name,
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    whiff_pct = mean(whiff)*100,
    blast_swing_pct = mean(blast_swing)*100,
    barrel_swing_pct = mean(ifelse(launch_speed_angle %in% 6, 1,0)*100),
    n = n())%>% 
  distinct() %>% 
  slice_max(n, n = 2) 

# Riley Greene
riley_greene <- competitive_swing_data %>% 
  filter(batter_name == 'Riley Greene') %>% 
  group_by(cluster) %>% 
  reframe(
    batter_name,
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    whiff_pct = mean(whiff)*100,
    blast_swing_pct = mean(blast_swing)*100,
    barrel_swing_pct = mean(ifelse(launch_speed_angle %in% 6, 1,0)*100),
    n = n())%>% 
  distinct() %>% 
  slice_max(n, n = 2) 

# Jackson Chourio
jackson_chourio <- competitive_swing_data %>% 
  filter(batter_name == 'Jackson Chourio') %>% 
  group_by(cluster) %>% 
  reframe(
    batter_name,
    wOBA = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    whiff_pct = mean(whiff)*100,
    blast_swing_pct = mean(blast_swing)*100,
    barrel_swing_pct = mean(ifelse(launch_speed_angle %in% 6, 1,0)*100),
    n = n())%>% 
  distinct() %>% 
  slice_max(n, n = 2) 

player_comparisons <- bind_rows(bryce_harper, aaron_judge, anthony_santander,
          bobby_witt, riley_greene, jackson_chourio)

write_csv(player_comparisons, 'player_comparisons.csv')

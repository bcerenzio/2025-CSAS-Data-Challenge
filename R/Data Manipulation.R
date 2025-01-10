library(baseballr)
library(tidyverse)
library(colorspace)
library(ggridges)


data <- read_csv('statcast_pitch_swing_data_20240402_20241030_with_arm_angle.csv')

#removing overlapping or unuseful columns
data <- data %>% 
  select(-spin_dir, -spin_rate_deprecated, -break_angle_deprecated, -break_length_deprecated,
         -tfs_deprecated, -tfs_zulu_deprecated)



#adding runners on column 
data1 <- data %>% mutate(
  runners_on_base = ifelse((!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),TRUE, FALSE)
) %>% 
  relocate(runners_on_base, .after = on_1b)

# adding in categorical variables for innings
data1 <- data1 %>% 
  mutate(binned_innings = case_when(
    between(inning, 1,5) ~ '1-5',
    between(inning, 6,8) ~ '6-8',
    inning >= 9 ~ '9+'
  ))
# density plot of swing length
data1 %>% ggplot(aes(x = swing_length, y = runners_on_base, fill = runners_on_base)) +
  geom_density_ridges(scale = 100, alpha = 0.6)


data1 %>% ggplot(aes(x = bat_speed, y = runners_on_base, fill = runners_on_base)) +
  geom_density_ridges(scale = 100, alpha = 0.6)


data1 %>% ggplot(aes(runners_on_base, bat_speed)) +
  geom_boxplot() +
  theme_bw()

# writing statcast data to parquet
arrow::write_parquet(data1, 'statcast_2024.parquet')



#getting additional mlb game data
game_ids <- unique(data$game_pk)

#testing
get_pbp_mlb(game_ids[1])

pbp_data <- map_df(game_ids, function(game_id){
  print(which(game_ids == game_id)); get_pbp_mlb(game_id)})

#removes non-pitch events (Mound Visits, Pitching Changes, etc)
pbp_data_pitches <- pbp_data %>% filter(isPitch)
write_csv(pbp_data, 'mlb_pbp.csv')
colnames(pbp_data)
colnames(data)
#converting top_bottom columns in play-by-play to statcast inputs for joining
pbp_data_pitches <- pbp_data_pitches %>% 
  mutate(about.halfInning = case_when(about.halfInning == "top" ~ "Top",
                                      about.halfInning == 'bottom' ~ 'Bot'))

pbp_data_pitches1 <-  pbp_data_pitches %>% 
  mutate(last.pitch.of.ab = case_when(last.pitch.of.ab == 'true' ~ TRUE,
                                      last.pitch.of.ab == 'false' ~ FALSE,
                                      .default = NA)) %>% 
  select(-starts_with('reviewDetails'), -endTime, -index, -type,
         -details.description, -details.event, -details.homeScore, -details.awayScore,
         -details.isScoringPlay, -details.hasReview,details.ballColor, -player.id, -player.link,
         -pitchData.strikeZoneBottom, -pitchData.strikeZoneTop,-details.fromCatcher,
         -hitData.coordinates.coordX, -hitData.coordinates.coordY, -actionPlayId,
         -details.eventType, -position.code, -position.name, -position.type,
         -position.abbreviation, -battingOrder, -result.type,
         -about.startTime, -about.endTime, -about.isComplete, -about.captivatingIndex,
         -matchup.batter.link, -matchup.batSide.description,-matchup.pitcher.link,
         -matchup.pitchHand.description, -batted.ball.result, -home_level_id,
         -home_level_name, -home_parentOrg_id, -home_parentOrg_name, -home_league_id,
         -home_league_name, -away_level_id, -away_level_name, -away_parentOrg_name,
         -away_parentOrg_id, -away_league_id, -details.trailColor, -pfxId,
         -pitchData.coordinates.pfxX, -pitchData.coordinates.pfxZ, -injuryType,
         -umpire.id, -umpire.link, -starts_with('details.violation'),
         -isBaseRunningPlay, -isSubstitution, -replacedPlayer.id, -replacedPlayer.link,
         -about.isTopInning, -matchup.postOnFirst.link,  -matchup.postOnSecond.link, 
         -matchup.postOnThird.link, -away_league_name, -base, -pitchData.breaks.breakVertical,
         -details.ballColor)

arrow::write_parquet(pbp_data_pitches1, 'pbp_data_pitches1.parquet')





#### Final Data ####
data <- data %>% group_by(game_pk) %>% 
  arrange(at_bat_number, pitch_number, inning,
          balls, strikes) %>% 
  mutate(game_pitch_id = row_number()) %>%
  ungroup() %>% 
  arrange(game_pk) %>% 
  relocate(game_pitch_id, .before = balls) %>% 
  relocate(pitch_number, .after = game_pitch_id)


pbp_data_pitches1 <- pbp_data_pitches1 %>% group_by(game_pk) %>% 
  arrange(about.atBatIndex, startTime) %>% 
  mutate(game_pitch_id = row_number()) %>%
  ungroup() %>% 
  arrange(game_pk)


final_data <- left_join(data, pbp_data_pitches1, by = c('game_pk',"game_pitch_id"), suffix = c('_statcast', '_mlb'))

# fixing data errors in batter and pitcher ids
# noticed batter names did not match play description
final_data <- final_data %>% 
  mutate(batter = matchup.batter.id,
         pitcher = matchup.pitcher.id) %>% 
  select(-matchup.batter.id, -matchup.pitcher.id, -player_name) %>% 
  relocate(matchup.batter.fullName, .before = des) %>% 
  relocate(matchup.pitcher.fullName, .after = matchup.batter.fullName) %>% 
  rename('batter_name' = 'matchup.batter.fullName', 'pitcher_name' = 'matchup.pitcher.fullName')

# only including datapoints which have bat_speed & swing_length data
swing_data <- final_data %>% 
  filter(!is.na(bat_speed), !is.na(swing_length)) %>% 
  filter(description != 'ball')

#writing to parquet file
arrow::write_parquet(final_data, 'final_data.parquet')
arrow::write_parquet(swing_data, 'swing_data.parquet')

#finding contact plays
swing_data <- swing_data %>% 
  mutate(contact = ifelse(description %in% c('foul', 'hit_into_play','foul_bunt', 'foul_tip',
                                             'bunt_foul_tip'), 1,0),
         whiff = ifelse(description %in% c('swinging_strike', 'swinging_strike_blocked',
                                           'missed_bunt'), 1,0))

# finding chase rates (excluding bunts)
final_data <- final_data %>% 
  mutate(chase = ifelse((!between(plate_x, -0.83,0.83) |
                           !between(plate_z, sz_bot, sz_top)) & description %in% c(
                             'foul', 'hit_into_play', 'swinging_strike', 'swinging_strike_blocked',
                             'foul_tip'),1,0),
         ball_out = ifelse((!between(plate_x, -0.83,0.83) | !between(plate_z, sz_bot, sz_top)) &
                             !(description %in% c('foul_bunt', 'bunt_foul_tip', 'missed_bunt')), 1,0))

# finding competitve swings for each player
swing_data <- swing_data %>% 
  group_by(batter) %>% 
  mutate(competitive_swing = ifelse(bat_speed > quantile(bat_speed, probs = c(0.1)) | bat_speed > 60 & launch_speed > 90,1,0)) %>% 
  ungroup() %>% 
  relocate(competitive_swing,.after = swing_length)

## Calculating rotational acceleration data
# Define conversion factors
MPH_TO_FTPS <- 1.46667  # 1 mph = 1.46667 ft/s

# Recalculate rotational acceleration and time to contact without dividing swing_length by 12
swing_data <- swing_data %>%
  mutate(
    bat_speed_ftps = bat_speed * MPH_TO_FTPS,    # Convert bat speed to ft/s
    swing_length_ft = swing_length,             # Use as is (assuming feet)
    rotational_acceleration = (bat_speed_ftps^2) / (2 * swing_length_ft),  # in ft/s²
    time_to_contact = (2 * swing_length_ft) / bat_speed_ftps                # in seconds
  ) %>% #converting rotational acceleration to g-force (blast motion standard)
  mutate(rotational_acceleration = rotational_acceleration/32.17404855643) %>% 
  relocate(rotational_acceleration, .after = 'swing_length') %>% 
  relocate(time_to_contact, .after = rotational_acceleration)


#calculating squared up rate
collision_efficiency <- 0.23

swing_data <- swing_data %>% 
  mutate(max_possible_exitvelo = ifelse(!is.na(launch_speed) & description %in% c('hit_into_play'), 
                                        (bat_speed * (1+collision_efficiency)) + (release_speed * collision_efficiency), NA)) %>% 
  relocate(max_possible_exitvelo, .after = launch_speed)

swing_data <- swing_data %>% 
  mutate(
    squared_up_swing = ifelse((competitive_swing == 1 & is.na(max_possible_exitvelo)) | ((launch_speed/max_possible_exitvelo) < 0.8 & competitive_swing == 1), 0,
                              ifelse((launch_speed/max_possible_exitvelo) > 0.8 & competitive_swing == 1, 1,NA)),
    squared_up_contact = case_when(
      (launch_speed/max_possible_exitvelo) > 0.8 & competitive_swing == 1 ~ 1,
      (launch_speed/max_possible_exitvelo) < 0.8 | description %in% c('foul', 'foul_tip') ~ 0,
      .default = NA
    )) %>% 
  relocate(squared_up_swing, .after = max_possible_exitvelo) %>% 
  relocate(squared_up_contact, .after = squared_up_swing)

# calculating blast rate
swing_data <- swing_data %>% 
  mutate(
    blast_swing = ifelse(competitive_swing == 1 & is.na(max_possible_exitvelo) | (launch_speed/max_possible_exitvelo)*100 + bat_speed < 164 & competitive_swing == 1, 0,
                         ifelse((launch_speed/max_possible_exitvelo)*100 + bat_speed > 164 & competitive_swing == 1, 1, NA)),
    blast_contact =
      case_when(
        (launch_speed/max_possible_exitvelo)*100 + bat_speed > 164 & competitive_swing == 1 ~ 1,
        (launch_speed/max_possible_exitvelo)*100 + bat_speed < 164 | description %in% c('foul', 'foul_tip') ~ 0,
        .default = NA
      )) %>% 
  relocate(blast_swing, .after = squared_up_contact) %>% 
  relocate(blast_contact, .after = blast_swing)

# season stats (on swings)
summarized_data <- swing_data %>% 
  mutate(compet_bat_speed = ifelse(competitive_swing == 1, bat_speed, NA),
         compet_swing_length = ifelse(competitive_swing == 1, swing_length, NA),
         compet_rotational_acceleration = ifelse(competitive_swing == 1, rotational_acceleration, NA),
         compet_time_to_contact = ifelse(competitive_swing == 1, time_to_contact, NA)) %>% 
  group_by(batter) %>% 
  reframe(
          contact_pct = mean(contact)*100,
          whiff_pct = mean(whiff)*100,
          bat_speed = mean(compet_bat_speed, na.rm  = TRUE),
          swing_length = mean(compet_swing_length, na.rm = TRUE),
          rotational_acceleration = mean(compet_rotational_acceleration, na.rm = TRUE),
          time_to_contact = mean(compet_time_to_contact, na.rm = TRUE),
          squared_up_swing_pct = mean(squared_up_swing, na.rm = TRUE)*100,
          squared_up_contact_pct = mean(squared_up_contact, na.rm = TRUE)*100,
          blast_swing_pct = mean(blast_swing, na.rm = TRUE)*100,
          blast_contact_pct = mean(blast_contact, na.rm = TRUE)*100,
          pitches = n(),
          batter_name
          ) %>% 
  distinct() %>% 
  filter(pitches >= 100)

chase_data <- final_data %>% 
  group_by(batter) %>% 
  reframe(chase_pct = sum(chase, na.rm = TRUE)/sum(ball_out, na.rm= TRUE)) %>% 
  distinct()

summarized_data <- summarized_data %>% 
  left_join(chase_data, by = 'batter')

# swing data by on base situation
on_base_season_stats <- swing_data %>% 
  filter(competitive_swing == 1) %>% 
  group_by(batter, matchup.splits.menOnBase) %>% 
  reframe(batter_name = batter_name,
          bat_speed_sit = mean(bat_speed, na.rm = TRUE),
          swing_length_sit = mean(swing_length, na.rm = TRUE),
          rotational_acceleration_sit = mean(rotational_acceleration, na.rm = TRUE),
          time_to_contact_sit = mean(time_to_contact, na.rm = TRUE)) %>% 
  distinct() %>% 
  left_join(select(summarized_data, bat_speed, swing_length, batter, rotational_acceleration, time_to_contact), by = 'batter')  %>% 
  drop_na()

on_base_season_stats <- on_base_season_stats %>% 
  mutate(bat_speed_rel_avg = bat_speed_sit - bat_speed,
         swing_length_rel_avg = swing_length_sit - swing_length,
         rotational_acceleration_rel_avg = rotational_acceleration_sit - rotational_acceleration,
         time_to_contact_rel_avg = time_to_contact_sit - time_to_contact
  )


# Swing Data based on Outs
outs_season_stats <- swing_data %>% 
  filter(competitive_swing == 1) %>% 
  group_by(batter, outs_when_up) %>% 
  reframe(batter_name = batter_name,
          bat_speed_sit = mean(bat_speed, na.rm = TRUE),
          swing_length_sit = mean(swing_length, na.rm = TRUE),
          rotational_acceleration_sit = mean(rotational_acceleration, na.rm = TRUE),
          time_to_contact_sit = mean(time_to_contact, na.rm = TRUE)) %>% 
  distinct() %>% 
  left_join(select(summarized_data, bat_speed, swing_length, batter, rotational_acceleration, time_to_contact), by = 'batter')  %>% 
  drop_na()

outs_season_stats <- outs_season_stats %>% 
  mutate(bat_speed_rel_avg = bat_speed_sit - bat_speed,
         swing_length_rel_avg = swing_length_sit - swing_length,
         rotational_acceleration_rel_avg = rotational_acceleration_sit - rotational_acceleration,
         time_to_contact_rel_avg = time_to_contact_sit - time_to_contact
  )

# Swing Data based on Strikes
strikes_season_stats <- swing_data %>% 
  filter(competitive_swing == 1) %>% 
  group_by(batter, strikes) %>% 
  reframe(batter_name = batter_name,
          bat_speed_sit = mean(bat_speed, na.rm = TRUE),
          swing_length_sit = mean(swing_length, na.rm = TRUE),
          rotational_acceleration_sit = mean(rotational_acceleration, na.rm = TRUE),
          time_to_contact_sit = mean(time_to_contact, na.rm = TRUE)) %>% 
  distinct() %>% 
  left_join(select(summarized_data, bat_speed, swing_length, batter, rotational_acceleration, time_to_contact), by = 'batter')  %>% 
  drop_na()

strikes_season_stats <- strikes_season_stats %>% 
  mutate(bat_speed_rel_avg = bat_speed_sit - bat_speed,
         swing_length_rel_avg = swing_length_sit - swing_length,
         rotational_acceleration_rel_avg = rotational_acceleration_sit - rotational_acceleration,
         time_to_contact_rel_avg = time_to_contact_sit - time_to_contact
  )

# finding count
final_data <- final_data %>% 
  mutate(pitch_count = str_c(balls,'-',strikes)) %>% 
  relocate(pitch_count, .after = strikes)

swing_data <- swing_data %>% 
  mutate(pitch_count = str_c(balls,'-',strikes)) %>% 
  relocate(pitch_count, .after = strikes)


# finding AB & hits

ab <- c('force_out', 'field_out', 'single','double', 'home_run',
        'field_error','strikeout','grounded_into_double_play',
        'triple','double_play','fielders_choice','fielders_choice_out','strikeout_double_play',
        'triple_play')

final_data <- final_data %>% 
  mutate(AB = ifelse(events %in% ab,1, 0))
swing_data <- swing_data %>% 
  mutate(AB = ifelse(events %in% ab,1, 0))

final_data <- final_data %>% 
  mutate(hit = case_when(
    AB == 1 & events %in% c('single','double', 'home_run','triple') ~ 1,
    AB == 1 & !(events %in% c('single','double', 'home_run','triple')) ~ 0,
    AB == 0 ~ NA
  ))

swing_data <- swing_data %>% 
  mutate(hit_contact = case_when(
    AB == 1 & events %in% c('single','double', 'home_run','triple') ~ 1,
    AB == 1 & !(events %in% c('single','double', 'home_run','triple')) ~ 0,
    AB == 0 ~ NA
  ),
  hit_swing = ifelse(events %in% c('single','double', 'home_run','triple'), 1,0))

#adding K%
final_data <- final_data %>% 
  mutate(k_pct = case_when(
    events %in% c('strikeout','strikeout_double_play') ~ 1,
    is.na(events) | events == 'truncated_pa' ~ NA,
    .default = 0
  ))


# season stats
season_statline <- final_data %>% 
  group_by(batter) %>% 
  reframe(
    woba = sum(woba_value, na.rm = TRUE)/sum(woba_denom, na.rm = TRUE),
    BA = mean(hit, na.rm = TRUE),
    k_pct = mean(k_pct, na.rm = TRUE)
  )

summarized_data <- summarized_data %>% 
  left_join(season_statline, by = 'batter')

## adding base_out scenarios
swing_data <- swing_data %>% 
  mutate(base_sit = case_when(
    is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ '___',
    !is.na(on_1b) & is.na(on_2b) & is.na(on_3b) ~ '__1',
    is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ '_2_',
    is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ '3__',
    !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b) ~ '_21',
    is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ '32_',
    !is.na(on_1b) & is.na(on_2b) & !is.na(on_3b) ~ '3_1',
    !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b) ~ '321',
    .default = NA, # check to see if we're missing any base situations
  )) %>% 
  relocate(base_sit, .after = 'on_3b')

table(swing_data$base_sit) # no NAs


competitive_swing_data <- swing_data %>% 
  filter(competitive_swing == 1)

arrow::write_parquet(final_data, 'final_data.parquet')
arrow::write_parquet(swing_data, 'swing_data.parquet')
arrow::write_parquet(summarized_data, 'summarized_data.parquet')
arrow::write_parquet(on_base_season_stats, 'on_base_stats.parquet')
arrow::write_parquet(strikes_season_stats, 'strikes_season_stats.parquet')
arrow::write_parquet(outs_season_stats, 'outs_season_stats.parquet')
arrow::write_parquet(competitive_swing_data, 'competitive_swing_data.parquet')


library(tidyverse)
library(xgboost)
library(SHAPforxgboost)
library(fastDummies)
library(rsample)
library(Metrics)


#finding mean delta_bat_win_exp
competitive_swing_data <- competitive_swing_data %>% 
  mutate(delta_bat_win_exp = ifelse(inning_topbot == 'Top', delta_home_win_exp*-1,delta_home_win_exp))

delta_win_exp_model_df <- competitive_swing_data %>% 
  reframe(
    delta_bat_win_exp = ifelse(is.na(events), 0, delta_bat_win_exp), #ball-strike changes don't change delta_win_expenctancy
    # forcing it to 0 ignores any stolen bases or other events that may be affecting the 
    # delta_win_exp besides the pitcher-batter interaction
    cluster = as.factor(cluster),
    base_sit = as.factor(base_sit),
    inning = inning,
    bat_score_diff,
    balls,
    strikes,
    outs_when_up,
  ) %>% 
  filter(!is.na(delta_bat_win_exp)) %>% 
  filter(balls != 4)

#splitting into train & test data
set.seed(123);split <- initial_split(delta_win_exp_model_df, strata = delta_bat_win_exp)

train_data <- training(split)
test_data <- testing(split)

remove(split)

#### Swing XGBoost Model ####
train_x_win <- train_data %>% dplyr::select(-delta_bat_win_exp)
train_x_win <- train_x_win %>% dummy_columns(select_columns = c('cluster','base_sit'),remove_selected_columns = TRUE)

test_x_win <- test_data %>% dplyr::select(-delta_bat_win_exp)
test_x_win <- test_x_win %>% dummy_columns(select_columns = c('cluster','base_sit'), remove_selected_columns = TRUE)

dtrain_win <- xgb.DMatrix(data = as.matrix(train_x_win), label = train_data$delta_bat_win_exp)

num_cores <- parallel::detectCores() -1

#finding gamma value that prevents overfitting the best
find_regularization <- function(gamma_win, lambda_win, alpha_win){
  print(paste('Gamma:',gamma_win, 'Lambda:', lambda_win, 'Alpha:', alpha_win))
  set.seed(123);xgb.cv(
    params = list(
      eta = 0.3,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win
    ),
    data = dtrain_win,
    stratified = TRUE,
    nrounds = 600,
    print_every_n = 10,
    early_stopping_rounds = 20,
    nfold = 5,
    nthread = num_cores,
  )$evaluation_log %>% 
    dplyr::select(test_rmse_mean) %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    dplyr::slice(1) %>% #safeguard
    flatten_dbl()
}

regularzation_run_df <- expand_grid(
  gamma = seq(0,4, by = 2),
  lambda = seq(0,20,by = 10),
  alpha = seq(0,20, by = 10)
)

regularzation_run_df <- regularzation_run_df %>% 
  mutate(rmse = pmap_dbl(list(gamma, lambda, alpha), find_regularization))

gamma_win <- regularzation_run_df %>% 
  slice_min(rmse) %>% 
  pull(gamma) #0

lambda_win <- regularzation_run_df %>% 
  slice_min(rmse) %>% 
  pull(lambda) #10

alpha_win <- regularzation_run_df %>% 
  slice_min(rmse, n = 1) %>%
  pull(alpha)#10

current_rmse_win <- regularzation_run_df %>% 
  slice_min(rmse) %>% 
  pull(rmse) #0.0389


#finding optmial subsample
find_subsample_win <- function(subsample_win){
  print(paste('Subsample:',round(subsample_win,3), sep = ' '))
  set.seed(123);xgb.cv(
    params = list(
      eta = 0.3,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win,
      subsample = subsample_win
    ),
    data = dtrain_win,
    stratified = TRUE,
    nrounds = 600,
    print_every_n = 10,
    early_stopping_rounds = 20,
    nfold = 5,
    nthread = num_cores,
  )$evaluation_log %>% 
    dplyr::select(test_rmse_mean) %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    flatten_dbl()
}

(subsample_win_obj <- optimize(find_subsample_win, c(0.5,1), tol = 0.01)) 

if(subsample_win_obj$objective < current_rmse_win){
  subsample_win <- subsample_win_obj$minimum # 0.863
  current_rmse_win <- subsample_win_obj$objective # 0.0568
} else{
  subsample_win <- 1 #default
}

find_by_tree <- function(by_tree_win){
  print(paste('By Tree:', round(by_tree_win,3)))
  set.seed(123);xgb.cv(
    params = list(
      eta = 0.3,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win,
      subsample = subsample_win,
      colsample_bytree = by_tree_win
    ),
    data = dtrain_win,
    nrounds = 1200,
    print_every_n = 10,
    stratified = TRUE,
    early_stopping_rounds = 20,
    nfold = 5,
    nthread = num_cores,
  )$evaluation_log %>% 
    dplyr::select(test_rmse_mean) %>% 
    slice_min(test_rmse_mean, n = 1) %>%
    dplyr::slice(1) %>% 
    flatten_dbl()
}

by_tree_obj <- optimize(find_by_tree, c(0,1), tol = 0.01)

if(by_tree_obj$objective < current_rmse_win){
  by_tree_win <- by_tree_obj$minimum 
  current_rmse_win <- by_tree_obj$objective 
} else{
  by_tree_win <- 1 #default
}


find_by_level <- function(by_level_win){
  print(paste('By Level:', round(by_level_win,3)))
  set.seed(123);xgb.cv(
    params = list(
      eta = 0.3,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win,
      subsample = subsample_win,
      colsample_bytree = by_tree_win,
      colsample_bylevel = by_level_win
    ),
    data = dtrain_win,
    nrounds = 1200,
    print_every_n = 10,
    stratified = TRUE,
    early_stopping_rounds = 20,
    nfold = 5,
    nthread = num_cores,
  )$evaluation_log %>% 
    dplyr::select(test_rmse_mean) %>% 
    slice_min(test_rmse_mean, n = 1) %>%
    dplyr::slice(1) %>% 
    flatten_dbl()
}

by_level_obj <- optimize(find_by_level, c(0,1), tol = 0.01)

if(by_level_obj$objective < current_rmse_win){
  by_level_win <- by_level_obj$minimum 
  current_rmse_win <- by_level_obj$objective 
} else{
  by_level_win <- 1 #default
}


find_by_node <- function(by_node_win){
  print(paste('By Node:', round(by_node_win,3)))
  set.seed(123);xgb.cv(
    params = list(
      eta = 0.3,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win,
      subsample = subsample_win,
      colsample_bytree = by_tree_win,
      colsample_bylevel = by_level_win,
      colsample_bynode = by_node_win
    ),
    data = dtrain_win,
    nrounds = 1200,
    print_every_n = 10,
    stratified = TRUE,
    early_stopping_rounds = 20,
    nfold = 5,
    nthread = num_cores,
  )$evaluation_log %>% 
    dplyr::select(test_rmse_mean) %>% 
    slice_min(test_rmse_mean, n = 1) %>%
    dplyr::slice(1) %>% 
    flatten_dbl()
}

by_node_obj <- optimize(find_by_node, c(0,1), tol = 0.01)

if(by_node_obj$objective < current_rmse_win){
  by_node_win <- by_node_obj$minimum  
  current_rmse_win <- by_node_obj$objective 
} else{
  by_node_win <- 1 #default
}


#finding child weight
find_depth_weight_win <- function(depth_win, weight_win){
  print(paste('Depth:', depth_win, 'Weight:', weight_win, sep = ' '))
  set.seed(123);xgb.cv(
    params = list(
      eta = 0.3,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win,
      subsample = subsample_win,
      colsample_bytree = by_tree_win,
      colsample_bylevel = by_level_win,
      colsample_bynode = by_node_win,
      min_child_weight = weight_win,
      max_depth = depth_win
    ),
    data = dtrain_win,
    nrounds = 600,
    print_every_n = 10,
    early_stopping_rounds = 20,
    nfold = 5,
    nthread = num_cores,
  )$evaluation_log %>% 
    dplyr::select(test_rmse_mean) %>% 
    slice_min(test_rmse_mean, n = 1) %>% 
    flatten_dbl()
}

depth_weight_df <- expand_grid(
  depth = c(3,6,10,20),
  weight = c(1,5,10)
) %>% 
  mutate(rmse = map2_dbl(depth, weight, find_depth_weight_win))

depth_win <- depth_weight_df %>% slice_min(rmse) %>% pull(depth) # 6
weight_win <- depth_weight_df %>% slice_min(rmse) %>% pull(weight) #1

#becoming more exact
depth_weight_df <- expand_grid(
  depth = 5:8,
  weight = 1:3
) %>% 
  mutate(rmse = map2_dbl(depth, weight, find_depth_weight_win))

depth_win <- depth_weight_df %>% slice_min(rmse) %>% pull(depth) # 7
weight_win <- depth_weight_df %>% slice_min(rmse) %>% pull(weight) #3

dtest_win <- xgb.DMatrix(data = as.matrix(test_x_win), label = test_data$delta_bat_win_exp)


#finding optimal nrounds and eta combination
find_rounds_rate <- function(eta_win, rounds_win, early_stopping_rounds_win){
  print(paste('Eta:',eta_win,'Rounds:', rounds_win, 'Early Stopping Rounds:', early_stopping_rounds_win, sep = ' '))
  # using xgb.train instead of xgb.cv because eta and nrounds don't
  # typically have an extreme impact on model accuracy
  # and xgb.train runs much faster than xgb.cv, especially when rounds
  # get increasingly high
  set.seed(123);xgb.train(
    params = list(
      eta = eta_win,
      objective = "reg:squarederror",
      eval_metric = "rmse",
      gamma = gamma_win,
      lambda = lambda_win,
      alpha = alpha_win,
      subsample = subsample_win,
      colsample_bytree = by_tree_win,
      colsample_bylevel = by_level_win,
      colsample_bynode = by_node_win,
      min_child_weight = weight_win,
      max_depth = depth_win
    ),
    data = dtrain_win,
    nrounds = rounds_win,
    print_every_n = 50,
    early_stopping_rounds = early_stopping_rounds_win,
    watchlist = list(train = dtrain_win, test = dtest_win), 
    nthread = num_cores,
  )$best_score
}



rounds_eta_df <- tibble(
  nrounds = 600 * 10 *(1:5),
  eta = 0.3/(10*(1:5)),
  early_stopping_rounds = 200*1:5,
) %>% mutate(
  rmse = pmap_dbl(list(eta, nrounds, early_stopping_rounds),find_rounds_rate)
) #rmse = 0.03857


rounds_win <- rounds_eta_df %>% 
  slice_min(rmse) %>% 
  pull(nrounds) #24000

eta_win <- rounds_eta_df %>% 
  slice_min(rmse) %>% 
  pull(eta) # 0.0075

early_stopping_rounds_win <-  rounds_eta_df %>% 
  slice_min(rmse) %>% 
  pull(early_stopping_rounds) #800


#getting nrounds
set.seed(123);xg_model <- xgb.train(
  params = list(
    eta = eta_win,
    objective = "reg:squarederror",
    eval_metric = "rmse",
    gamma = gamma_win,
    lambda = lambda_win,
    alpha = alpha_win,
    subsample = subsample_win,
    colsample_bytree = by_tree_win,
    colsample_bylevel = by_level_win,
    colsample_bynode = by_node_win,
    min_child_weight = weight_win,
    max_depth = depth_win
  ),
  data = dtrain_win,
  nrounds = rounds_win,
  print_every_n = 50,
  watchlist = list(train = dtrain_win, test = dtest_win),
  early_stopping_rounds = early_stopping_rounds_win,
  nthread = num_cores)

(rounds_win <- xg_model$best_iteration) #17784

remove(xg_model)

delta_win_exp_model_df_x <- delta_win_exp_model_df %>% dummy_cols(select_columns = c('cluster','base_sit'),
                                                  remove_selected_columns = TRUE) %>% select(-delta_bat_win_exp)

delta_win_exp_matrix <- xgb.DMatrix(as.matrix(delta_win_exp_model_df_x), label = delta_win_exp_model_df$delta_bat_win_exp)

set.seed(123);delta_win_exp_xg_model <- xgboost(
  params = list(
    eta = eta_win,
    objective = "reg:squarederror",
    eval_metric = "rmse",
    gamma = gamma_win,
    lambda = lambda_win,
    alpha = alpha_win,
    subsample = subsample_win,
    colsample_bytree = by_tree_win,
    colsample_bylevel = by_level_win,
    colsample_bynode = by_node_win,
    min_child_weight = weight_win,
    max_depth = depth_win
  ),
  data = delta_win_exp_matrix,
  nrounds = rounds_win,
  print_every_n = 50,
  early_stopping_rounds = early_stopping_rounds_win,
  nthread = num_cores)

delta_win_exp_df <- expand_grid(
  cluster = 1:7,
  inning = 1:10,
  bat_score_diff = -4:4,
  balls = 0:3,
  strikes = 0:2,
  outs_when_up = 0:2,
  base_sit = unique(competitive_swing_data$base_sit)
) 


delta_win_exp_df$delta_bat_win_exp <- (predict(delta_win_exp_xg_model, newdata = delta_win_exp_df %>% 
                                                dummy_cols(select_columns = c('cluster','base_sit'),
                                                           remove_selected_columns = TRUE) %>% as.matrix()))*100

#overall delta_win
overall_by_outs <- delta_win_exp_df %>% 
  group_by(cluster, outs_when_up, base_sit) %>% 
  reframe(delta_bat_win_exp = mean(delta_bat_win_exp)) %>% distinct() %>% 
  arrange(base_sit, outs_when_up)


write_csv(overall_by_outs, 'overall_by_outs.csv')


#filtering for 7th inning or later down by 1 or tied
inning7orlater_within1 <- delta_win_exp_df %>% 
  filter(inning >= 7, between(bat_score_diff,-1.01,1.01)) %>% 
  arrange(outs_when_up, balls, strikes, base_sit) %>% 
  group_by(cluster, base_sit, outs_when_up) %>% 
  reframe(delta_bat_win_exp = mean(delta_bat_win_exp)) %>% 
  distinct() %>% 
  arrange(base_sit, outs_when_up)


write_csv(inning7orlater_within1, 'inning7orlater_within1.csv')

# filtering for 0 to 1 strike counts
inning7orlater_within1_less2strikes <- delta_win_exp_df %>% 
  filter(inning >= 7, between(bat_score_diff,-1.01,1.01), strikes != 2) %>% 
  group_by(cluster, base_sit, outs_when_up) %>% 
  reframe(delta_bat_win_exp = mean(delta_bat_win_exp)) %>% 
  distinct() %>% 
  arrange(base_sit, outs_when_up)



write_csv(inning7orlater_within1_less2strikes, 'inning7orlater_within1_less2strikes.csv')

# filtering for 2 strike counts
inning7orlater_within1_2strikes <- delta_win_exp_df %>% 
  filter(inning >= 7, between(bat_score_diff,-1.01,1.01),strikes == 2) %>% 
  group_by(cluster, base_sit, outs_when_up) %>% 
  reframe(delta_bat_win_exp = mean(delta_bat_win_exp)) %>% 
  distinct() %>% 
  arrange(base_sit, outs_when_up)

write_csv(inning7orlater_within1_2strikes, 'inning7orlater_within1_2strikes.csv')



xgb.ggplot.importance(xgb.importance(model = delta_win_exp_xg_model),
                      xlab = 'Importance') +
  ggtitle('Win Probability Added\nFeature Importance') +
  guides(fill = 'none') +
  theme_bw()
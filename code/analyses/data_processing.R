
#'
#' Pre-processing script for behavioral and model data
#'


# INIT ----
rm(list = ls())
library(reticulate) # for evaluating python lists in the columns
library(tidyverse)



# GLOBALS ----

DATA_DIR = '../../data/experiment_data_unprocessed'
MODEL_DIR = '../../data/model_predictions_unprocessed'
OUTPUT_DIR = '../../data/data_processed'

SESSION_DATA = 'session_data_fullDataCogSci2024.csv'
PRACTICE_TRIALS = 'practice_trial_data_fullDataCogSci2024.csv'
EVAL_TRIALS = 'judgment_trial_data_fullDataCogSci2024.csv'
PARAM_FIT_MODEL = 'cf_model_default_samples_1000_berry_noise_0.112_step_noise_0.03_softmax_0.59.csv' # model simulations using parameter fits
HEURISTIC_MODEL = 'heuristic_model_trial_summary.csv'


# READ + PROCESS DATA ----

# Read in data
session_data = read_csv(file.path(DATA_DIR, SESSION_DATA))
practice_trials = read_csv(file.path(DATA_DIR, PRACTICE_TRIALS))
eval_trials = read_csv(file.path(DATA_DIR, EVAL_TRIALS))
param_fit_model_predictions = read_csv(file.path(MODEL_DIR, PARAM_FIT_MODEL))
heuristic_model_predictions = read_csv(file.path(MODEL_DIR, HEURISTIC_MODEL))


# Format summary df for each participant
session_data = session_data %>%
  mutate(
    # NB: these columns are in ms
    total_experiment_time = (end_experiment_ts - start_instruction_ts) / (1000 * 60),
    active_experiment_time = (practice_procedure_time_spent + evaluation_procedure_time_spent) / (1000 * 60)
  )
glimpse(session_data)


# Format practice trial vector columns
# NB: when running this, watch for py_eval prompt (type 'no')
practice_trials = practice_trials %>%
  rowwise() %>%
  mutate(
    predicted_path_accuracy = predicted_path_num_overlapping_trees / agent_path_num_trees,
    # `py_eval` processing of python list columns to convert to R lists
    agent_start_position_list = list(py_eval(agent_start_position)),
    tree_reward_list = list(py_eval(tree_rewards)),
    agent_path_tree_reward_list = list(py_eval(agent_path_tree_rewards)),
    predicted_path_tree_reward_list = list(py_eval(predicted_path_tree_rewards)),
    tree_visibility_list = list(py_eval(tree_visibility)),
    agent_path_tree_visibility_list = list(py_eval(agent_path_tree_visibility)),
    predicted_path_tree_visibility_list = list(py_eval(predicted_path_tree_visibility)),
    # nested lists: each position is an array c(x, y) nested in a list
    tree_position_list = list(lapply(as.list(py_eval(tree_positions)), as.list)),
    tree_position_list = list(lapply(as.array(py_eval(tree_positions)), as.array)),
    agent_path_tree_position_list = list(lapply(as.array(py_eval(agent_path_tree_positions)), as.array)),
    predicted_path_tree_position_list = list(lapply(as.array(py_eval(predicted_path_tree_positions)), as.array)),
  )
glimpse(practice_trials)

# Format evaluation (counterfactual / causal judgment) trials
eval_trials = eval_trials %>%
  mutate(
    outcome = ifelse(agent_path_succeeded, 'success', 'failure')
  )
glimpse(eval_trials)



# Format parameter fitted model predictions
param_fit_model_predictions = param_fit_model_predictions %>%
  mutate(
    counterfactual_start_outcome_change_se = sqrt(
      (counterfactual_start_outcome_change_probability*(1-counterfactual_start_outcome_change_probability))/model_samples),
    counterfactual_agent_outcome_change_se = sqrt(
      (counterfactual_agent_outcome_change_probability*(1-counterfactual_agent_outcome_change_probability))/model_samples),
    causal_tradeoff_prediction = counterfactual_agent_outcome_change_probability /
      (counterfactual_agent_outcome_change_probability+counterfactual_start_outcome_change_probability),
    causal_tradeoff_prediction_se = sqrt((causal_tradeoff_prediction*(1-causal_tradeoff_prediction))/model_samples),
    outcome = ifelse(as.logical(path_reached_reward_goal), 'success', 'failure')
  )
# Handle possible divide by 0 from above
param_fit_model_predictions %>% filter(is.na(causal_tradeoff_prediction)) %>% glimpse()
glimpse(param_fit_model_predictions)



# Summarize eval trials
eval_trial_summary = eval_trials %>%
  # Env CF summary
  filter(!is.na(environment_counterfactual_slider)) %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_env_cf = mean(environment_counterfactual_slider),
    obs_env_cf = n(),
    sem_env_cf = sd(environment_counterfactual_slider) / sqrt(n())
  ) %>%
  ungroup() %>%
  inner_join(
    # Trait CF summary
    eval_trials %>%
      filter(!is.na(trait_counterfactual_slider)) %>%
      group_by(agent_type, trial_name, outcome) %>%
      summarize(
        mean_trait_cf = mean(trait_counterfactual_slider),
        obs_trait_cf = n(),
        sem_trait_cf = sd(trait_counterfactual_slider) / sqrt(n())
      ) %>%
      ungroup(),
    by = c('agent_type', 'trial_name', 'outcome')
  ) %>%
  inner_join(
    # Env causal summary
    eval_trials %>%
      filter(!is.na(environment_causal_slider)) %>%
      group_by(agent_type, trial_name, outcome) %>%
      summarize(
        mean_env_caus = mean(environment_causal_slider),
        obs_env_caus = n(),
        sem_env_caus = sd(environment_causal_slider) / sqrt(n())
      ) %>% ungroup(),
    by = c('agent_type', 'trial_name', 'outcome')
  ) %>%
  inner_join(
    # Trait causal summary
    eval_trials %>%
      filter(!is.na(trait_causal_slider)) %>%
      group_by(agent_type, trial_name, outcome) %>%
      summarize(
        mean_trait_caus = mean(trait_causal_slider),
        obs_trait_caus = n(),
        sem_trait_caus = sd(trait_causal_slider) / sqrt(n())
      ) %>% ungroup(),
    by = c('agent_type', 'trial_name', 'outcome')
  ) %>%
  inner_join(
    # Causal tradeoff summary
    eval_trials %>%
      filter(!is.na(causal_selection_slider)) %>%
      group_by(agent_type, trial_name, outcome) %>%
      summarize(
        mean_tradeoff = mean(causal_selection_slider),
        obs_tradeoff = n(),
        sem_tradeoff = sd(causal_selection_slider) / sqrt(n())
      ) %>% ungroup(),
    by = c('agent_type', 'trial_name', 'outcome')
  )





# SAVE DATA ----
save(
  session_data,
  practice_trials,
  eval_trials,
  file = file.path(OUTPUT_DIR, 'experiment_data.RData')
)

save(
  param_fit_model_predictions,
  heuristic_model_predictions,
  file = file.path(OUTPUT_DIR, 'model_data.RData')
)




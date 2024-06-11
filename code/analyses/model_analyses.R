
#'
#' Analyses comparing model predictions to human responses
#'



# INIT ----
rm(list = ls())
library(tidyverse)




# GLOBALS ----

DATA_DIR = '../../data/data_processed'
SAVE_FIGS = FALSE
FIGURES_DIR = '../../figures/results'
# How to deal with pdf saving:
# https://stackoverflow.com/questions/74339268/ggplot-with-different-font-family-fails-to-save-to-pdf
# https://stackoverflow.com/questions/50767445/how-to-fix-failed-to-load-cairo-dll-in-r
# https://github.com/wch/extrafont



default_plot_theme = theme(
  # titles
  plot.title = element_text(face = 'bold', size = 32, family = 'Avenir', margin = margin(b = 0.5, unit = 'line')),
  axis.title.y = element_text(face = 'bold', size = 24, family = 'Avenir', margin = margin(r = 0.5, unit = 'line')),
  axis.title.x = element_text(face = 'bold', size = 24, family = 'Avenir', margin = margin(t = 0.5, unit = 'line')),
  legend.title = element_text(face = 'bold', size = 24, family = 'Avenir'),
  # axis text
  axis.text.x = element_text(size = 20, face = 'bold', angle = 0, vjust = 1, family = 'Avenir', margin = margin(t = 0.5, unit = 'line'), color = 'black'),
  axis.text.y = element_text(size = 20, face = 'bold', family = 'Avenir', margin = margin(r = 0.5, unit = 'line'), color = 'black'),
  # legend text
  legend.text = element_text(size = 22, face = 'bold', family = 'Avenir', margin = margin(b = 0.5, unit = 'line')),
  # facet text
  strip.text = element_text(size = 12, family = 'Avenir'),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid = element_line(color = 'gray'),
  axis.line = element_line(color = 'black'),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  # positioning
  legend.position = 'right',
  legend.key = element_rect(colour = 'transparent', fill = 'transparent')
)

color_lookup = c(
  'trait_counterfactual' = '#6790b9',
  'environment_counterfactual' = '#c27a72'
)

color_lookup_single_trial = c(
  'a_cf_env' = '#c27a72',
  'b_cf_trait' = '#6790b9',
  'c_c_env' = '#a94236',
  'd_c_trait' = '#26619c',
  'e_tradeoff' = '#a2798f'
)





# READ DATA ----
load(file.path(DATA_DIR, 'model_data.RData'))
load(file.path(DATA_DIR, 'experiment_data.RData'))
model_df = param_fit_model_predictions




# ANALYSIS: counterfactual model predictions -> counterfactual judgments ----
# This analysis explores whether counterfactual responses are predicted by the counterfactual simulation model

# > trait ----
# Summarize counterfactual judgments by condition and join with model predictions
trait_cf_model_summary = eval_trials %>%
  filter(!is.na(trait_counterfactual_slider)) %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_resp = mean(trait_counterfactual_slider),
    obs = n(),
    sem = sd(trait_counterfactual_slider) / sqrt(n())
  ) %>%
  ungroup() %>%
  inner_join(model_df %>%
               select(trial_name, agent_type,
                      counterfactual_agent_outcome_change_probability,
                      counterfactual_agent_outcome_change_se),
             by = c('trial_name', 'agent_type'),
  ) %>%
  mutate(
    counterfactual_agent_outcome_change_probability = counterfactual_agent_outcome_change_probability * 100,
    counterfactual_agent_outcome_change_se = counterfactual_agent_outcome_change_se * 100
  )

# Get summary values: correlation and RMSE
corr_trait = cor.test(trait_cf_model_summary$mean_resp, trait_cf_model_summary$counterfactual_agent_outcome_change_probability)
# source for rmse calc below: https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
trait_regression = lm(
  mean_resp ~ counterfactual_agent_outcome_change_probability,
  data = trait_cf_model_summary
)
rmse_trait = sqrt(c(crossprod(trait_regression$residuals)) / length(trait_regression$residuals))


# Figure
trait_cf_model_fig = trait_cf_model_summary %>%
  ggplot(aes(x = counterfactual_agent_outcome_change_probability, y = mean_resp)) +
  geom_abline(color = 'black', linewidth = 0.75, linetype = 'dashed') +
  geom_point(size = 8, alpha = 1, color = color_lookup['trait_counterfactual']) +
  geom_errorbar(aes(ymin = mean_resp - sem, ymax = mean_resp + sem),
                width = 0,
                linewidth = 1.5,
                color = color_lookup['trait_counterfactual']
  ) +
  geom_errorbarh(aes(xmin = counterfactual_agent_outcome_change_probability - counterfactual_agent_outcome_change_se,
                     xmax = counterfactual_agent_outcome_change_probability + counterfactual_agent_outcome_change_se),
                 height = 0,
                 linewidth = 1.5,
                 color = color_lookup['trait_counterfactual']
  ) +
  # Displaying RMSE and corr
  geom_text(
    label = paste('r = ', round(corr_trait$estimate, 2)),
    size = 10,
    family = 'Avenir',
    x = 12.5,
    y = 100,
    color = 'black'
  ) +
  geom_text(
    label = paste('RMSE = ', round(rmse_trait, 2)),
    size = 10,
    family = 'Avenir',
    x = 19,
    y = 90,
    color = 'black'
  ) +
  scale_x_continuous(
    name = 'MODEL COUNTERFACTUAL \nTRAIT',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_y_continuous(
    name = 'COUNTERFACTUAL \nTRAIT',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  default_plot_theme +
  theme(
    axis.title.x = element_text(face = 'plain',
                                size = 36,
                                family = 'Avenir',
                                margin = margin(t = 0.5, unit = 'line'),
                                lineheight = 1.1
    ),
    axis.title.y = element_text(face = 'plain',
                                size = 36,
                                family = 'Avenir',
                                margin = margin(t = 0.5, unit = 'line'),
                                lineheight = 1.1
    ),
    axis.text.x = element_text(size = 36,
                               face = 'plain',
                               angle = 0,
                               vjust = 1,
                               family = 'Avenir',
                               margin = margin(t = 0.5, unit = 'line'),
                               color = 'black'),
    axis.text.y = element_text(size = 36,
                               face = 'plain',
                               family = 'Avenir',
                               margin = margin(r = 0.5, unit = 'line'),
                               color = 'black'),
  ); trait_cf_model_fig

# Save figure
if (SAVE_FIGS) {
  ggsave(
    trait_cf_model_fig,
    filename = 'counterfactual_trait_model_preds.pdf',
    path = FIGURES_DIR,
    device = cairo_pdf,
    width = 10,
    height = 8,
  )
}



# > situation ----
# Summarize counterfactual judgments by condition and join with model predictions
situation_cf_model_summary = eval_trials %>%
  filter(!is.na(environment_counterfactual_slider)) %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_resp = mean(environment_counterfactual_slider),
    obs = n(),
    sem = sd(environment_counterfactual_slider) / sqrt(n())
  ) %>%
  ungroup() %>%
  inner_join(model_df %>%
               select(trial_name, agent_type,
                      counterfactual_start_outcome_change_probability,
                      counterfactual_start_outcome_change_se
               ),
             by = c('trial_name', 'agent_type'),
  ) %>%
  mutate(
    counterfactual_start_outcome_change_probability = counterfactual_start_outcome_change_probability * 100,
    counterfactual_start_outcome_change_se = counterfactual_start_outcome_change_se * 100
  )

# Get summary values: correlation and RMSE
corr_situation = cor.test(situation_cf_model_summary$mean_resp, situation_cf_model_summary$counterfactual_start_outcome_change_probability)
# source for rmse calc below: https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
situation_regression = lm(
  mean_resp ~ counterfactual_start_outcome_change_probability,
  data = situation_cf_model_summary
)
rmse_situation = sqrt(c(crossprod(situation_regression$residuals)) / length(situation_regression$residuals))

# Figure
situation_cf_model_fig = situation_cf_model_summary %>%
  ggplot(aes(x = counterfactual_start_outcome_change_probability, y = mean_resp)) +
  geom_abline(color = 'black', linewidth = 0.75, linetype = 'dashed') +
  geom_point(size = 8, alpha = 1, color = color_lookup['environment_counterfactual']) +
  geom_errorbar(aes(ymin = mean_resp - sem, ymax = mean_resp + sem),
                width = 0,
                linewidth = 1.5,
                color = color_lookup['environment_counterfactual']
  ) +
  geom_errorbarh(aes(xmin = counterfactual_start_outcome_change_probability - counterfactual_start_outcome_change_se,
                     xmax = counterfactual_start_outcome_change_probability + counterfactual_start_outcome_change_se),
                 height = 0,
                 linewidth = 1.5,
                 color = color_lookup['environment_counterfactual']
  ) +
  # Displaying RMSE and corr
  geom_text(
    label = paste('r = ', round(corr_situation$estimate, 2)),
    size = 10,
    family = 'Avenir',
    x = 12.5,
    y = 100,
    color = 'black'
  ) +
  geom_text(
    label = paste('RMSE = ', round(rmse_situation, 2)),
    size = 10,
    family = 'Avenir',
    x = 20,
    y = 90,
    color = 'black'
  ) +
  scale_x_continuous(
    name = 'MODEL COUNTERFACTUAL \nSITUATION',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_y_continuous(
    name = 'COUNTERFACTUAL \nSITUATION',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  default_plot_theme +
  theme(
    axis.title.x = element_text(face = 'plain',
                                size = 36,
                                family = 'Avenir',
                                margin = margin(t = 0.5, unit = 'line'),
                                lineheight = 1.1
    ),
    axis.title.y = element_text(face = 'plain',
                                size = 36,
                                family = 'Avenir',
                                margin = margin(r = 0.5, unit = 'line'),
                                lineheight = 1.1
    ),
    axis.text.x = element_text(size = 36,
                               face = 'plain',
                               angle = 0,
                               vjust = 1,
                               family = 'Avenir',
                               margin = margin(t = 0.5, unit = 'line'),
                               color = 'black'),
    axis.text.y = element_text(size = 36,
                               face = 'plain',
                               family = 'Avenir',
                               margin = margin(r = 0.5, unit = 'line'),
                               color = 'black'),
  ); situation_cf_model_fig

# Save figure
if (SAVE_FIGS) {
  ggsave(
    situation_cf_model_fig,
    filename = 'counterfactual_situation_model_preds.pdf',
    path = FIGURES_DIR,
    device = cairo_pdf,
    width = 10,
    height = 8,
  )
}



# ANALYSIS: counterfactual model predictions -> causal judgments ----

# > trait ----

trait_cause_model_summary = eval_trials %>%
  filter(!is.na(trait_causal_slider)) %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_resp = mean(trait_causal_slider),
    obs = n(),
    sem = sd(trait_causal_slider) / sqrt(n())
  ) %>%
  ungroup() %>%
  inner_join(model_df %>%
               select(trial_name, agent_type,
                      counterfactual_agent_outcome_change_probability,
                      counterfactual_agent_outcome_change_se),
             by = c('trial_name', 'agent_type'),
  ) %>%
  mutate(
    counterfactual_agent_outcome_change_probability = counterfactual_agent_outcome_change_probability * 100,
    counterfactual_agent_outcome_change_se = counterfactual_agent_outcome_change_se * 100
  )

# correlation
corr_trait_cause = cor.test(trait_cause_model_summary$mean_resp, trait_cause_model_summary$counterfactual_agent_outcome_change_probability)
corr_trait_cause



# > situation ----

situation_cause_model_summary = eval_trials %>%
  filter(!is.na(environment_causal_slider)) %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_resp = mean(environment_causal_slider),
    obs = n(),
    sem = sd(environment_causal_slider) / sqrt(n())
  ) %>%
  ungroup() %>%
  inner_join(model_df %>%
               select(trial_name, agent_type,
                      counterfactual_start_outcome_change_probability,
                      counterfactual_start_outcome_change_se),
             by = c('trial_name', 'agent_type'),
  ) %>%
  mutate(
    counterfactual_start_outcome_change_probability = counterfactual_start_outcome_change_probability * 100,
    counterfactual_start_outcome_change_se = counterfactual_start_outcome_change_se * 100
  )

# correlation
corr_situation_cause = cor.test(situation_cause_model_summary$mean_resp, situation_cause_model_summary$counterfactual_start_outcome_change_probability)
corr_situation_cause



# ANALYSIS: counterfactual model predictions -> causal tradeoff ----

tradeoff_summary = eval_trials %>%
  filter(!is.na(causal_selection_slider)) %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_resp = mean(causal_selection_slider),
    obs = n(),
    sem = sd(causal_selection_slider) / sqrt(n())
  ) %>%
  ungroup() %>%
  inner_join(model_df %>%
               select(trial_name, agent_type,
                      counterfactual_start_outcome_change_probability,
                      counterfactual_start_outcome_change_se,
                      counterfactual_agent_outcome_change_probability,
                      counterfactual_agent_outcome_change_se),
             by = c('trial_name', 'agent_type'),
  ) %>%
  mutate(
    counterfactual_agent_outcome_change_probability = counterfactual_agent_outcome_change_probability * 100,
    counterfactual_agent_outcome_change_se = counterfactual_agent_outcome_change_se * 100,
    counterfactual_start_outcome_change_probability = counterfactual_start_outcome_change_probability * 100,
    counterfactual_start_outcome_change_se = counterfactual_start_outcome_change_se * 100,
    # prediction for tradeoff value: 'decision policy' averaging the above counterfactual sliders
    mean_cf_agg = 100*(counterfactual_agent_outcome_change_probability / (counterfactual_agent_outcome_change_probability + counterfactual_start_outcome_change_probability)),
    se_cf_agg = sqrt((counterfactual_agent_outcome_change_se^2) + (counterfactual_start_outcome_change_se^2))
  )

# correlation
corr_tradeoff_cf = cor.test(tradeoff_summary$mean_cf_agg, tradeoff_summary$mean_resp)
corr_tradeoff_cf



# ANALYSIS: reliability of counterfactual judgments ----

split_half_samples = 1000

get_subject_splits = function(data) {
  split1_subjs = data %>%
    select(game_id) %>%
    unique() %>%
    slice_sample(prop = 0.5) # stochastic: see https://ds4world.cs.miami.edu/sampling
  split1_data = data %>%
    filter(game_id %in% split1_subjs$game_id) %>%
    group_by(trial_name) %>%
    summarize(mean_resp = mean(response)) %>%
    ungroup() %>%
    arrange(trial_name)
  split2_data = data %>%
    filter(!(game_id %in% split1_subjs$game_id)) %>%
    group_by(trial_name) %>%
    summarize(mean_resp = mean(response)) %>%
    ungroup() %>%
    arrange(trial_name)
  return(c(split1_data, split2_data))
}

get_split_half_correlation = function(split1, split2) {
  split_cor = cor.test(split1, split2)$estimate
  # Pearson-Brown correction:
  # https://en.wikipedia.org/wiki/Spearman%E2%80%93Brown_prediction_formula
  split_cor_adjusted = (2 * split_cor) / (1 + split_cor)
  return(split_cor_adjusted)
}

# Long form df for use in functions above
eval_trials_long = eval_trials %>%
  pivot_longer(
    cols = c('trait_counterfactual_slider',
             'environment_counterfactual_slider',
             'trait_causal_slider',
             'environment_causal_slider',
             'causal_selection_slider'),
    names_to = 'response_condition',
    values_to = 'response'
  ) %>%
  drop_na(response) %>%
  mutate(response = response / 100) %>%
  inner_join(
    model_df %>% select(
      'trial_name',
      'counterfactual_start_outcome_change_probability',
      'counterfactual_start_outcome_change_se',
      'counterfactual_agent_outcome_change_probability',
      'counterfactual_agent_outcome_change_se'
    ),
    by = c('trial_name')
  )



# > trait ----
reliability_samples_trait_cf = data.frame(
  condition = character(),
  reliability = numeric()
)
# NB: runtime of the loop below for 1k samples = ~5s
for(i in seq(split_half_samples)) {
  split_half_data = get_subject_splits(eval_trials_long %>% filter(condition == 'trait_counterfactual'))
  reliability_samples_trait_cf = rbind(
    reliability_samples_trait_cf,
    data.frame(
      condition = 'trait_counterfactual',
      reliability = get_split_half_correlation(split_half_data[2]$mean_resp, split_half_data[4]$mean_resp)
    )
  )
}
# Compare adjusted split-half correlations above to observed correlation with model results
summary(reliability_samples_trait_cf$reliability)
sd(reliability_samples_trait_cf$reliability)


# > situation ----
reliability_samples_situation_cf = data.frame(
  condition = character(),
  reliability = numeric()
)
# NB: runtime of the loop below for 1k samples = ~5s
for(i in seq(split_half_samples)) {
  split_half_data = get_subject_splits(eval_trials_long %>% filter(condition == 'environment_counterfactual'))
  reliability_samples_situation_cf = rbind(
    reliability_samples_situation_cf,
    data.frame(
      condition = 'environment_counterfactual',
      reliability = get_split_half_correlation(split_half_data[2]$mean_resp, split_half_data[4]$mean_resp)
    )
  )
}
# Compare adjusted split-half correlations above to observed correlation with model results
summary(reliability_samples_situation_cf$reliability)
sd(reliability_samples_situation_cf$reliability)


# ANALYSIS: individual trial performance ----
# This analysis used to compare trial-level behavior and select example trials for visualization

plot_individual_trial = function(subject_means, model_means, trial_data, trial, y_axis) {
  if(y_axis == TRUE) {
    yaxis_theme = theme(
      axis.title.y = element_text(face = 'plain',
                                  size = 48,
                                  family = 'Avenir',
                                  margin = margin(t = 0.25, unit = 'line'),
                                  lineheight = 1.1),
      axis.text.y = element_text(size = 36,
                                 face = 'plain',
                                 family = 'Avenir',
                                 margin = margin(r = 0.5, unit = 'line'),
                                 color = 'black')
      )
  } else {
    yaxis_theme = theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank())
  }
  subject_means %>%
    filter(trial_name == trial) %>%
    ggplot(aes(
      x = condition,
      y = response_mean,
      fill = condition,
    )) +
    geom_col(
      width = 0.9,
    ) +
    geom_errorbar(
      aes(
        ymin = response_mean - response_se,
        ymax = response_mean + response_se
      ),
      width = 0,
      linewidth = 2
    ) +
    geom_point(
      aes(x = condition, y = model_mean),
      data = model_means %>% filter(trial_name == trial),
      size = 12,
      color = alpha('black', 0.5),
      stroke = 3,
    ) +
    geom_jitter(
      data = trial_data %>% filter(trial_name == trial),
      aes(x = condition,
          y = response),
      color = 'black',
      height = 0,
      width = 0.2,
      size = 2,
      alpha = 0.6
    ) +
    scale_y_continuous(
      name = 'response',
      breaks = seq(0, 100, by = 25),
      labels = seq(0, 100, by = 25),
      limits = c(0, 100)
    ) +
    scale_x_discrete(
      name = element_blank(),
      labels = element_blank()
    ) +
    scale_fill_manual(
      name = element_blank(),
      values = color_lookup_single_trial
    ) +
    default_plot_theme +
    theme(
      legend.position = 'none',
      axis.title.x = element_text(face = 'plain',
                                  size = 36,
                                  family = 'Avenir',
                                  margin = margin(t = 0.25, unit = 'line'),
                                  lineheight = 1.1
      ),
      axis.text.x = element_text(size = 36,
                                 face = 'plain',
                                 angle = 90,
                                 hjust = 1,
                                 vjust = 0.5,
                                 family = 'Avenir',
                                 margin = margin(t = 0.5, unit = 'line'),
                                 color = 'black'),
    ) +
    yaxis_theme
}

eval_trial_summary = eval_trials %>%
  group_by(trial_name, agent_type, outcome) %>%
  summarize(
    a_cf_env = mean(environment_counterfactual_slider, na.rm = T),
    b_cf_trait = mean(trait_counterfactual_slider, na.rm = T),
    c_c_env = mean(environment_causal_slider, na.rm = T),
    d_c_trait = mean(trait_causal_slider, na.rm = T),
    e_tradeoff = mean(causal_selection_slider, na.rm = T),
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff),
    names_to = 'condition',
    values_to = 'response_mean'
  ) %>%
  inner_join(
    eval_trials %>%
      group_by(trial_name, agent_type, outcome) %>%
      summarize(
        a_cf_env = sd(environment_counterfactual_slider, na.rm = T) / sqrt(30),
        b_cf_trait = sd(trait_counterfactual_slider, na.rm = T) / sqrt(30),
        c_c_env = sd(environment_causal_slider, na.rm = T) / sqrt(30),
        d_c_trait = sd(trait_causal_slider, na.rm = T) / sqrt(30),
        e_tradeoff = sd(causal_selection_slider, na.rm = T) / sqrt(30)
      ) %>%
      ungroup() %>%
      pivot_longer(
        cols = c(a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff),
        names_to = 'condition',
        values_to = 'response_se'
      ),
    by = c('trial_name', 'agent_type', 'outcome', 'condition')
  )

model_trial_summary = model_df %>%
  mutate(
    a_cf_env = 100*counterfactual_start_outcome_change_probability,
    b_cf_trait = 100*counterfactual_agent_outcome_change_probability,
    c_c_env = 100*counterfactual_start_outcome_change_probability,
    d_c_trait = 100*counterfactual_agent_outcome_change_probability,
    e_tradeoff = 100*causal_tradeoff_prediction
  ) %>%
  select(
    trial_name, agent_type, outcome, a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff
  ) %>%
  pivot_longer(
    cols = c(a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff),
    names_to = 'condition',
    values_to = 'model_mean'
  ) %>%
  inner_join(
    model_df %>%
      mutate(
        a_cf_env = 100*counterfactual_start_outcome_change_se,
        b_cf_trait = 100*counterfactual_agent_outcome_change_se,
        c_c_env = 100*counterfactual_start_outcome_change_se,
        d_c_trait = 100*counterfactual_agent_outcome_change_se,
        e_tradeoff = 100*causal_tradeoff_prediction_se
      ) %>%
      select(
        trial_name, agent_type, outcome, a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff
      ) %>%
      pivot_longer(
        cols = c(a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff),
        names_to = 'condition',
        values_to = 'model_se'
      ),
    by = c('trial_name', 'agent_type', 'outcome', 'condition')
  )


eval_trial_subject_responses = eval_trials %>%
  mutate(
    a_cf_env = environment_counterfactual_slider,
    b_cf_trait = trait_counterfactual_slider,
    c_c_env = environment_causal_slider,
    d_c_trait = trait_causal_slider,
    e_tradeoff = causal_selection_slider,
  ) %>%
  select(game_id, trial_name, agent_type,
         a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff
  ) %>%
  pivot_longer(
    cols = c(a_cf_env, b_cf_trait, c_c_env, d_c_trait, e_tradeoff),
    names_to = 'condition',
    values_to = 'response'
  ) %>%
  filter(!is.na(response))



# Figures
trials = c('trial_13_v2', 'trial_269', 'trial_894', 'trial_528', 'trial_453')
# Order of appearance in the figure is order above (note this is not numerical order)
trial_i = trials[1] # NB: toggle this value then run the below

trial_individ = plot_individual_trial(subject_means = eval_trial_summary,
                                      model_means = model_trial_summary,
                                      trial_data = eval_trial_subject_responses,
                                      trial = trial_i,
                                      y_axis = T)
trial_individ

if (SAVE_FIGS) {
  ggsave(
    trial_individ,
    filename= paste('trial_summary_', trial_i, '.pdf', sep=''),
    path = FIGURES_DIR,
    device = cairo_pdf,
    width = 12, # 14 if including y axis
    height = 6,
  )
}



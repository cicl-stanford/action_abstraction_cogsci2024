
#'
#' Analyses of behavioral data
#'


# INIT ----
rm(list = ls())
library(lme4)
library(tidyverse)



# GLOBALS ----

DATA_DIR = '../../data/data_processed'
SAVE_FIGS = FALSE
FIGURES_DIR = '../../figures/results'
# How to deal with pdf saving:
# https://stackoverflow.com/questions/74339268/ggplot-with-different-font-family-fails-to-save-to-pdf
# https://stackoverflow.com/questions/50767445/how-to-fix-failed-to-load-cairo-dll-in-r
# https://github.com/wch/extrafont


color_lookup = c(
  'trait_counterfactual' = '#26619c',
  'environment_counterfactual' = '#a94236',
  'causal_selection' = '#a2798f'
)

default_plot_theme = theme(
  # titles
  plot.title = element_text(face = 'bold', size = 32, family = 'Avenir', margin = margin(b = 0.5, unit = 'line')),
  axis.title.y = element_text(face = 'bold', size = 24, family = 'Avenir', margin = margin(r = 0.5, unit = 'line')),
  axis.title.x = element_text(face = 'bold', size = 24, family = 'Avenir', margin = margin(t = 0.5, unit = 'line')),
  legend.title = element_text(face = 'bold', size = 24, family = 'Avenir'),
  # axis text
  axis.text.x = element_text(size = 20, face = 'bold', angle = 0, vjust = 1, family = 'Avenir', margin = margin(t = 0.5, unit = 'line'), color = 'black'),
  axis.text.y = element_text(size = 20, face = 'bold', family = 'Avenir', margin = margin(r = 0.5, unit = 'line'), color = 'black'),
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



# READ DATA ----
load(file.path(DATA_DIR, 'experiment_data.RData'))



# DEMOGRAPHICS ----

# Age, gender, race
summary(session_data$age)
table(session_data$gender)
table(session_data$race)

# Completion time
summary(session_data$total_experiment_time)
sd(session_data$total_experiment_time)




# VALIDATION: learning trial prediction accuracy ----
# This is a sanity check analysis evaluating people's success on the learning trials
# "Participants acquired an accurate predictive model of each farmer before the evaluation trials"

# Overall accuracy
summary(practice_trials$predicted_path_accuracy)
sd(practice_trials$predicted_path_accuracy)
# First and last trial accuracy
summary(practice_trials$predicted_path_accuracy[practice_trials$trial_num==min(practice_trials$trial_num)])
sd(practice_trials$predicted_path_accuracy[practice_trials$trial_num==min(practice_trials$trial_num)])
summary(practice_trials$predicted_path_accuracy[practice_trials$trial_num==max(practice_trials$trial_num)])
sd(practice_trials$predicted_path_accuracy[practice_trials$trial_num==max(practice_trials$trial_num)])


# Regressions: test for learning and condition differences
acc_baseline = lmer(
  predicted_path_accuracy ~ (1 | game_id) + (1 | trial_name),
  data = practice_trials,
  REML = F
)
acc_trial = lmer(
  predicted_path_accuracy ~ trial_num + (1 | game_id) + (1 | trial_name),
  data = practice_trials,
  REML = F
)
anova(acc_trial, acc_baseline) # People aren't learning!

acc_cond = lmer(
  predicted_path_accuracy ~ condition + (1 | game_id) + (1 | trial_name),
  data = practice_trials,
  REML = F
)
anova(acc_cond, acc_baseline) # No effect of condition


# Visualizing accuracy
practice_trials %>%
  ggplot(aes(x = trial_num + 1, y = predicted_path_accuracy)) +
  stat_summary(fun.data = 'mean_cl_boot') +
  scale_x_continuous(
    name = 'trial',
    breaks = seq(1, 6, by = 1),
    labels = seq(1, 6, by = 1)
  ) +
  scale_y_continuous(
    name = 'prediction accuracy',
  ) +
  default_plot_theme





# VALIDATION: comprehension check attempts ----
# This is a sanity check analysis evaluating people's comprehension check performance

# Summary
session_data %>%
  mutate(total_n = n()) %>%
  group_by(comprehension_attempts) %>%
  summarize(
    subjs = n(),
  ) %>%
  mutate(
    completion_pct = subjs / sum(subjs)
  )

# Visualizing retries
session_data %>%
  ggplot(aes(x = 'comprehension check retries',
             y = comprehension_attempts)) +
  geom_jitter(size = 3, alpha = 0.25, height = 0, width = 0.1) +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(
    name = 'comprehension check retries',
    breaks = seq(0, 8, by = 1),
    labels = seq(0, 8, by = 1),
    limits = c(0, 8)
  ) +
  default_plot_theme






# ANALYSIS: counterfactual judgments -> causal judgments ----
# This analysis explores whether causal responses are predicted by people's counterfactual responses

# Summarize counterfactual and causal judgments by condition
causal_cf_summary = eval_trials %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    mean_situation_counterfactual = mean(environment_counterfactual_slider, na.rm = T),
    se_situation_counterfactual = sd(environment_counterfactual_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'environment_counterfactual']))),
    mean_trait_counterfactual = mean(trait_counterfactual_slider, na.rm = T),
    se_trait_counterfactual = sd(trait_counterfactual_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'trait_counterfactual']))),
    mean_situation_causal = mean(environment_causal_slider, na.rm = T),
    se_situation_causal = sd(environment_causal_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'environment_causal']))),
    mean_trait_causal = mean(trait_causal_slider, na.rm = T),
    se_trait_causal = sd(trait_causal_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'trait_causal']))),
  ) %>%
  ungroup()



# > trait ----

# Get summary values: correlation and RMSE
corr_trait = cor.test(causal_cf_summary$mean_trait_counterfactual, causal_cf_summary$mean_trait_causal)
# source for rmse calc below: https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
trait_regression = lm(
  mean_trait_causal ~ mean_trait_counterfactual,
  data = causal_cf_summary
)
rmse_trait = sqrt(c(crossprod(trait_regression$residuals)) / length(trait_regression$residuals))

# Figure
trait_fig = causal_cf_summary %>%
  ggplot(aes(x = mean_trait_counterfactual, y = mean_trait_causal)) +
  geom_abline(color = 'black', linewidth = 0.75, linetype = 'dashed') +
  geom_point(size = 8, alpha = 1, color = color_lookup['trait_counterfactual']) +
  geom_errorbar(aes(ymin = mean_trait_causal - se_trait_causal,
                    ymax = mean_trait_causal + se_trait_causal),
                width = 0,
                linewidth = 1.5,
                color = color_lookup['trait_counterfactual']
  ) +
  geom_errorbarh(aes(xmin = mean_trait_counterfactual - se_trait_counterfactual,
                     xmax = mean_trait_counterfactual + se_trait_counterfactual),
                 height = 0,
                 linewidth = 1.5,
                 color = color_lookup['trait_counterfactual']
  ) +
  # Displaying RMSE and corr
  # geom_text(
  #   label = paste('r = ', round(corr_trait$estimate, 2)),
  #   size = 10,
  #   family = 'Avenir',
  #   x = 12.5,
  #   y = 100,
  #   color = 'black'
  # ) +
  # geom_text(
  #   label = paste('RMSE = ', round(rmse_trait, 2)),
  #   size = 10,
  #   family = 'Avenir',
  #   x = 20,
  #   y = 90,
  #   color = 'black'
  # ) +
  scale_x_continuous(
    name = 'COUNTERFACTUAL \nTRAIT',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_y_continuous(
    name = 'CAUSAL \nTRAIT',
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
                               face='plain',
                               angle = 0,
                               vjust = 1,
                               family = 'Avenir',
                               margin = margin(t = 0.5, unit = 'line'), color = 'black'),
    axis.text.y = element_text(size = 36,
                               face='plain',
                               family = 'Avenir',
                               margin = margin(r = 0.5, unit = 'line'),
                               color = 'black'),
  ); trait_fig

# Save figure
if (SAVE_FIGS) {
  ggsave(
    trait_fig,
    filename = 'counterfactual_causal_trait.pdf',
    path = FIGURES_DIR,
    device = cairo_pdf,
    width = 10,
    height = 8,
  )
}




# > situation ----

# Get summary values: correlation and RMSE
corr_situation = cor.test(causal_cf_summary$mean_situation_counterfactual, causal_cf_summary$mean_situation_causal)
# source for RMSE calc below: https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
situation_regression = lm(
  mean_situation_causal ~ mean_situation_counterfactual,
  data = causal_cf_summary
)
rmse_situation = sqrt(c(crossprod(situation_regression$residuals)) / length(situation_regression$residuals))

# Figure
situation_fig = causal_cf_summary %>%
  ggplot(aes(x = mean_situation_counterfactual, y = mean_situation_causal)) +
  geom_abline(color = 'black', linewidth = 0.75, linetype = 'dashed') +
  geom_point(size = 8, alpha = 1, color = color_lookup['environment_counterfactual']) +
  geom_errorbar(aes(ymin = mean_situation_causal - se_situation_causal,
                    ymax = mean_situation_causal + se_situation_causal),
                width = 0,
                linewidth = 1.5,
                color = color_lookup['environment_counterfactual']
  ) +
  geom_errorbarh(aes(xmin = mean_situation_counterfactual - se_situation_counterfactual,
                     xmax = mean_situation_counterfactual + se_situation_counterfactual),
                 height = 0,
                 linewidth = 1.5,
                 color = color_lookup['environment_counterfactual']
  ) +
  # Displaying RMSE and corr
  # geom_text(
  #   label = paste('r = ', round(corr_situation$estimate, 2)),
  #   size = 10,
  #   family = 'Avenir',
  #   x = 12.5,
  #   y = 100,
  #   color = 'black'
  # ) +
  # geom_text(
  #   label = paste('RMSE = ', round(rmse_situation, 2)),
  #   size = 10,
  #   family = 'Avenir',
  #   x = 20,
  #   y = 90,
  #   color = 'black'
  # ) +
  scale_x_continuous(
    name = 'COUNTERFACTUAL \nSITUATION',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_y_continuous(
    name = 'CAUSAL \nSITUATION',
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
  ); situation_fig

# Save figure
if (SAVE_FIGS) {
  ggsave(
    situation_fig,
    filename = 'counterfactual_causal_situation.pdf',
    path = FIGURES_DIR,
    device = cairo_pdf,
    width = 10,
    height = 8,
  )
}



# ANALYSIS: causal judgments -> causal tradeoff ----

# Summarize tradeoff and causal judgments by condition, normalize causal judgments to predict tradeoff
tradeoff_causal_summary = eval_trials %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    # Summarize causal tradeoff
    mean_causal_tradeoff = mean(causal_selection_slider, na.rm = T),
    se_causal_tradeoff = sd(causal_selection_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'causal_selection']))),
    # Summarize causal sliders
    mean_trait_causal = mean(trait_causal_slider, na.rm = T),
    se_trait_causal = sd(trait_causal_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'trait_causal']))),
    mean_situation_causal = mean(environment_causal_slider, na.rm = T),
    se_situation_causal = sd(environment_causal_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'environment_causal']))),
    # 'Decision policy' averaging the above causal sliders: prediction for tradeoff value
    mean_causal_agg = 100*(mean_trait_causal / (mean_trait_causal + mean_situation_causal)),
    se_causal_agg = sqrt((se_trait_causal^2) + (se_situation_causal^2))
  ) %>%
  ungroup()



# Get summary values: correlation and RMSE
corr_tradeoff_causal = cor.test(tradeoff_causal_summary$mean_causal_agg, tradeoff_causal_summary$mean_causal_tradeoff)
# source for rmse calc below: https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result
tradeoff_regression = lm(
  mean_causal_tradeoff ~ mean_causal_agg,
  data = tradeoff_causal_summary
)
rmse_tradeoff_causal = sqrt(c(crossprod(tradeoff_regression$residuals)) / length(tradeoff_regression$residuals))

# Figure
tradeoff_fig = tradeoff_causal_summary %>%
  ggplot(aes(x = mean_causal_agg, y = mean_causal_tradeoff)) +
  geom_abline(color = 'black', linewidth = 0.75, linetype = 'dashed') +
  geom_point(size = 8, alpha = 1, color = color_lookup['causal_selection']) +
  geom_errorbar(aes(ymin = mean_causal_tradeoff - se_causal_tradeoff,
                    ymax = mean_causal_tradeoff + se_causal_tradeoff),
                width = 0,
                linewidth = 1.5,
                color = color_lookup['causal_selection']
  ) +
  geom_errorbarh(aes(xmin = mean_causal_agg - se_causal_agg,
                     xmax = mean_causal_agg + se_causal_agg),
                 height = 0,
                 linewidth = 1.5,
                 color = color_lookup['causal_selection']
  ) +
  # geom_text(
  #   label = paste('r = ', round(corr_tradeoff_causal$estimate, 2)),
  #   size = 10,
  #   family = 'Avenir',
  #   x = 13,
  #   y = 100,
  #   color = 'black'
  # ) +
  # geom_text(
  #   label = paste('RMSE = ', round(rmse_tradeoff_causal, 2)),
  #   size = 10,
  #   family = 'Avenir',
  #   x = 19,
  #   y = 90,
  #   color = 'black'
  # ) +
  scale_x_continuous(
    name = 'CAUSAL AGGREGATED \n0: situation 100: trait',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_y_continuous(
    name = 'CAUSAL TRADEOFF \n0: situation 100: trait',
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
  ); tradeoff_fig

if (SAVE_FIGS) {
  ggsave(
    tradeoff_fig,
    filename = 'tradeoff_causal_predictors.pdf',
    path = FIGURES_DIR,
    device = cairo_pdf,
    width = 10,
    height = 8,
  )
}



# ANALYSIS: counterfactual judgments -> causal tradeoff ----

# Summarize tradeoff and causal judgments by condition, normalize causal judgments to predict tradeoff
tradeoff_cf_summary = eval_trials %>%
  group_by(agent_type, trial_name, outcome) %>%
  summarize(
    # Summarize causal tradeoff
    mean_causal_tradeoff = mean(causal_selection_slider, na.rm = T),
    se_causal_tradeoff = sd(causal_selection_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'causal_selection']))),
    # Summarize causal sliders
    mean_trait_cf = mean(trait_counterfactual_slider, na.rm = T),
    se_trait_cf = sd(trait_counterfactual_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'trait_counterfactual']))),
    mean_situation_cf = mean(environment_counterfactual_slider, na.rm = T),
    se_situation_cf = sd(environment_counterfactual_slider, na.rm = T) / sqrt(length(unique(eval_trials$game_id[eval_trials$condition == 'environment_counterfactual']))),
    # prediction for tradeoff value: 'decision policy' averaging the above causal sliders
    mean_cf_agg = 100*(mean_trait_cf / (mean_trait_cf + mean_situation_cf)),
    se_cf_agg = sqrt((se_trait_cf^2) + (se_situation_cf^2))
  ) %>%
  ungroup()

corr_tradeoff_cf = cor.test(tradeoff_cf_summary$mean_cf_agg, tradeoff_cf_summary$mean_causal_tradeoff)
corr_tradeoff_cf




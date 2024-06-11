#'
#' Model comparison
#'




# INIT ----
rm(list = ls())
library(brms)
library(lme4)
library(tidyverse)


# GLOBALS ----
DATA_DIR = "../../data/data_processed"

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


# READ DATA ----
load(file.path(DATA_DIR, "model_data.RData"))
load(file.path(DATA_DIR, "experiment_data.RData"))



# PROCESS DATA ----
# Format trial data
# TODO move all the below into `data_processing.R`
eval_trials = eval_trials %>%
  mutate(
    agent_type_factor = ifelse(agent_type == "optimist", 1, -1),
    outcome_factor = ifelse(outcome == "success", 1, -1)
  )
heuristic_model_predictions = heuristic_model_predictions %>%
  rename(agent_type = agent,
         outcome_binary = outcome)

# Get average and SE of responses at trial level
eval_summary = eval_trials %>%
  group_by(trial_name, agent_type, agent_type_factor, outcome, outcome_factor) %>%
  summarize(
    mean_situation_counterfactual = mean(environment_counterfactual_slider, na.rm = T),
    se_situation_counterfactual = sd(environment_counterfactual_slider, na.rm = T) /
      sqrt(length(unique(eval_trials$game_id[!is.na(eval_trials$environment_counterfactual_slider)]))),
    mean_trait_counterfactual = mean(trait_counterfactual_slider, na.rm = T),
    se_trait_counterfactual = sd(trait_counterfactual_slider, na.rm = T) /
      sqrt(length(unique(eval_trials$game_id[!is.na(eval_trials$trait_counterfactual_slider)]))),
    mean_situation_causal = mean(environment_causal_slider, na.rm = T),
    se_situation_causal = sd(environment_causal_slider, na.rm = T) /
      sqrt(length(unique(eval_trials$game_id[!is.na(eval_trials$environment_causal_slider)]))),
    mean_trait_causal = mean(trait_causal_slider, na.rm = T),
    se_trait_causal = sd(trait_causal_slider, na.rm = T) /
      sqrt(length(unique(eval_trials$game_id[!is.na(eval_trials$trait_causal_slider)]))),
  ) %>%
  ungroup()

# Add trial averages to individual response data frame
eval_trials = eval_trials %>%
  left_join(
    eval_summary %>% select(trial_name,
      mean_situation_counterfactual,
      se_situation_counterfactual,
      mean_trait_counterfactual,
      se_trait_counterfactual,
      mean_situation_causal,
      se_situation_causal,
      mean_trait_causal,
      se_trait_causal),
    by = c("trial_name")
  )

# Add heuristic model predictions to individual response and trial average dataframes
eval_trials = eval_trials %>%
  left_join(
    heuristic_model_predictions,
    by = c("agent_type", "trial_name")
  )
eval_summary = eval_summary %>%
  left_join(
    heuristic_model_predictions,
    by = c("agent_type", "trial_name")
  )



# ANALYSIS: BRM model comparison ----
# NB: each model below takes ~1 min to run
# Uses individual slider responses (Y) ~ trial average counterfactual and heuristic values (X)

# > Trait models ----

# Baseline
baseline_trait = brm(
  data = eval_trials %>% filter(!is.na(trait_causal_slider)),
  formula = trait_causal_slider ~ 1 + (1 | game_id),
  file = "brms_fits/baseline_trait", # saves to baseline_trait.rds
  seed = 1
)

# Counterfactual simulation
cf_trait = brm( # NB: divergent transitions with slopes, not with intercepts only
  data = eval_trials %>% filter(!is.na(trait_causal_slider)),
  # formula = trait_causal_slider ~ 1 + mean_trait_counterfactual + (1 + mean_trait_counterfactual | game_id),
  formula = trait_causal_slider ~ 1 + mean_trait_counterfactual + (1 | game_id),
  file = "brms_fits/cf_trait",
  seed = 1
)


# Heuristic
heuristic_trait = brm(
  data = eval_trials %>% filter(!is.na(trait_causal_slider)),
  # formula = trait_causal_slider ~ agent_type_factor*outcome_factor + (1 + agent_type_factor*outcome_factor | game_id),
  formula = trait_causal_slider ~ 1 + agent_type_factor*outcome_factor + (1 | game_id),
  file = "brms_fits/heuristic_trait",
  seed = 1
)

# Basis for heuristic trait model
# Note very high average causal slider when agent == optimist, outcome == success
eval_trials %>%
  filter(!is.na(trait_causal_slider)) %>%
  group_by(agent_type, outcome) %>%
  summarize(
    mean_trait = mean(trait_causal_slider),
    se_trait = sd(trait_causal_slider) / sqrt(n())) %>%
  ggplot(aes(x = outcome, y = mean_trait, fill = agent_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin = mean_trait-se_trait,
                    ymax = mean_trait+se_trait),
                width = 0, linewidth = 1,
                position = position_dodge(width = 1)) +
  scale_y_continuous(
    name = 'CAUSAL TRAIT',
    breaks = seq(0, 80, by = 25),
    labels = seq(0, 75, by = 25),
    limits = c(0, 80)
  ) +
  scale_fill_manual(
    name = element_blank(),
    values = c(
        "pessimist" = "#AC38FF",
        "optimist" = "#FFD400"
      )
  ) +
  default_plot_theme


# Leave-one-out cross-validation
baseline_trait = add_criterion(
  baseline_trait,
  criterion = "loo",
  reloo = T,
  file = "brms_fits/baseline_trait")

cf_trait = add_criterion(
  cf_trait,
  criterion = "loo",
  reloo = T,
  file = "brms_fits/cf_trait")

heuristic_trait = add_criterion(
  heuristic_trait,
  criterion = "loo",
  reloo = T,
  file = "brms_fits/heuristic_trait")


# Comparisons to baseline
loo_compare(baseline_trait, cf_trait)
loo_compare(baseline_trait, heuristic_trait)
# Comparison to alternatives
# NB: results are similar with subject intercepts only
loo_compare(cf_trait, heuristic_trait)



# Individual model fits
# Fit models to each individual's slider values
# Initialize models
cf_trait_individ = brm(
  # NB: choosing one subject here just to start process
  data = eval_trials %>% filter(!is.na(trait_causal_slider), game_id == "0779-31fab7dc-6922-48c8-8ca6-9d4012a55d08"),
  formula = trait_causal_slider ~ 1 + mean_trait_counterfactual,
  save_pars = save_pars(all = TRUE),
  file = "brms_fits/cf_trait_individ",
  seed = 1
)

heuristic_trait_individ = brm(
  # NB: choosing one subject here just to start process
  data = eval_trials %>% filter(!is.na(trait_causal_slider), game_id == "0779-31fab7dc-6922-48c8-8ca6-9d4012a55d08"),
  formula = trait_causal_slider ~ 1 + agent_type_factor*outcome_factor,
  save_pars = save_pars(all = TRUE),
  file = "brms_fits/heuristic_trait_individ",
  seed = 1,
)


# Model fits for each participant
# Source: https://cicl-stanford.github.io/responsibility_replacement/#32_Models
# NB: warnings on some of the individual fits
eval_trial_model_fits_trait = eval_trials %>%
  filter(!is.na(trait_causal_slider)) %>%
  group_by(game_id) %>%
  nest() %>%
  ungroup() %>%
  mutate(fit_cf_trait = map(.x = data,
                            .f = ~ update(cf_trait_individ,
                                          newdata = .x,
                                          seed = 1)),
         fit_heuristic_trait = map(.x = data,
                                   .f = ~ update(heuristic_trait_individ,
                                                 newdata = .x,
                                                 seed = 1))) %>%
  mutate(fit_cf_trait = map(.x = fit_cf_trait,
                            .f = ~ add_criterion(.x, criterion = "loo", moment_match = TRUE)),
         fit_heuristic_trait = map(.x = fit_heuristic_trait,
                                   .f = ~ add_criterion(.x, criterion = "loo", moment_match = TRUE)),
         model_comparison = pmap(.l = list(cf_trait = fit_cf_trait,
                                           heuristic_trait = fit_heuristic_trait),
                                 .f = ~ loo_compare(..1, ..2)),
         best_model = map_chr(.x = model_comparison,
                              .f = ~ rownames(.) %>%
                                .[1]),
         best_model = factor(best_model,
                             levels = c("..1", "..2"),
                             labels = c("cf_trait",
                                        "heuristic_trait")))

# glimpse(eval_trial_model_fits_trait)
save(list = c("eval_trial_model_fits_trait"),
     file = "brms_fits/trait_individual_model_fits.RData")

load(file = "brms_fits/trait_individual_model_fits.RData")
eval_trial_model_fits_trait %>%
  count(best_model) %>%
  arrange(desc(n))


# > Situation models ----

baseline_situation = brm(
  data = eval_trials %>% filter(!is.na(environment_causal_slider)),
  formula = environment_causal_slider ~ 1 + (1 | game_id),
  file = "brms_fits/baseline_situation",
  seed = 1,
)

cf_situation = brm(
  data = eval_trials %>% filter(!is.na(environment_causal_slider)),
  # formula = environment_causal_slider ~ 1 + mean_situation_counterfactual + (1 + mean_situation_counterfactual | game_id),
  formula = environment_causal_slider ~ 1 + mean_situation_counterfactual + (1 | game_id),
  file = "brms_fits/cf_situation",
  seed = 1,
)

heuristic_situation = brm( # NB: divergent transitions with slopes, not with intercepts only
  data = eval_trials %>% filter(!is.na(environment_causal_slider)),
  # formula = environment_causal_slider ~ 1 + discounted_expected_reward*outcome_factor + (1 + discounted_expected_reward*outcome_factor | game_id),
  formula = environment_causal_slider ~ 1 + discounted_expected_reward*outcome_factor + (1 | game_id),
  file = "brms_fits/heuristic_situation",
  seed = 1,
)

# Basis for heuristic situation model
summary_dist = eval_trials %>%
  filter(!is.na(environment_causal_slider)) %>%
  group_by(trial_name, outcome) %>%
  summarize(
    mean_distance_expected_reward = mean(discounted_expected_reward), # just one unique val per trial here
    mean_causal_situation = mean(environment_causal_slider),
    se_causal_situation = sd(environment_causal_slider) / sqrt(n())
  )

cor_summary = cor.test(summary_dist$mean_distance_expected_reward, summary_dist$mean_causal_situation)
reg_summary = lm(data = summary_dist,
              mean_causal_situation ~ mean_distance_expected_reward)
rmse = sqrt(c(crossprod(reg_summary$residuals)) / length(reg_summary$residuals))

summary_dist %>%
  ggplot(aes(x = mean_distance_expected_reward, y = mean_causal_situation, color = outcome)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = mean_causal_situation-se_causal_situation,
                    ymax = mean_causal_situation+se_causal_situation),
                width = 0, linewidth = 1.5) +
  geom_text(
    label = paste('r = ', round(cor_summary$estimate, 2)),
    size = 6,
    family = 'Avenir',
    x = 1.85,
    y = 100,
    color = 'black'
  ) +
  geom_text(
    label = paste('RMSE = ', round(rmse, 2)),
    size = 6,
    family = 'Avenir',
    x = 2,
    y = 90,
    color = 'black'
  ) +
  scale_x_continuous(
    name = 'distance-weighted expected reward',
    breaks = seq(1.5, 3, by = .5),
    labels = seq(1.5, 3, by = .5),
    limits = c(1.5, 3)
  ) +
  scale_y_continuous(
    name = 'CAUSAL SITUATION',
    breaks = seq(0, 100, by = 25),
    labels = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_color_manual(
    name = element_blank(),
    values = c(
      "failure" = "#EE220C",
      "success" = "#1DB100"
    )
  ) +
  default_plot_theme



# Leave-one-out cross-validation
baseline_situation = add_criterion(
  baseline_situation,
  criterion = "loo",
  reloo = T,
  file = "brms_fits/baseline_situation")

cf_situation = add_criterion(
  cf_situation,
  criterion = "loo",
  reloo = T,
  file = "brms_fits/cf_situation")

heuristic_situation = add_criterion(
  heuristic_situation,
  criterion = "loo",
  reloo = T,
  file = "brms_fits/heuristic_situation")

# Comparisons to baseline
loo_compare(baseline_situation, cf_situation)
loo_compare(baseline_situation, heuristic_situation)
# Comparison to alternatives
loo_compare(cf_situation, heuristic_situation)



# Individual model fits

# Initialize models
cf_situation_individ = brm(
  # NB: choosing one subject here just to start process
  data = eval_trials %>% filter(!is.na(environment_causal_slider), game_id == "0219-b4330982-2196-4772-96fb-e12405ff8a83"),
  formula = environment_causal_slider ~ 1 + mean_situation_counterfactual,
  save_pars = save_pars(all = TRUE),
  file = "brms_fits/cf_situation_individ",
  seed = 1
)

heuristic_situation_individ = brm(
  data = eval_trials %>% filter(!is.na(environment_causal_slider), game_id == "0219-b4330982-2196-4772-96fb-e12405ff8a83"),
  formula = environment_causal_slider ~ 1 + discounted_expected_reward * outcome_factor,
  save_pars = save_pars(all = TRUE),
  file = "brms_fits/heuristic_situation_individ",
  seed = 1,
)


# Model fits for each participant
# Source: https://cicl-stanford.github.io/responsibility_replacement/#32_Models
# NB: warnings on some of the individual fits
eval_trial_model_fits_situation = eval_trials %>%
  filter(!is.na(environment_causal_slider)) %>%
  group_by(game_id) %>%
  nest() %>%
  ungroup() %>%
  mutate(fit_cf_situation = map(.x = data,
                          .f = ~ update(cf_situation_individ,
                                        newdata = .x,
                                        seed = 1)),
         fit_heuristic_situation = map(.x = data,
                                 .f = ~ update(heuristic_situation_individ,
                                               newdata = .x,
                                               seed = 1))) %>%
  mutate(fit_cf_situation = map(.x = fit_cf_situation,
                                .f = ~ add_criterion(.x, criterion = "loo", moment_match = TRUE)),
         fit_heuristic_situation = map(.x = fit_heuristic_situation,
                                       .f = ~ add_criterion(.x, criterion = "loo", moment_match = TRUE)),
         model_comparison = pmap(.l = list(cf_situation = fit_cf_situation,
                                           heuristic_situation = fit_heuristic_situation),
                                 .f = ~ loo_compare(..1, ..2)),
         best_model = map_chr(.x = model_comparison,
                              .f = ~ rownames(.) %>%
                                .[1]),
         best_model = factor(best_model,
                             levels = c("..1", "..2"),
                             labels = c("cf_situation",
                                        "heuristic_situation")))

# glimpse(eval_trial_model_fits_situation)
save(list = c("eval_trial_model_fits_situation"),
     file = "brms_fits/situation_individual_model_fits.RData")

load(file = "brms_fits/situation_individual_model_fits.RData")
eval_trial_model_fits_situation %>%
  count(best_model) %>%
  arrange(desc(n))





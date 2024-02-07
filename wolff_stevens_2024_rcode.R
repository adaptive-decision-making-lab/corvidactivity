## ---
##
## Script name: wolff_stevens_2024_rcode.R
##
## Purpose of script: Analyze pinyon jay activity level data
##
## Authors: London Wolff (lmwolff3@gmail.com) & Jeffrey R. Stevens (jeffrey.r.stevens@gmail.com)
##
## Date Created: 2022-07-13
##
## Date Finalized: 2024-02-05
##
## License: All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0).
##  You are free to:
##   Share — copy and redistribute the material in any medium or format
##   Adapt — remix, transform, and build upon the material for any purpose, even commercially.
##  Under the following terms:
##   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests the licencor endorses you or your use.
##   No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
##
##
## ---



# Load libraries ----------------------------------------------------------

library(BayesFactor)
library(easystats)
library(formatstats)

library(papaja)
library(patchwork)
library(psych)
library(tidyverse)


# Functions ---------------------------------------------------------------

# Formats statistics from contrasts
extract_contrasts <- function(term1, term2) {
  contrast <- filter(activity_contrasts, (Level1 == term1 & Level2 == term2) | (Level1 == term2 & Level2 == term1))
  paste0("Mean difference = ", formatstats::format_num(contrast$Difference, digits = 2), ", ", 
         "_t_(", contrast$df, ") = ", formatstats::format_num(contrast$t), ", ", 
         formatstats::format_p(contrast$p, pzero = TRUE), ", ",
         "_d_ = ", formatstats::format_num(contrast$d), ", ", 
         formatstats::format_bf(contrast$bf))
}

# Creates table of within-subject confidence intervals
mywsci <- function(x) {
  ci <- summary(wsci(data = daily_behavior_wider,
                     id = "bird_pair", 
                     dv = x, 
                     factors = "phase"))
  ci <- ci |> 
    mutate(behavior = x, .before = 1)
}

# Create color-vision deficiency color pallette
cvd_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


# Activity data -----------------------------------------------------------

## Import data -----
activity_data <- read_csv("wolff_stevens_2024_data1.csv") |> 
  mutate(phase = case_when(phase == 1 ~ "Pre", 
                           phase == 2 ~ "During",
                           phase == 3 ~ "Post"),
         phase = as.factor(phase),
         phase = fct_relevel(phase, "Pre", "During", "Post"))
daily_recordings <- activity_data |> 
  count(date)

## Analyze data -----
# Fit models
activity_intercept_model <- lm(activity ~ 1, data = activity_data)
activity_phase_model <- lm(activity ~ phase, data = activity_data)
activity_time_model <- lm(activity ~ timeofday, data = activity_data)
activity_phase_time_model <- lm(activity ~ phase + timeofday, data = activity_data)

# Compare models and select best
activity_compare <- compare_performance(activity_intercept_model, activity_phase_model, activity_time_model, activity_phase_time_model)
activity_test <- test_performance(activity_intercept_model, activity_phase_model, activity_time_model, activity_phase_time_model)
activity_models <- left_join(activity_compare, activity_test)
activity_model <- eval(parse(text = activity_models$Name[which(activity_models$BF == max(activity_models$BF, na.rm = TRUE))]))
activity_model_table <- data.frame(name = c("Intercept only", "Phase only", "Time of day only", "Phase and time of day"), model = c("activity ~ 1", "activity ~ phase", "activity ~ timeofday", "activity ~ phase + timeofday"), aic = activity_models$AIC, bic = activity_models$BIC, bf = activity_models$BF)

# Check model assumptions
performance::check_model(activity_model)
check_heteroskedasticity(activity_model)
check_normality(activity_model)
summary(activity_model)

# Estimate marginal means
activity_mean <- estimate_means(activity_model, at = "phase") |> 
  mutate(date = as.Date(c("2021-02-18", "2021-02-25", "2021-03-03")))

# Calculate contrasts and effect sizes
activity_contrasts <- estimate_contrasts(activity_model, contrast = "phase")
activity_effects <- t_to_d(activity_contrasts$t, activity_contrasts$df) |> 
  rename(d_CI_low = CI_low, d_CI_high = CI_high)

# Calculate Bayesian t-tests
pre_during_data <- activity_data |> 
  filter(phase != "Post") |> 
  droplevels()
pre_post_data <- activity_data |> 
  filter(phase != "During") |> 
  droplevels()
during_post_data <- activity_data |> 
  filter(phase != "Pre") |> 
  droplevels()
pre_during_ttestbf <- ttestBF(formula = activity ~ phase, data = pre_during_data)
pre_post_ttestbf <- ttestBF(formula = activity ~ phase, data = pre_post_data)
during_post_ttestbf <- ttestBF(formula = activity ~ phase, data = during_post_data)
activity_contrasts <- bind_cols(activity_contrasts, activity_effects) |> 
  mutate(bf = c(extractBF(during_post_ttestbf)$bf, extractBF(pre_during_ttestbf)$bf, extractBF(pre_post_ttestbf)$bf))

## Plot data -----
activity_time_plot <- activity_data |> 
  ggplot(aes(x = timeofday, y = activity, color = phase, shape = phase)) +
  geom_point(size = 3, alpha = 0.75) +
  labs(x = "Time of day", y = "Mean activity level", shape = "Phase", color = "Phase") +
  scale_color_manual(values = cvd_colors[c(3, 6, 2)]) +
  scale_x_time(labels = function(x) format(as.POSIXct(x), format = '%H:%M')) +
  theme_bw(base_family = "Arial") +
  theme(panel.grid = element_blank(),
        legend.position = c(0.88, 0.155),
        legend.background = element_rect(color = "grey"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

activity_summary <- activity_data %>% 
  summarise(mean_activity = mean(activity), .by = c(date, phase))

(activity_date_plot <- activity_summary |> 
  ggplot(aes(x = date, y = mean_activity, color = phase)) +
  geom_point(aes(shape = phase), size = 1.5, alpha = 0.75) +
  geom_point(data = activity_mean, aes(x = date, y = Mean), size = 4) +
  geom_linerange(data = activity_mean, aes(x = date, ymin = CI_low, ymax = CI_high, color = phase), inherit.aes = F) +
  scale_color_manual(values = cvd_colors[c(3, 6, 2)]) + 
  labs(x = "Date", y = "Activity level") +
  scale_x_date(breaks = as_date(c("2021-02-15", "2021-02-22", "2021-03-01", "2021-03-07")),
                   labels = format(as_date(c("2021-02-15", "2021-02-22", "2021-03-01", "2021-03-07")), format = "%b %d")) +
  theme_bw(base_family = "Arial") +
  theme(panel.grid = element_blank(),
        legend.position = "",
        plot.margin = margin(2, 3, 2, 2, "mm"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)))

activity_time_plot + activity_date_plot +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave("figures/activity_time_date.png", width = 10, height = 5, scale = 0.9)


# Behavioral data  -------------------------------------------------

## Calculate inter-rater reliability -----

all_behavioral_data <- read_csv("wolff_stevens_2024_data2.csv")
reliability_data <- all_behavioral_data |> 
  filter(phase == "irr")

reliability_data_long <- reliability_data %>% 
  pivot_longer(
    cols = feeding:hopping, 
    names_to = "behavior", 
    values_to = "count"
  ) |> 
  pivot_wider(id_cols = c("id", "minute", "bird_pair", "video_number", "behavior"), names_from = initials, values_from = count)

lw_rb_icc_scores <- ICC(as.matrix(reliability_data_long[, c("LW", "RB")]))
lw_im_icc_scores <-ICC(as.matrix(reliability_data_long[, c("LW", "IM")]))
im_rb_icc_scores <-ICC(as.matrix(reliability_data_long[, c("IM", "RB")]))


## Analyze behavioral data -----

behavior_data <- all_behavioral_data |> 
  filter(phase != "irr") |> 
  droplevels() |> 
  mutate(phase = fct_relevel(phase, "Pre", "Post"))

daily_behavior <- behavior_data |> 
  summarise(across(feeding:hopping, sum), .by = c(video_number, date, phase, bird_pair))

# rare_behaviors <- daily_behavior |> 
#   group_by(phase) |> 
#   summarise(across(feeding:hopping, mean)) |> 
#   pivot_longer(feeding:hopping, names_to = "behavior", values_to = "mean") |> 
#   pivot_wider(id_cols = behavior, names_from = phase, values_from = mean) |> 
#   mutate(threshold = ifelse(Pre < 0.5 & Post < 0.5, 1, 0)) |> 
#   filter(threshold == 1) |> 
#   pull(behavior)
# 
# rare_behaviors_descriptives <- daily_behavior |> 
#   ungroup() |> 
#   summarise(across(feeding:hopping, list(mean = mean))) |> 
#   pivot_longer(feeding_mean:hopping_mean, names_to = "behavior", values_to = "mean")
# 
daily_behavior_long <- daily_behavior |> 
  summarise(across(feeding:hopping, mean), .by = c(phase, bird_pair)) |> 
  pivot_longer(cols = feeding:hopping, names_to = "behavior", values_to = "count") |> 
  # filter(!behavior %in% rare_behaviors) |> 
  mutate(behavior = gsub("_", " ", behavior),
         behavior = str_to_sentence(behavior))

# model <- lme4::lmer(count ~ phase * behavior + (1 | bird_pair), data = daily_behavior_long)
# emm <- emmeans(model, c("phase", "behavior"))

daily_behavior_wider <- pivot_wider(daily_behavior_long, 
                                    id_cols = c(bird_pair, phase), 
                                    names_from = behavior, 
                                    values_from = count)

# Calculate WSCIs for each behavior
behaviors <- names(daily_behavior_wider[, -(1:2)])

daily_behavior_wsci <- tibble(behavior = NA, phase = NA, mean = NA, lower_limit = NA, upper_limit = NA)
for (i in seq_along(behaviors)) {
  daily_behavior_wsci <- bind_rows(daily_behavior_wsci, mywsci(behaviors[i]))
}
daily_behavior_wsci <- daily_behavior_wsci[-1, ]


# Calculate BFs for behavior before and after housing change
daily_behavior_diff <- daily_behavior_long |> 
  pivot_wider(id_cols = c(bird_pair, behavior), names_from = phase, values_from = count) |> 
  mutate(diff = Post - Pre) |> 
  pivot_wider(id_cols = bird_pair, names_from = behavior, values_from = diff)

daily_behavior_bf <- daily_behavior_diff |> 
  select(-bird_pair) |> 
  map(ttestBF) |> 
  map_df(~ extractBF(.x)$bf)  |> 
  pivot_longer(everything(), names_to = "behavior", values_to = "bf")

daily_behavior_ttest <- daily_behavior_diff |> 
  select(-bird_pair) |> 
  map(t.test) |> 
  map_df(function(x) c(x[["statistic"]], x[["parameter"]], x[["p.value"]]))  |> 
  mutate(measure = c("tstatistic", "df", "pvalue")) |> 
  pivot_longer(!measure, names_to = "behavior") |> 
  pivot_wider(id_cols = behavior, names_from = measure, values_from = value)

daily_behavior_longer <- daily_behavior_long |> 
  left_join(daily_behavior_ttest, by = "behavior") |> 
  left_join(daily_behavior_bf, by = "behavior") |> 
  rowwise() |>
  mutate(bf10 = formatstats::format_bf(bf, cutoff = 10000, digits1 = 2, italics = FALSE, subscript = ""),
         p_value = formatstats::format_p(pvalue, italics = FALSE, pzero = TRUE))


## Plot data -----

# Plot behavior before and after housing change
daily_behavior_longer |> 
  ggplot(aes(x = phase, y = count, group = bird_pair)) +
  geom_line(color = "grey90") +
  geom_point(aes(x = phase, y = mean, color = phase), data = daily_behavior_wsci, inherit.aes = FALSE) +
  geom_linerange(aes(x = phase, ymin = lower_limit, ymax = upper_limit, color = phase), data = daily_behavior_wsci, inherit.aes = FALSE) +
  facet_wrap(~ behavior, scales = "free") +
  labs(x = "Phase", y = "Mean frequency") +
  scale_color_manual(values = cvd_colors[c(3, 2)]) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.35))) +
  theme_bw(base_family = "Arial") +
  theme(panel.grid = element_blank(),
        legend.position = "") +
  geom_text(x = Inf, y = Inf, label = daily_behavior_longer$p_value, hjust = 1.06, vjust = 2.75, size = 3) +
  # geom_text(x = Inf, y = Inf, label = paste("p =", sprintf("%.2f", daily_behavior_longer$pvalue)), hjust = 1.05, vjust = 2.75, size = 3) +
  # geom_text(x = Inf, y = Inf, label = paste("BF =", sprintf("%.2f", daily_behavior_longer$bf)), hjust = 1.05, vjust = 1.5, size = 3)
geom_text(x = Inf, y = Inf, label = daily_behavior_longer$bf10, hjust = 1.05, vjust = 1.5, size = 3)
ggsave("figures/behavior_freq_phase.png", width = 8, height = 8, scale = 0.8)


# Calculate bird colony age
bird_ages <- data.frame(age = c(14,12,14,14,13,14,12,14,17,12,14,12,11,10), subject = c("Fozzie", "Dartagnan", "He-man", "Uno", "Dumbledore", "Piper", "Comanche", "Sapphire", "Fern", "Prudence", "Mote", "Mork", "Mulder", "Black Elk"))


library(tidyverse)
library(dplyr)
library(stringr)


# set up ------------------------------------------------------------------



read_qualitrics <- function(path) {
  qualtrics_header <- colnames(read_csv(path, 
                                        skip = 1, 
                                        n_max = 1, 
                                        show_col_types = FALSE))
  read_csv(path, skip = 3, col_names = qualtrics_header, show_col_types = FALSE)
}

species <- c("Surf Scoter",
             "Common Murre",
             "Herring Gull",
             "Common Loon",
             "Northern Fulmar",
             "Northern Gannet")

actual_fh <- tibble(
  species,
  actual = c(0, 0.5, 13.9, 28.0, 0, 9.2)
)


# flight height direct ----------------------------------------------------


questions_dir <- c("lowest", "highest", "best", "confidence", "notes")

question_colnames_dir <- lapply(species, \(s) paste(s, questions_dir, sep = "_")) %>% 
  unlist()

direct_fh_hd <- read_qualitrics("data/raw_data/raw_survey_outputs/direct_fh_qualtrics.csv") %>% 
  # Remove test responses
  slice(-(1:3))
colnames(direct_fh_hd)[-(1:14)] <- question_colnames_dir
direct_fh_hd <- direct_fh_hd %>% 
  select(!ends_with("notes")) %>% 
  pivot_longer(cols = !(1:14),
               names_to = c("species", "parameter"),
               names_sep = "_",
               values_to = "value") %>% 
  pivot_wider(names_from = "parameter", values_from = "value") %>% 
  rename(expert_id = `Please enter your unique ID code below`)


ggplot(direct_fh_hd, aes(expert_id, best)) +
  geom_pointrange(aes(ymin = lowest, ymax = highest, color = confidence)) +
  geom_hline(aes(yintercept = actual), 
             actual_fh, 
             color = "cornflowerblue",
             linetype = "dashed",
             linewidth = 1.2) +
  scale_color_viridis_c(option = "plasma") +
  facet_grid(rows = vars(species)) +
  theme_bw(14)




# flight height indirect --------------------------------------------------


ref_ind <- c("SCAUP SP [54.5]", "COEI [0.6]", "WWSC [0.0]", "COSC [0.8]", "BLSC [0.0]", "RAZO [0.0]", "DOVE [0.0]", "BLKI [8.9]", "LAGU [4.2]", "SMALL GULL SP [9.7]", "GBBG [9.5]", "COTE-ARTE [1.2]", "SATE [4.5]", "RTLO-BTLO [12.2]", "WISP [0.0]", "COSH [0.0]", "GRSH [0.0]", "GRCO [7.3]", "notes")

question_colnames_ind <- lapply(species, \(s) paste(s, ref_ind, sep = "_")) %>% 
  unlist()


indirect_fh_hd <- read_qualitrics("data/raw_data/raw_survey_outputs/indirect_fh_qualtrics.csv") %>% 
  # Remove test responses
  slice(-(1:3))

colnames(indirect_fh_hd)[-(1:14)] <- question_colnames_ind

indirect_fh_hd <- indirect_fh_hd %>% 
  select(!ends_with("notes")) %>% 
  pivot_longer(
    cols = !(1:14),
    names_to = c("species", "ref"),
    names_sep = "_",
    values_to = "weight"
  ) %>% 
  mutate(weight = weight / 100) %>% 
  pivot_wider(names_from = "ref", values_from = "weight") %>% 
  rename(expert_id = `Please enter your unique ID code below.`) 

# Identify the weight columns (everything except species/expert_id)
weight_cols <- names(indirect_fh_hd)[grepl("\\[", names(indirect_fh_hd))]

# Extract numeric values inside brackets
bracket_vals <- str_extract(weight_cols, "(?<=\\[).*(?=\\])") %>% 
  as.numeric()

indirect_fh_hd <- indirect_fh_hd %>% 
  mutate(
    estimate = as.matrix(select(., all_of(weight_cols))) %*% bracket_vals
  )
  

ggplot(indirect_fh_hd, aes(expert_id, estimate)) +
  geom_point() +
  geom_hline(aes(yintercept = actual), 
             actual_fh, 
             color = "cornflowerblue",
             linetype = "dashed",
             linewidth = 1.2) +
  scale_color_viridis_c(option = "plasma") +
  facet_grid(rows = vars(species)) +
  theme_bw(14)





# direct-direct model  ----------------------------------------------------

#start by standardizing expert intervals to 90 - following hemming et al 2018

direct_fh_hd_standardized <- direct_fh_hd %>% 
  mutate(
    lowest_90_CI = pmax(0, (best - (best-lowest)*(90/confidence))),
    highest_90_CI = pmin(100, (best + (highest-best)*(90/confidence)))
  )

direct_fh_hd_aggregated <- direct_fh_hd_standardized %>% 
  group_by(species) %>%
  summarise(best_avg = mean(best, na.rm = TRUE),
            upper_avg = mean(highest_90_CI, na.rm = TRUE),
            lower_avg = mean(lowest_90_CI, na.rm = TRUE)
            )





ggplot(direct_fh_hd_aggregated, aes(x = species, y = best_avg)) +
  geom_pointrange(aes(ymin = lower_avg, ymax = upper_avg)) +
  geom_point(data = actual_fh, aes(x = species, y = actual),
             color = "cornflowerblue", size = 3) +
  theme_bw(14)



# direct-indirect model ---------------------------------------------------

# y ~ RV(E, V)
# link(E) = sum_i { (beta_0[i] + x_i) * beta_1[i]}
# beta_0 ~ Normal(0, sigma)
# beta_1 ~ Dirichlet(alpha)

di_df <- direct_fh_hd %>% 
  select(species, expert_id, best) %>% 
  left_join(actual_fh, by = "species") %>% 
  arrange(species, expert_id)

library(rstan)

# Assuming your data frame is called `df`

# one actual per species, preserving factor order
actual_df <- di_df[!duplicated(di_df$species), ]
actual_df <- actual_df[order(as.integer(factor(actual_df$species))), ]

stan_data <- list(
  N          = length(unique(di_df$species)),
  E          = length(unique(di_df$expert_id)),
  M          = nrow(di_df),
  obs_id     = as.integer(factor(di_df$species)),
  expert_id  = as.integer(factor(di_df$expert_id)),
  fh_best    = di_df$best / 100,
  fh_actual  = actual_df$actual / 100
)

fit <- stan(
  file    = "scratch/di_fh.stan",
  data    = stan_data,
  chains  = 4,
  iter    = 8000,
  cores   = 4,
  seed    = 42
)

library(posterior)
draws <- as_draws_df(fit)

# ── 1. Trace plots ────────────────────────────────────────────────────────────

draws_long <- draws |>
  select(.chain, .iteration, phi, sigma, sigma_gamma) |>
  pivot_longer(c(phi, sigma, sigma_gamma), names_to = "parameter", values_to = "value")

ggplot(draws_long, aes(x = .iteration, y = value, colour = factor(.chain))) +
  geom_line(alpha = 0.6, linewidth = 0.3) +
  facet_wrap(~ parameter, scales = "free_y") +
  labs(title = "Trace plots — fixed effects", colour = "Chain") +
  theme_minimal()

# ── 2. Distribution of fixed effects ─────────────────────────────────────────

fixed_long <- draws |>
  select(phi, sigma, sigma_gamma) |>
  pivot_longer(everything(), names_to = "parameter", values_to = "value")

ggplot(fixed_long, aes(x = value)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  facet_wrap(~ parameter, scales = "free") +
  labs(title = "Posterior distributions — fixed effects") +
  theme_minimal()

# ── 3. Distribution of random effects ────────────────────────────────────────

# beta_0 (per-expert intercepts)
beta0_long <- draws |>
  select(starts_with("beta_0[")) |>
  pivot_longer(everything(), names_to = "expert", values_to = "value") |>
  mutate(expert = gsub("beta_0\\[|\\]", "", expert))

ggplot(beta0_long, aes(x = value, y = expert)) +
  ggdist::stat_halfeye() +
  labs(title = "Posterior distributions — beta_0 (Beta part intercepts)", x = "value", y = "expert") +
  xlim(-6, 6) +
  theme_minimal()

# beta_1 (Dirichlet weights — Beta part)
beta1_long <- draws |>
  select(starts_with("beta_1[")) |>
  pivot_longer(everything(), names_to = "expert", values_to = "value") |>
  mutate(expert = gsub("beta_1\\[|\\]", "", expert))

ggplot(beta1_long, aes(x = value, y = expert)) +
  ggdist::stat_halfeye() +
  labs(title = "Posterior distributions — beta_1 (Beta part weights)", x = "value", y = "expert") +
  xlim(0, 0.4) +
  theme_minimal()

# gamma_0 (per-expert intercepts — hurdle part)
gamma0_long <- draws |>
  select(starts_with("gamma_0[")) |>
  pivot_longer(everything(), names_to = "expert", values_to = "value") |>
  mutate(expert = gsub("gamma_0\\[|\\]", "", expert))

ggplot(gamma0_long, aes(x = value, y = expert)) +
  ggdist::stat_halfeye() +
  labs(title = "Posterior distributions — gamma_0 (hurdle part intercepts)", x = "value", y = "expert") +
  xlim(-3, 3) +
  theme_minimal()

# gamma_1 (Dirichlet weights — hurdle part)
gamma1_long <- draws |>
  select(starts_with("gamma_1[")) |>
  pivot_longer(everything(), names_to = "expert", values_to = "value") |>
  mutate(expert = gsub("gamma_1\\[|\\]", "", expert))

ggplot(gamma1_long, aes(x = value, y = expert)) +
  ggdist::stat_halfeye() +
  xlim(0, 0.4) +
  labs(title = "Posterior distributions — gamma_1 (hurdle part weights)", x = "value", y = "expert") +
  theme_minimal()

# ── 4. Predicted vs actual ────────────────────────────────────────────────────

pred_draws <- draws |>
  select(starts_with("fh_pred[")) |>
  pivot_longer(everything(), names_to = "obs", values_to = "value") |>
  mutate(n = as.integer(gsub("fh_pred\\[|\\]", "", obs))) |>
  group_by(n) |>
  summarise(
    median = median(value),
    lo     = quantile(value, 0.055),  # 89% credible interval
    hi     = quantile(value, 0.945),
    .groups = "drop"
  )

# attach actuals (same ordering as stan_data$fh_actual)
pred_draws$actual <- stan_data$fh_actual

ggplot(pred_draws, aes(x = median, y = actual)) +
  geom_pointrange(aes(ymin = lo, ymax = hi), alpha = 0.4, linewidth = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "firebrick") +
  labs(
    title    = "Predicted vs actual",
    subtitle = "Median and 89% credible interval",
    x        = "Predicted fh",
    y        = "Actual fh"
  ) +
  theme_minimal()





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
#this shows the self reported best estimates and confidence bounds per species and expert
#plots it against the actual value in the dataset 
#also colored by how confident the expert said they were in their responses

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

species_codes <- function(x) {
  case_match(x,
             "Surf Scoter"     ~ "SUSC",
             "Common Murre"    ~ "COMU",
             "Herring Gull"    ~ "HERG",
             "Common Loon"     ~ "COLO",
             "Northern Fulmar" ~ "NOFU",
             "Northern Gannet" ~ "NOGA"
  )
}

direct_fh_hd %>%
  mutate(species = species_codes(species)) %>%
  ggplot(aes(expert_id, best)) +
  geom_pointrange(aes(ymin = lowest, ymax = highest, color = confidence),
                  linewidth = 0.8, fatten = 2) +
  geom_hline(aes(yintercept = actual),
             actual_fh %>% mutate(species = species_codes(species)),
             color = "cornflowerblue",
             linetype = "dashed",
             linewidth = 1.2) +
  scale_color_viridis_c(
    option = "plasma",
    name   = "Expert\nconfidence (%)"
  ) +
  facet_grid(rows = vars(species)) +
  labs(
    title    = "Expert direct flight height estimates by species",
    subtitle = "Dashed blue line = observed value; point ranges = expert lower/best/upper estimates",
    x        = "Expert ID",
    y        = "Flight height (% time at rotor height)"
  ) +
  theme_bw(14) +
  theme(
    strip.text       = element_text(face = "italic", size = 12),
    strip.background = element_rect(fill = "grey92", colour = NA),
    panel.spacing    = unit(0.6, "lines"),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 11, colour = "grey40"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("direct_fh_plot.png", width = 12, height = 6, dpi = 300)

# flight height indirect --------------------------------------------------
#this is by sp and expert, the point estimate that the indirect model produces 
#plotted against the actual value


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
  

indirect_fh_hd %>%
  mutate(species = species_codes(species)) %>%
  ggplot(aes(expert_id, estimate)) +
  geom_point(size = 2.5) +
  geom_hline(aes(yintercept = actual),
             actual_fh %>% mutate(species = species_codes(species)),
             color = "cornflowerblue",
             linetype = "dashed",
             linewidth = 1.2) +
  facet_grid(rows = vars(species)) +
  labs(
    title    = "Expert indirect flight height estimates by species",
    subtitle = "Dashed blue line = observed value; points = expert derived estimates",
    x        = "Expert ID",
    y        = "Flight height (% time at rotor height)"
  ) +
  theme_bw(14) +
  theme(
    strip.text       = element_text(face = "italic", size = 12),
    strip.background = element_rect(fill = "grey92", colour = NA),
    panel.spacing    = unit(0.6, "lines"),
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 11, colour = "grey40"),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )
ggsave("indirect_fh_plot.png", width = 12, height = 6, dpi = 300)



# direct-direct model  ----------------------------------------------------
#now we're actually going to see how well the direct direct method does to get close to the real values??
#this chunk just aggregates the expert responses to a single estimate with confidence across experts and then plots that against the actual value. 

#start by standardizing expert intervals to 90 - following hemming et al 2018

direct_fh_hd_standardized <- direct_fh_hd %>% 
  mutate(
    lowest_90_CI = pmax(0, (best - (best-lowest)*(90/confidence))),
    highest_90_CI = pmin(100, (best + (highest-best)*(90/confidence)))
  )

#then, calculate the arithmetic means of the upper, best, and lower values to get the new means and CIs (following Hemming et al)

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
# link(E) = sum_i {x_i * beta[i]}
# beta ~ Dirichlet(alpha)

di_df <- direct_fh_hd %>% 
  select(species, expert_id, best) %>% 
  left_join(actual_fh, by = "species") %>% 
  arrange(species, expert_id)

library(rstan)

new_zero <- 0.001
di_wide <- di_df %>% 
  pivot_wider(names_from = expert_id,
              values_from = best) %>% #since it's direct-indirect we're only using the best column in this model
  mutate(actual = pmax(actual / 100, new_zero))
di_mtx <- as.matrix(select(di_wide, -(1:2))) / 100 #this is now the matrix we feed the model
# slightly offset 0's
di_mtx[di_mtx == 0] <- new_zero

stan_data <- list(
  E = ncol(di_mtx), #experts is number columns
  M = nrow(di_mtx), #species is number of rows
  X = di_mtx,
  Y = pmax(di_wide$actual / 100, new_zero)
)

fit <- stan(
  file    = "scratch/di_fh.stan",
  data    = stan_data,
  chains  = 4,
  iter    = 10000,
  cores   = 4,
  seed    = 42
)

library(posterior)
draws <- as_draws_df(fit)

# Posterior distribution of phi (the dispersion parameter)
ggplot(draws, aes(x = phi)) +
  geom_density() +
  theme_minimal() #the plot of this is reasonable, says max 

# Posterior distribution of beta
draws %>% 
  select(starts_with("beta")) %>% 
  pivot_longer(everything(), names_to = "expert", values_to = "weight") %>% 
  mutate(expert = fct_reorder(expert, weight)) %>% 
  ggplot(aes(weight, expert)) +
  ggdist::stat_halfeye() +
  theme_minimal()

# Posterior distribution of flight height (not leave one out yet, this is the whole model - if we train the model on all the data can it predict within itself - we've already given it the right answer)
# MAX NEXT TIME
# THIS IS THE CI OF THE MEAN
# WE WANT THE PI OF THE ESTIMATE
expert_mean <- di_df %>% 
  group_by(species) %>% 
  summarize(mean_best = pmax(mean(best) / 100, new_zero))
fh_post <- draws %>% 
  select(starts_with("mu[")) %>% 
  pivot_longer(everything(), 
               names_to = "species", 
               values_to = "estimate") %>% 
  mutate(species = factor(species, labels = di_wide$species),
         species = fct_reorder(species, estimate))
ggplot(fh_post, aes(estimate, species)) +
  ggdist::stat_halfeye() +
  geom_point(aes(x = actual), 
             mutate(di_wide, species = factor(species, levels = levels(fh_post$species))),
             color = "cornflowerblue") +
  geom_point(aes(x = mean_best),
             expert_mean,
             color = "firebrick") +
  facet_grid(. ~ species, scales = "free") +
  theme_minimal()



#this is where max was starting to do LOO


## LOO (leave one out)
fit_fh_loo <- function(sp) {
  new_zero <- 0.001
  di_wide <- di_df %>% 
    filter(species != sp) %>% 
    pivot_wider(names_from = expert_id,
                values_from = best) %>% 
    mutate(actual = pmax(actual / 100, new_zero))
  di_mtx <- as.matrix(select(di_wide, -(1:2))) / 100
  # slightly offset 0's
  di_mtx[di_mtx == 0] <- new_zero
  
  stan_data <- list(
    E = ncol(di_mtx),
    M = nrow(di_mtx),
    X = di_mtx,
    Y = pmax(di_wide$actual / 100, new_zero)
  )
  
  fit <- stan(
    file    = "scratch/di_fh.stan",
    data    = stan_data,
    chains  = 4,
    iter    = 10000,
    cores   = 4,
    seed    = 42
  )
  
  fit
}

#commenting out what max wrote:

# loo_herg <- fit_fh_loo("Herring Gull")
# loo_predict <- function(loo_mod, expert_best) {
#   loo_draws <- as_draws_df(loo_mod)
#   loo_beta <- as.matrix(select(loo_draws, starts_with("beta")))
#   loo_mu_logit <- loo_beta
# }
# herg_draws <- as_draws_df(loo_herg)
# herg_beta <- as.matrix(select(herg_draws, starts_with("beta")))


#now here's claude's help completing it:
loo_predict <- function(loo_mod, expert_best) {
  # expert_best: named vector of expert estimates for the held-out species,
  # in the same expert order as the model was fit, scaled to [0,1]
  
  loo_draws <- as_draws_df(loo_mod) #draws from posterier estimates of the parameters (beta and shapes)
  
  beta_draws <- as.matrix(select(loo_draws, starts_with("beta")))
  phi_draws   <- loo_draws$phi #pulling out your concentrations (how wide is the distribution)
  
  n_draws <- nrow(beta_draws)
  
  # compute mu for the held-out species across all posterior draws
  mu_logit_draws <- beta_draws %*% qlogis(expert_best)  # [n_draws x 1]
  mu_draws       <- plogis(mu_logit_draws)              # inv_logit
  
  # draw from the Beta likelihood for each posterior sample
  Y_pred <- rbeta(
    n      = n_draws,
    shape1 = mu_draws * phi_draws,
    shape2 = (1 - mu_draws) * phi_draws
  )
  
  list(
    Y_pred  = Y_pred,
    mean  = mean(Y_pred),
    lower90 = quantile(Y_pred, 0.05),
    upper90 = quantile(Y_pred, 0.95)
  )
}


#loop over species and get results:
library(purrr)

loo_fits <- setNames(
  map(species, fit_fh_loo),
  species
)

loo_results <- map_dfr(species, function(sp) {
  # expert estimates for the held-out species, scaled to [0,1]
  expert_best <- di_df %>%
    filter(species == sp) %>%
    arrange(expert_id) %>%
    mutate(best = pmax(best / 100, new_zero)) %>%
    pull(best)
  
  pred <- loo_predict(loo_fits[[sp]], expert_best)
  
  tibble(
    species = sp,
    actual  = pmax(actual_fh$actual[actual_fh$species == sp]/100, new_zero),
    mean  = pred$mean,
    lower90 = pred$lower90,
    upper90 = pred$upper90
  )
}) %>%
  mutate(contains_actual = actual >= lower90 & actual <= upper90)

# plot
ggplot(loo_results, aes(x = species, y = mean)) +
  geom_pointrange(aes(ymin = lower90, ymax = upper90,
                      colour = contains_actual)) +
  geom_point(aes(y = actual), colour = "cornflowerblue", size = 3) +
  scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "firebrick")) +
  labs(
    title    = "LOO posterior predictive intervals vs actual flight height",
    subtitle = "90% prediction interval; blue = actual; red interval = missed",
    y        = "Flight height (proportion)",
    colour   = "Contains actual"
  ) +
  theme_bw(14)




#indirect-indirect model (using the same LOO approach as above)

ii_df <- indirect_fh_hd

fit_fh_loo_indirect <- function(sp) {
  new_zero <- 0.001
  
  ii_wide <- indirect_fh_hd %>%
    left_join(actual_fh, by = "species") %>%
    filter(species != sp) %>%
    select(species, expert_id, estimate, actual) %>%   # <-- add this
    pivot_wider(names_from = expert_id, values_from = estimate) %>%
    mutate(actual = pmax(actual / 100, new_zero))
  
  ii_mtx <- as.matrix(select(ii_wide, -(1:2))) / 100
  ii_mtx[ii_mtx == 0] <- new_zero
  
  stan_data <- list(
    E = ncol(ii_mtx),
    M = nrow(ii_mtx),
    X = ii_mtx,
    Y = pmax(ii_wide$actual / 100, new_zero)
  )
  
  stan(
    file       = "scratch/di_fh.stan",
    model_name = "ii_fh",
    data       = stan_data,
    chains     = 4,
    iter       = 10000,
    cores      = 4,
    seed       = 42
  )
}

# run LOO for all species
loo_fits_indirect <- setNames(
  map(species, fit_fh_loo_indirect),
  species
)

# generate predictions for each held-out species
loo_results_indirect <- map_dfr(species, function(sp) {
  expert_best <- indirect_fh_hd %>%
    filter(species == sp) %>%
    arrange(expert_id) %>%
    mutate(estimate = pmax(estimate / 100, new_zero)) %>%
    pull(estimate)
  
  pred <- loo_predict(loo_fits_indirect[[sp]], expert_best)
  
  tibble(
    species = sp,
    actual  = pmax(actual_fh$actual[actual_fh$species == sp]/100, new_zero),
    mean  = pred$mean,
    lower90 = pred$lower90,
    upper90 = pred$upper90
  )
}) %>%
  mutate(contains_actual = actual >= lower90 & actual <= upper90)

# plot
indirect_expert_mean <- indirect_fh_hd %>%
  group_by(species) %>%
  summarise(mean_estimate = mean(estimate, na.rm = TRUE) / 100)

ggplot(loo_results_indirect, aes(x = species, y = mean)) +
  geom_pointrange(aes(ymin = lower90, ymax = upper90,
                      colour = contains_actual)) +
  geom_point(aes(y = actual), colour = "cornflowerblue", size = 3) +
#  geom_point(data = indirect_expert_mean,
             # aes(x = species, y = mean_estimate),
             # colour = "firebrick", shape = 17, size = 3) +
  scale_colour_manual(values = c("TRUE" = "black", "FALSE" = "grey60")) +
  labs(
    title    = "Indirect LOO posterior predictive intervals vs actual flight height",
    subtitle = "90% PI (black/grey) | blue circle = actual | red triangle = expert mean",
    y        = "Flight height (proportion)",
    colour   = "Contains actual"
  ) +
  theme_bw(14)




#comparing the methods

library(tidyverse)

# ── Standardise all three methods to a common format ─────────────────────────

results_dd <- direct_fh_hd_aggregated %>%
  left_join(actual_fh, by = "species") %>%
  transmute(
    species,
    method   = "Direct-Direct",
    actual   = actual / 100,
    estimate = best_avg / 100,
    lower    = lower_avg / 100,
    upper    = upper_avg / 100
  )

results_di <- loo_results %>%
  transmute(
    species,
    method   = "Direct-Indirect LOO",
    actual,
    estimate = mean,
    lower    = lower90,
    upper    = upper90
  )

results_ii <- loo_results_indirect %>%
  transmute(
    species,
    method   = "Indirect-Indirect LOO",
    actual,
    estimate = mean,
    lower    = lower90,
    upper    = upper90
  )

all_results <- bind_rows(results_dd, results_di, results_ii)

# ── Compute per-species metrics ───────────────────────────────────────────────

all_results <- all_results %>%
  mutate(
    abs_error      = abs(estimate - actual),
    sq_error       = (estimate - actual)^2,
    contains_actual = actual >= lower & actual <= upper,
    interval_width  = upper - lower
  )

# ── Summary metrics per method ────────────────────────────────────────────────

method_metrics <- all_results %>%
  group_by(method) %>%
  summarise(
    MAE                = mean(abs_error),
    RMSE               = sqrt(mean(sq_error)),
    Coverage           = sum(contains_actual),
    Coverage_pct       = mean(contains_actual) * 100,
    Mean_interval_width = mean(interval_width),
    .groups = "drop"
  )

print(method_metrics)

# ── Per-species breakdown table ───────────────────────────────────────────────

per_species_metrics <- all_results %>%
  select(method, species, actual, estimate, lower, upper,
         abs_error, contains_actual, interval_width) %>%
  arrange(method, species)

print(per_species_metrics)

# ── Plot: MAE and RMSE ────────────────────────────────────────────────────────

method_metrics %>%
  select(method, MAE, RMSE) %>%
  pivot_longer(c(MAE, RMSE), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = method, y = value, fill = method)) +
  geom_col() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Accuracy by method", y = "Error (proportion)", x = NULL) +
  theme_bw(14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

# ── Plot: Coverage and interval width ────────────────────────────────────────

method_metrics %>%
  select(method, Coverage_pct, Mean_interval_width) %>%
  pivot_longer(c(Coverage_pct, Mean_interval_width),
               names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = method, y = value, fill = method)) +
  geom_col() +
  geom_hline(data = data.frame(metric = "Coverage_pct", value = 90),
             aes(yintercept = value),
             linetype = "dashed", colour = "firebrick") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "Calibration by method", y = NULL, x = NULL) +
  theme_bw(14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))










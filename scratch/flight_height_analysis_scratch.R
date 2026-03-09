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

new_zero <- 0.001
di_wide <- di_df %>% 
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

library(posterior)
draws <- as_draws_df(fit)

# Posterior distribution of phi (the dispersion parameter)
ggplot(draws, aes(x = phi)) +
  geom_density() +
  theme_minimal()

# Posterior distribution of beta
draws %>% 
  select(starts_with("beta")) %>% 
  pivot_longer(everything(), names_to = "expert", values_to = "weight") %>% 
  mutate(expert = fct_reorder(expert, weight)) %>% 
  ggplot(aes(weight, expert)) +
  ggdist::stat_halfeye() +
  theme_minimal()

# Posterior distribution of flight height
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
loo_herg <- fit_fh_loo("Herring Gull")
loo_predict <- function(loo_mod, expert_best) {
  loo_draws <- as_draws_df(loo_mod)
  loo_beta <- as.matrix(select(loo_draws, starts_with("beta")))
  loo_mu_logit <- loo_beta
}
herg_draws <- as_draws_df(loo_herg)
herg_beta <- as.matrix(select(herg_draws, starts_with("beta")))


library(tidyverse)

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

questions <- c("lowest", "highest", "best", "confidence", "notes")

question_colnames <- lapply(species, \(s) paste(s, questions, sep = "_")) %>% 
  unlist()

direct_fh_hd <- read_qualitrics("data/raw_data/raw_survey_outputs/direct_fh_qualtrics.csv") %>% 
  # Remove test responses
  slice(-(1:3))
colnames(direct_fh_hd)[-(1:14)] <- question_colnames
direct_fh_hd <- direct_fh_hd %>% 
  select(!ends_with("notes")) %>% 
  pivot_longer(cols = !(1:14),
               names_to = c("species", "parameter"),
               names_sep = "_",
               values_to = "value") %>% 
  pivot_wider(names_from = "parameter", values_from = "value") %>% 
  rename(expert_id = `Please enter your unique ID code below`)

actual_fh <- tibble(
  species,
  actual = c(0, 0.5, 13.9, 28.0, 0, 9.2)
)

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


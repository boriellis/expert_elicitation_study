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



  







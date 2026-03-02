library(tidyverse)
library(likert)


# SPATIAL -----------------------------------------------------------------


#get headers from qualtrics
header <- read_csv("data/raw_data/raw_survey_outputs/spatial_reflection_qualtrics.csv", skip = 1, n_max = 1, show_col_types = FALSE)

# Read the actual data starting from row 4, using proper column names
survey <- read_csv("data/raw_data/raw_survey_outputs/spatial_reflection_qualtrics.csv",
                          skip = 3,
                          col_names = colnames(header),
                          show_col_types = FALSE) %>% 
  select(-c(`Recipient Last Name`, `Recipient First Name`, `Recipient Email`, `External Data Reference`)) %>% 
  rename(ID = `Please enter your unique identifier below`)

# Identify metadata columns
metadata_cols <- c("Start Date", "End Date", "Progress", "Duration (in seconds)", "Finished", "Recorded Date", "Response ID", "Distribution Channel", "User Language", "ID")

# Convert all non-metadata columns to character
survey <- survey %>%
  mutate(across(-all_of(metadata_cols), as.character))


likert_levels <- c("Strongly favor Survey 1 (direct method)", 
                   "Somewhat favor Survey 1 (direct method)", 
                   "No difference between methods", 
                   "Somewhat favor Survey 2 (indirect method)", 
                   "Strongly favor Survey 2 (indirect method)")

survey_spatial <- survey %>% 
  mutate(across(c(`In which method did you feel more confident in the estimates you produced?`, 
                  `Which method was easier and more intuitive for you to use?`, 
                  `Which method better allowed you to effectively apply your expertise?`),
         ~ factor(.x, levels = likert_levels))) 

likert_cols <- survey_spatial %>% 
  select(
    `In which method did you feel more confident in the estimates you produced?`,
    `Which method was easier and more intuitive for you to use?`,
    `Which method better allowed you to effectively apply your expertise?`
  ) %>% 
  as.data.frame()

likert_spatial <- likert(likert_cols)

plot(likert_spatial,
     centered = TRUE, 
     panel.strip.color = "#dbd2ea") + 
  scale_fill_manual(values = c("#f29da6", 
                               "#fab084", 
                               "#fbe095",  
                               "#c9e6c9",  
                               "#9bd1c9", 
                               "#a0c695"), 
                    breaks = likert_levels) + 
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "#fff9ed"), 
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "#fff9ed"))



# flight height -----------------------------------------------------------


#get headers from qualtrics
header_fh <- read_csv("data/raw_data/raw_survey_outputs/fh_reflection_qualtrics.csv", skip = 1, n_max = 1, show_col_types = FALSE)

# Read the actual data starting from row 4, using proper column names
survey_fh <- read_csv("data/raw_data/raw_survey_outputs/fh_reflection_qualtrics.csv",
                   skip = 3,
                   col_names = colnames(header),
                   show_col_types = FALSE) %>% 
  select(-c(`Recipient Last Name`, `Recipient First Name`, `Recipient Email`, `External Data Reference`)) %>% 
  rename(ID = `Please enter your unique identifier below`)

# Convert all non-metadata columns to character
survey_fh <- survey_fh %>%
  mutate(across(-all_of(metadata_cols), as.character))

likert_levels <- c("Strongly favor Survey 1 (direct method)", 
                   "Somewhat favor Survey 1 (direct method)", 
                   "No difference between methods", 
                   "Somewhat favor Survey 2 (indirect method)", 
                   "Strongly favor Survey 2 (indirect method)")

survey_fh <- survey_fh %>% 
  mutate(across(c(`In which method did you feel more confident in the estimates you produced?`, 
                  `Which method was easier and more intuitive for you to use?`, 
                  `Which method better allowed you to effectively apply your expertise?`),
                ~ factor(.x, levels = likert_levels))) 

likert_cols_fh <- survey_fh %>% 
  select(
    `In which method did you feel more confident in the estimates you produced?`,
    `Which method was easier and more intuitive for you to use?`,
    `Which method better allowed you to effectively apply your expertise?`
  ) %>% 
  as.data.frame()

likert_fh <- likert(likert_cols_fh)

plot(likert_fh,
     centered = TRUE, 
     panel.strip.color = "#dbd2ea") + 
  scale_fill_manual(values = c("#f29da6", 
                               "#fab084", 
                               "#fbe095",  
                               "#c9e6c9",  
                               "#9bd1c9", 
                               "#a0c695"), 
                    breaks = likert_levels) + 
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "#fff9ed"), 
        legend.title = element_blank(), 
        legend.background = element_rect(fill = "#fff9ed"))



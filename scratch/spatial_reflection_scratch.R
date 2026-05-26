library(tidyverse)
library(likert)

# Shared config -----------------------------------------------------------

metadata_cols <- c(
  "Start Date", "End Date", "Progress", "Duration (in seconds)",
  "Finished", "Recorded Date", "Response ID", "Distribution Channel",
  "User Language", "ID"
)

likert_levels <- c(
  "Strongly favor Survey 1 (direct method)",
  "Somewhat favor Survey 1 (direct method)",
  "No difference between methods",
  "Somewhat favor Survey 2 (indirect method)",
  "Strongly favor Survey 2 (indirect method)"
)

likert_questions <- c(
  "In which method did you feel more confident in the estimates you produced?",
  "Which method was easier and more intuitive for you to use?",
  "Which method better allowed you to effectively apply your expertise?"
)

sp_preference_question <- "If asked to participate in a future expert elicitation exercise for species distribution estimates, which elicitation method would you prefer to use?"
fh_preference_question <- "If asked to participate in a future expert elicitation exercise for flight height estimates, which elicitation method would you prefer to use?"

preference_levels <- c("Direct method", "No preference", "Indirect method")

likert_fill_colours <- c(
  "#f29da6",  # Strongly favor Survey 1
  "#fab084",  # Somewhat favor Survey 1
  "#fbe095",  # No difference
  "#c9e6c9",  # Somewhat favor Survey 2
  "#9bd1c9"   # Strongly favor Survey 2
)

# Reuse the outer + middle colours from the main palette to signal equivalence
preference_fill_colours <- c(
  "#f29da6",  # Direct method   (mirrors "Strongly favor Survey 1")
  "#fbe095",  # No preference   (mirrors "No difference")
  "#9bd1c9"   # Indirect method (mirrors "Strongly favor Survey 2")
)

shared_theme <- theme(
  panel.background  = element_rect(fill = "white"),
  plot.background   = element_rect(fill = "#fff9ed"),
  legend.title      = element_blank(),
  legend.background = element_rect(fill = "#fff9ed")
)


# Helper functions --------------------------------------------------------

#' Read and clean a Qualtrics survey CSV
read_qualtrics <- function(path) {
  header <- read_csv(path, skip = 1, n_max = 1, show_col_types = FALSE)
  
  read_csv(path, skip = 3, col_names = colnames(header), show_col_types = FALSE) %>%
    select(-c(
      `Recipient Last Name`, `Recipient First Name`,
      `Recipient Email`, `External Data Reference`
    )) %>%
    rename(ID = `Please enter your unique identifier below`) %>%
    mutate(across(-all_of(metadata_cols), as.character))
}

#' Build the main 5-level likert plot (3 questions)
make_likert_plot <- function(survey,
                             questions     = likert_questions,
                             levels        = likert_levels,
                             strip_colour  = "#dbd2ea",
                             fill_colours  = likert_fill_colours) {
  questions_present <- intersect(questions, colnames(survey))
  
  likert_cols <- survey %>%
    mutate(across(all_of(questions_present), ~ factor(.x, levels = levels))) %>%
    select(all_of(questions_present)) %>%
    as.data.frame()
  
  plot(likert(likert_cols), centered = TRUE, panel.strip.color = strip_colour) +
    scale_fill_manual(values = fill_colours, breaks = levels) +
    shared_theme
}

#' Build the 3-level preference bar as a plain ggplot
make_preference_plot <- function(survey,
                                 question     = preference_question,
                                 levels       = preference_levels,
                                 fill_colours = preference_fill_colours) {
  plot_data <- survey %>%
    select(response = all_of(question)) %>%
    filter(!is.na(response)) %>%
    mutate(response = factor(response, levels = levels)) %>%
    count(response) %>%
    mutate(pct = n / sum(n) * 100)
  
  ggplot(plot_data, aes(x = pct, y = "", fill = fct_rev(response))) +
    geom_col(position = "stack") +
    geom_text(
      aes(label = ifelse(pct >= 5, paste0(round(pct), "%"), "")),
      position = position_stack(vjust = 0.5),
      size = 3.5, colour = "grey30"
    ) +
    scale_fill_manual(
      values = setNames(rev(fill_colours), rev(levels)),
      breaks = levels,
      drop   = FALSE
    ) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      y    = str_wrap(question, width = 40),
      x    = NULL,
      fill = NULL
    ) +
    theme_minimal() +
    shared_theme +
    theme(
      axis.title.y     = element_text(size = 10, colour = "black", angle = 0,
                                      vjust = 0.5, hjust = 0),
      axis.text.y      = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

#' Save a plot as PNG
save_plot <- function(plot, path, width = 10, height = 6, dpi = 300) {
  ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  message("Saved: ", path)
}


# Spatial -----------------------------------------------------------------

survey_spatial <- read_qualtrics(
  "data/raw_data/raw_survey_outputs/spatial_reflection_qualtrics.csv"
)

save_plot(make_likert_plot(survey_spatial),     "output/likert_spatial.png")
save_plot(make_preference_plot(survey_spatial,
                               question = sp_preference_question), "output/preference_spatial.png",
          width = 8, height = 3)


# Flight height -----------------------------------------------------------

survey_fh <- read_qualtrics(
  "data/raw_data/raw_survey_outputs/fh_reflection_qualtrics.csv"
)

save_plot(make_likert_plot(survey_fh),          "output/likert_flight_height.png")
save_plot(make_preference_plot(survey_fh,
                               question = fh_preference_question), "output/preference_flight_height.png",
          width = 8, height = 3)

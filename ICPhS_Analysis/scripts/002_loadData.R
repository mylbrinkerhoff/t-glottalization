#----------------------------------------------------------------------------------------
# File: 002_loadData.R
# Project: t-glottalization
# Author: Mykel Brinkerhoff
# Date: 2026-09-15 (T)
# Description: What does this script do?
#   - Loads in the data and prepares it analysis
#
# Usage:
#   Rscript 002_loadData.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.
#----------------------------------------------------------------------------------------

icphs <- readr::read_csv(
  # Read in the file
  here::here(
    "ICPhs_Analysis",
    "data",
    "raw",
    "2027icphs_data.csv"
  )
) |>
  dplyr::filter(
    # Filter for the t-glottalization tokens that are valid for analysis
    icphs_analysis == "yes" & variable == "t_glottalization"
  ) |>
  dplyr::filter(
    # Remove the speakers that only produced t^h or creaky voice
    !(speaker_id %in% c("007", "014", "018"))
  ) |>
  dplyr::mutate(
    # Create variables for logistic regression
    glottal = dplyr::case_when(
      realization %in% c("glottal", "creaky") ~ TRUE,
      .default = FALSE
    ),
    stop = dplyr::case_when(
      realization == "creaky" ~ FALSE,
      .default = TRUE
    ),
    list = dplyr::case_when(
      reading_list %in% c("A1", "A2") ~ "A",
      reading_list %in% c("B1", "B2") ~ "B",
      reading_list == "C" ~ "C",
      reading_list == "D" ~ "D"
    )
  )

icphs <- icphs |>
  dplyr::mutate(
    # Factor the variables for treatment coding and analysis
    reading_list = factor(reading_list),
    list = factor(list),
    information_load = factor(
      information_load,
      levels = c("low", "high")
    ),
    speaker_id = factor(speaker_id),
    target_word = factor(target_word)
  )

# Sum coding the information load
(contrasts(icphs$information_load) <- contr.sum(2))

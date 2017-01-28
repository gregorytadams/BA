library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

get_data <- function(file_path){
  raw_data <- read.csv(file_path)
  # View(raw_data)
  cat("Number of rows in raw data:", nrow(raw_data), "\n")
  return(raw_data)
}

filter_data <- function(raw_data) { 
  df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
    filter(days_b_screening_arrest <= 30) %>%
    filter(days_b_screening_arrest >= -30) %>%
    filter(is_recid != -1) %>%
    filter(c_charge_degree != "O") %>%
    filter(score_text != 'N/A')
  cat("Number of rows after filter:", nrow(df), "\n")
  # View(df)
  return(df)
}

get_demographics <- function(df){
  cat("Demographics: \n")
  df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))
  cat("Correlation between length of stay and score: ", cor(df$length_of_stay, df$decile_score), "\n")
  cat("Racial Breakdown:\n")
  cat("    Black defendants: ",(3175 / 6172 * 100), "\n")
  cat("    White defendants: ",(2103 / 6172 * 100), "\n")
  cat("    Hispanic defendants: ", (509 / 6172 * 100), "\n")
  cat("    Asian defendants: ",(31 / 6172 * 100), "\n")
  cat("    Native American defendants: ",  (11 / 6172 * 100), "\n")
  cat("Scores:", summary(df$score_text), "\n")
  cat(xtabs(~ sex + race, data=df))
  cat("Men:",  (4997 / 6172 * 100), "\n")
  cat("Women:", (1175 / 6172 * 100),"\n")
}

file_path <- "~/Documents/compas-analysis/compas-scores-two-years.csv"
raw_data <- get_data(file_path)
df <- filter_data(raw_data)
# get_demographics(df)



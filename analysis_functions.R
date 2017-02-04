# This file walks through and comments on the ProPublica analysis. 
# I have heavily modified much of the code.

library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

FILEPATH_NORMAL <-  "~/BA/compas-analysis/compas-scores-two-years.csv"
FILEPATH_VIOLENT <- "~/BA/compas-analysis/compas-scores-two-years-violent.csv"

# Set up and clean data:
    
    get_data <- function(file_path){
        raw_data <- read.csv(file_path)
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
    
    filter_data_violent <- function(raw_data) {
        df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, v_score_text, sex, priors_count, 
                    days_b_screening_arrest, v_decile_score, is_recid, two_year_recid) %>% 
        filter(days_b_screening_arrest <= 30) %>%
        filter(days_b_screening_arrest >= -30) %>% 
        filter(is_recid != -1) %>%
        filter(c_charge_degree != "O") %>%
        filter(v_score_text != 'N/A')
    cat("Number of rows after filter:", nrow(df), "\n")
    return(df)
    }

    get_demographics_normal <- function(df){
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


# End Set up and clean data

# Start Exploratory Analysis
    
    plot_decile_scores_normal <- function(df) {
        pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) + 
                  geom_bar() + xlab("Decile Score") +
                  ylim(0, 650) + ggtitle("Black Defendant's Decile Scores")
        pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) + 
                  geom_bar() + xlab("Decile Score") +
                  ylim(0, 650) + ggtitle("White Defendant's Decile Scores")
        grid.arrange(pblack, pwhite,  ncol = 2)
    }

    plot_decile_scores_violent <- function(df){
        pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(v_decile_score))) + 
                  geom_bar() + xlab("Violent Decile Score") +
                  ylim(0, 700) + ggtitle("Black Defendant's Violent Decile Scores")
        pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(v_decile_score))) + 
                  geom_bar() + xlab("Violent Decile Score") +
                  ylim(0, 700) + ggtitle("White Defendant's Violet Decile Scores")
        grid.arrange(pblack, pwhite,  ncol = 2)
    }

    get_logistic_regression_normal_compas <- function(df){
        df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
                    mutate(age_factor = as.factor(age_cat)) %>%
                    within(age_factor <- relevel(age_factor, ref = 1)) %>%
                    mutate(race_factor = factor(race)) %>%
                    within(race_factor <- relevel(race_factor, ref = 3)) %>%
                    mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
                    within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
                    mutate(score_factor = factor(score_text == "High", 
                    labels = c("LowScore","HighScore")))
        model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
                                     priors_count + crime_factor + two_year_recid, family="binomial", data=df)
        print(summary(model))
        return(model)
    }
    
    get_logistic_regression_violent <- function(df){
        df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
              mutate(age_factor = as.factor(age_cat)) %>%
              within(age_factor <- relevel(age_factor, ref = 1)) %>%
              mutate(race_factor = factor(race,
                                          labels = c("African-American", 
                                                     "Asian",
                                                     "Caucasian", 
                                                     "Hispanic", 
                                                     "Native American",
                                                     "Other"))) %>%
              within(race_factor <- relevel(race_factor, ref = 3)) %>%
              mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
              within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
              mutate(score_factor = factor(v_score_text != "Low", labels = c("LowScore","HighScore")))
        model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
                                    priors_count + crime_factor + two_year_recid, family="binomial", data=df)
        print(summary(model))
        return(model)
    }




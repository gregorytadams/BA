# Much of this file comes from ProPublica's analysis.
# I have heavily modified much of that code and written much myself.

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
        pblack <- ggplot(data=filter(df, race =="Hispanic"), aes(ordered(decile_score))) + 
                  geom_bar() + xlab("Decile Score") +
                  ylim(0, 325) + ggtitle("Hispanic Defendant's Decile Scores")
        pwhite <- ggplot(data=filter(df, race == "Asian"), aes(ordered(decile_score))) + 
                  geom_bar() + xlab("Decile Score") +
                  ylim(0, 325) + ggtitle("Asian Defendant's Decile Scores")
        pother <- ggplot(data=filter(df, race == "Other" | race == "Native American"), aes(ordered(decile_score))) + 
                  geom_bar() + xlab("Decile Score") +
                  ylim(0, 325) + ggtitle("Other Defendant's Decile Scores")
        grid.arrange(pblack, pwhite, pother, ncol = 3)
    }

    plot_decile_scores_violent <- function(df){
        pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(v_decile_score))) + 
                  geom_bar() + xlab("Violent Decile Score") +
                  ylim(0, 700) + ggtitle("Black Defendant's Violent Decile Scores")
        pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(v_decile_score))) + 
                  geom_bar() + xlab("Violent Decile Score") +
                  ylim(0, 700) + ggtitle("White Defendant's Decile Scores")
        pother <- ggplot(data=filter(df, race != "Caucasian" & race != "African-American"), aes(ordered(v_decile_score))) + 
                  geom_bar() + xlab("Violent Decile Score") +
                  ylim(0, 700) + ggtitle("Other Defendant's Decile Scores")
        grid.arrange(pblack, pwhite, pother,  ncol = 3)
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
    
  bootstrap_t_test <- function(df, num_samples, size_sample, races){
    means1 <- c()
    means2 <- c()
    for (i in 1:num_samples){
        m <- sample(df$decile_score[df$race == races[1]], size=size_sample, replace=TRUE)
        m <- mean(m)
        means1 <- c(means1, m)
    }
    for (i in 1:num_samples){
      m <- mean(sample(df$decile_score[df$race == races[2]], size=size_sample, replace=TRUE))
      means2 <- c(means2, m)
    }
    t = t.test(means1, means2)
    return(t)
  }

  bootstrap_for_all_races <- function(df, num_samples, size_sample){
    values <- list()
    counter <- 0
    for (i in 1:length(unique(df$race))){
      print("here")
      for (j in 1:length(unique(df$race))){
        r1 <- as.vector(unique(df$race))[i]
        r2 <- as.vector(unique(df$race))[j]
         test <- bootstrap_t_test(df, num_samples, size_sample, c(r1, r2))
         if (test$p.value < 0.05){
           counter <- counter + 1 
           values[[counter]] = c(r1, r2, test$p.value, test$conf.int, test$x, test$y)
         }
      }
    }
    return(values)
  }
  
  
  


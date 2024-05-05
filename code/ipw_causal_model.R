# load the required libraries
library(ggplot2)
library(boot)
library(pscl)
library(glmnet)
library(caTools)
library(pROC)
library(broom)
library(halfmoon)
library(propensity)
library(dplyr)

# load the csv data file
df <- read.csv("data/preprocessed/inference_base.csv")
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)
df$child_attend_kindergarten_flag <- ifelse(df$child_attend_kindergarten == "yes", 1, 0)

cat_ftrs <- c("child_attend_kindergarten","povstatus", "child_gender_w1",
              "parents_edustatus_w1","mother_fin_security_w2345", "father_in_jail_w1",
               "cm1ethrace_w1")
num_ftrs <- c("hs_avg_score", "mother_mothly_pretax_inc_w1")
ftrs <- c(cat_ftrs, num_ftrs)
target <- c("college_grad_flag")
treatment <- c("child_attend_kindergarten_flag")

preprocess <- function(df, cat_ftrs, num_ftrs){
  for(feature in cat_ftrs){
    df[[feature]] <- as.factor(df[[feature]])
  }
  for(feature in num_ftrs){
    df[[feature]] <- scale(df[[feature]])
  }
  return(df)
}

model_df <- df[,c(target, treatment, ftrs)]
model_df <- model_df[complete.cases(model_df),]

propmodel_df <- glm(child_attend_kindergarten_flag ~ mother_mothly_pretax_inc_w1 + parents_edustatus_w1 + cm1ethrace_w1, 
                        family = "binomial", data = model_df) |>
  augment(type.predict = "response", data = model_df) |>
  mutate(w_ate = wt_ate(.fitted, child_attend_kindergarten_flag))

propensity_plot <- ggplot(propmodel_df, 
                          aes(.fitted, fill = factor(child_attend_kindergarten_flag))) +
  geom_mirror_histogram(bins = 15) +
  scale_y_continuous(labels = abs) +
  labs(x = "propensity score", fill = "child_attend_kindergarten_flag")
print(propensity_plot)

smds <- tidy_smd(propmodel_df, .vars = c(mother_mothly_pretax_inc_w1, parents_edustatus_w1, cm1ethrace_w1), 
                 .group = child_attend_kindergarten_flag, .wts = w_ate)
print(smds)


outcome_model <- glm(college_grad_flag ~ factor(child_attend_kindergarten_flag), 
                     data = propmodel_df, weights = w_ate, 
                     family = "binomial")
print(summary(outcome_model))


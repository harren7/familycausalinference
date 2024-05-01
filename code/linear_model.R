# load the required libraries
library(ggplot2)

# load the csv data file
df <- read.csv("data/preprocessed/inference_base.csv")

# linear regression
lm_model <- lm(hs_avg_score ~ child_edu_level + mother_mothly_pretax_inc_w1 +
               parents_edustatus_w1 + mother_fin_security_w2345, data = df)

# new target variable 
# 1: Completed college
# 0: else
df$target <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)

# create df_low_inc
df_low_inc <- df[df$povstatus == "low-income",]

# logistic regression
glm_model <- glm(target ~ child_attend_kindergarten +
                 parents_edustatus_w1 + mother_fin_security_w2345 + povstatus +
                 hs_avg_score, data = df, family = binomial)

summary(glm_model)
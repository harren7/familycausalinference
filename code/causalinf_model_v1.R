library(haven)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(scales)
library(broom)

set.seed(2020)

## Read the dataset
df <- read.csv("data/preprocessed/inference_base.csv")
# custom_format <- function(x) {
#   ifelse(x == floor(x), as.integer(x), format(x, nsmall = 1))
# }

## Set up treatement & target variable
df$treatment <- as.factor(ifelse(df$child_attend_kindergarten == "yes", 1, 0))
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)

## Check number of NAs in child_edu_level and child_attend_kindergarten
sum(is.na(df$child_edu_level))
sum(is.na(df$child_attend_kindergarten))

## Remove rows with missing values in child_edu_level and child_attend_kindergarten
df <- df[!is.na(df$child_attend_kindergarten),]
df <- df[!is.na(df$child_edu_level),]

# Count the number of treatment (Child goes to kindergarten = Yes) and control (Child goes to kindergarten = No) group elements
table(df$child_attend_kindergarten)

# Looking at the %s of each subgroup 
## For example, a child that does not attend kindergarten or college forms ~60% of the control group 
df %>% 
  count(treatment, college_grad_flag) %>% 
  group_by(treatment) %>% 
  mutate(percent_by_group = round(100 * n / sum(n), digits = 1))

# Create a feature list - taken from linear_model.R
ftrs <- c("treatment", "povstatus", "child_gender_w1", "parents_edustatus_w1",
          "hs_avg_score", "mother_fin_security_w2345", "father_in_jail_w1", "college_grad_flag")

# Subsetting dataframes for high and low income groups
model_df <- df[ftrs]
model_df_high_income <- filter(model_df, povstatus == 'high-income')
model_df_low_income <- filter(model_df, povstatus == 'low-income')


# ANOVA model 
## The treatment variable is the sole predictor of probability of probability on the log-odds scale
glm_full <- glm(
  data = model_df,
  family = binomial,
  college_grad_flag ~ treatment
)

glm_high_income <- glm(
  data = model_df_high_income,
  family = binomial,
  college_grad_flag ~ treatment
)

glm_low_income <- glm(
  data = model_df_low_income,
  family = binomial,
  college_grad_flag ~ treatment
)

# beta_0 should be the mean for those in the control group 
# beta_1 is the difference in the mean between the treatment and control group

summary(glm_full)
summary(glm_high_income)
summary(glm_low_income)

# Average Treatement Effect - 
# the likelihood of a child graduating college is 6% higher for those who do attend kindergarten than those who do not
##### plogis() - obtain the probability for a given log-odds value
plogis(coef(glm_full)[1] + coef(glm_full)[2]) - plogis(coef(glm_full)[1])
plogis(coef(glm_high_income)[1] + coef(glm_high_income)[2]) - plogis(coef(glm_high_income)[1])
plogis(coef(glm_low_income)[1] + coef(glm_low_income)[2]) - plogis(coef(glm_low_income)[1])

nd <- tibble(treatment = 0:1)
nd$treatment <- as.factor(nd$treatment)

# The probability of college graduation for each group

# For the full dataset
full_output <- predict(glm_full, 
                       newdata = nd,
                       se.fit = TRUE,
                       type = "response") %>% 
  data.frame() %>% 
  bind_cols(nd)

# For the high income group
high_inc_output <- predict(glm_high_income, 
                           newdata = nd,
                           se.fit = TRUE,
                           type = "response") %>% 
  data.frame() %>% 
  bind_cols(nd)

high_inc_output$group <- "High Income"

# For the low income group
low_inc_output <- predict(glm_low_income, 
                          newdata = nd,
                          se.fit = TRUE,
                          type = "response") %>% 
  data.frame() %>% 
  bind_cols(nd)

low_inc_output$group <- "Low Income"

# Merge high income and low income output in a dataframe with group variable
hl_inc_output <- rbind(high_inc_output, low_inc_output)

# create a column of maximum values of each group
hl_inc_output$max_per_group <- ave(hl_inc_output$fit, hl_inc_output$group, FUN = function(x) max(as.numeric(x)))

# Plot the probability of college completion in group bar plot for high income and low income
ggplot(hl_inc_output, aes(x = group, y = fit, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_errorbar(aes(ymin = ifelse(fit == max_per_group, NA, fit), ymax = ifelse(fit == max_per_group, NA, max_per_group)),
                position = position_dodge(width = 0.6), width = 0.2) +
  labs(title = "Probability of College Completion by Kindergarten Attendance",
       x = "Income Group",
       y = "Probability of College Completion") +
  scale_fill_discrete(name = "", labels = c("Control", "Treatment")) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(label = ifelse(max_per_group == fit, NA, scales::percent(max_per_group - fit, accuracy = 0.1))), hjust = 1.2, vjust = ifelse(hl_inc_output$group == "Low Income", -4.7, -6.5), position = position_dodge(width = 0.6)) +
  theme_minimal()

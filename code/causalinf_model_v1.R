library(haven)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(scales)
library(broom)

set.seed(1)

## Read the dataset
df <- read.csv("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/familycausalinference/data/preprocessed/inference_base.csv")
custom_format <- function(x) {
  ifelse(x == floor(x), as.integer(x), format(x, nsmall = 1))
}

table(df$child_attend_kindergarten) # Child attend kindergarten has no NAs
df <- df[!is.na(df$child_attend_kindergarten),]

## Set up target variable
df$treatment <- ifelse(df$child_attend_kindergarten == "yes", 1, 0)
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)

# Count the number of treatment (Child goes to kindergarten = Yes) and control (Child goes to kindergarten = No) group elements
table(df$child_attend_kindergarten)

# Looking at the %s of each subgroup 
## For example, a child that does not attend kindergarten or college forms ~60% of the control group 
df %>% 
  count(treatment, college_grad_flag) %>% 
  group_by(treatment) %>% 
  mutate(percent_by_group = round(100 * n / sum(n), digits = 1))

# Let's look at a subset with no missing values in the target variable
data_subset <- subset(df, df$college_grad_flag %in% c(0,1))
dim(data_subset)

# Create a feature list - taken from linear_model.R
ftrs <- c("treatment", "povstatus", "child_gender_w1", "parents_edustatus_w1",
          "hs_avg_score", "mother_fin_security_w2345", "father_in_jail_w1", "college_grad_flag")

model_data <- data_subset[ftrs]
model_data <- filter(model_data, povstatus == 'high-income')
dim(model_data)

# ANOVA model 
## The treatment variable is the sole predictor of probability of probability on the log-odds scale
glm1 <- glm(
  data = model_data,
  family = binomial,
  college_grad_flag ~ treatment
)

# beta_0 should be the mean for those in the control group 
# beta_1 is the difference in the mean between 

summary(glm1)

# Average Treatement Effect - 
# the likelihood of a child graduating college is 6% higher for those who do attend kindergarten than those who do not
##### plogis() - obtain the probability for a given log-odds value
plogis(coef(glm1)[1] + coef(glm1)[2]) - plogis(coef(glm1)[1])

nd <- tibble(treatment = 0:1)
predict(glm1, 
        newdata = nd,
        se.fit = TRUE,
        type = "response") %>% 
  data.frame() %>% 
  bind_cols(nd)



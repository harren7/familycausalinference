############################### PRE-DEFINED FUNCTIONS/SETTINGS ###############################
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

df <- read.csv("data/preprocessed/inference_base.csv")
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)
df$child_attend_kindergarten_flag <- ifelse(df$child_attend_kindergarten == "yes", 1, 0)

# df <- df[df$povstatus=="high-income",] ## toggle as necessary

cat_ftrs <- c("child_attend_kindergarten", "child_gender_w1",
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

############################### PROPENSITY MODEL ###############################

propmodel <- glm(child_attend_kindergarten_flag ~ mother_mothly_pretax_inc_w1 + parents_edustatus_w1 + 
                   cm1ethrace_w1, family = "binomial", data = model_df) 
print(summary(propmodel))
propmodel_df <- propmodel |> augment(type.predict = "response", data = model_df) |>
  mutate(w_ate = wt_ate(.fitted, child_attend_kindergarten_flag))


smds <- tidy_smd(propmodel_df, .vars = c(mother_mothly_pretax_inc_w1, parents_edustatus_w1, cm1ethrace_w1), 
                 .group = child_attend_kindergarten_flag, .wts = w_ate)
print(smds)

propensity_plot <- ggplot(propmodel_df, 
                          aes(.fitted, group = factor(child_attend_kindergarten))) +
  geom_mirror_histogram(bins = 15) +
  geom_mirror_histogram(aes(fill = factor(child_attend_kindergarten), weight=w_ate), 
                        bins = 15,  alpha=0.5) +
  scale_y_continuous(labels = abs) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8)) +
  labs(x = "Propensity score", fill = "Kindergarten?") +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8), 
        axis.text.x = element_text(size=8),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.10, 0.12), legend.text = element_text(size=9, hjust=0.5), 
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.just = "right",
        strip.text.x = element_text(size = 9),
        title = element_text(size=11)) +
  ggtitle("Distribution of propensity scores between treatment groups")
print(propensity_plot)
ggsave("data/plots/causal-propensity-plot.png", 
       dpi=300, width=2000, height=1200, units="px", bg="transparent")

# Description of the propensity plot:
# In this mirrored histogram, the dark bars represent the unweighted distribution 
# and the lighter bars represent the distribution weighted by the ATE weights


############################### OUTCOME MODEL ###############################


outcome_model <- glm(college_grad_flag ~ factor(child_attend_kindergarten_flag), 
                     data = propmodel_df, weights = w_ate, 
                     family = "binomial")
print(summary(outcome_model))

# Interpretation of outcome model:
# The coefficient value marks the increase in the log-odds of attending kindergarten 
# from the base value, i.e. as represented by the intercept. This is the ATE. 
# After some calculation, we can estimate the ATE for different scenarios as:
# 1. Low-income: 1.51% -> 5.739% => ATE = 4.229%
# 2. High-income: 9.43% -> 17.17% => ATE = 7.75%
# 3. Overall: 5.52% -> 11.923% => ATE = 6.4%
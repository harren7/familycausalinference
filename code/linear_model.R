# load the required libraries
library(ggplot2)
library(boot)
library(pscl)

# load the csv data file
df <- read.csv("data/preprocessed/inference_base.csv")
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)
ftrs <- c("child_attend_kindergarten", "povstatus", "child_gender_w1", "parents_edustatus_w1",
           "hs_avg_score", "mother_fin_security_w2345", "father_in_jail_w1")

target <- c("college_grad_flag")
model_df <- df[,c(target, ftrs)]

model_df <- model_df[complete.cases(model_df),]
print(dim(model_df))

logistic_loss <- function(y, yhat){
  return(mean(-(y*log(yhat) + (1-y)*log(1-yhat))))
}

set.seed(2020)
glm_model <- glm(college_grad_flag ~ ., data = model_df, family = binomial)
null_model <- glm(college_grad_flag ~ 1, data = model_df, family = binomial)
print(summary(glm_model))

cv_err <- cv.glm(model_df, glm_model, logistic_loss, K=10)
gof_measures <- pR2(glm_model)
print(cv_err$delta[1])
print(gof_measures)
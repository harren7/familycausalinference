# load the required libraries
library(ggplot2)
library(boot)
library(pscl)
library(glmnet)
library(caTools)

# load the csv data file
df <- read.csv("data/preprocessed/inference_base.csv")
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)
cat_ftrs <- c("child_attend_kindergarten","povstatus", "child_gender_w1", 
              "parents_edustatus_w1","mother_fin_security_w2345", "father_in_jail_w1")
num_ftrs <- c("hs_avg_score")
ftrs <- c(cat_ftrs, num_ftrs)
target <- c("college_grad_flag")

model_df <- df[,c(target, ftrs)]
model_df <- model_df[complete.cases(model_df),]
data_index <- 1:nrow(model_df)
train_index <- sample(data_index, size = floor(0.8 * length(data_index)))  # 80% for training

train_df <- model_df[train_index,]
test_df <- model_df[-train_index,]

for(feature in cat_ftrs){
  train_df[[feature]] <- as.factor(train_df[[feature]])
  test_df[[feature]] <- as.factor(test_df[[feature]])
}

for(feature in num_ftrs){
  train_df[[feature]] <- scale(train_df[[feature]])
  test_df[[feature]] <- scale(test_df[[feature]])
}


X_train <- model.matrix(college_grad_flag ~ . + povstatus:parents_edustatus_w1 + mother_fin_security_w2345:father_in_jail_w1, 
                        data = train_df)
y_train <- train_df[,target]
X_test <- model.matrix(college_grad_flag ~ . + povstatus:parents_edustatus_w1 + mother_fin_security_w2345:father_in_jail_w1, 
                       data = test_df)
y_test <- test_df[,target]
print(dim(X_train))
print(dim(X_test))

set.seed(2020)
lambda_values <- 10^seq(5, -5, length = 100)
cvfit <- cv.glmnet(X_train, y_train, family = "binomial", lambda = lambda_values, alpha = 0)
plot(cvfit$glmnet.fit, "lambda", label = TRUE)
plot(cvfit)


calculate_f1 <- function(threshold, probs, actual) {
  predicted <- ifelse(probs > threshold, 1, 0)
  confusion_matrix <- table(Predicted = predicted, Actual = actual)
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}




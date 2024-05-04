# load the required libraries
library(ggplot2)
library(boot)
library(pscl)
library(glmnet)
library(caTools)
library(pROC)

# load the csv data file
df <- read.csv("data/preprocessed/inference_base.csv")
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)
cat_ftrs <- c("child_attend_kindergarten","povstatus", "child_gender_w1",
              "parents_edustatus_w1","mother_fin_security_w2345", "father_in_jail_w1")
num_ftrs <- c("hs_avg_score")
ftrs <- c(cat_ftrs, num_ftrs)
target <- c("college_grad_flag")

preprocess <- function(df, cat_ftrs, num_ftrs){
  for(feature in cat_ftrs){
    df[[feature]] <- as.factor(df[[feature]])
  }
  for(feature in num_ftrs){
    df[[feature]] <- scale(df[[feature]])
  }
  return(df)
}

model_df <- df[,c(target, ftrs)]
model_df <- model_df[complete.cases(model_df),]

data_index <- 1:nrow(model_df)
train_index <- sample(data_index, size = floor(0.8 * length(data_index)))
train_df <- preprocess(model_df[train_index,], cat_ftrs, num_ftrs)
test_df <- preprocess(model_df[-train_index,], cat_ftrs, num_ftrs)

X_train <- model.matrix(college_grad_flag ~. + povstatus:parents_edustatus_w1 +
                          mother_fin_security_w2345:father_in_jail_w1 + child_attend_kindergarten:hs_avg_score,
                        data = train_df)
y_train <- train_df[,target]
X_test <- model.matrix(college_grad_flag ~. + povstatus:parents_edustatus_w1 +
                         mother_fin_security_w2345:father_in_jail_w1 + child_attend_kindergarten:hs_avg_score,
                       data = test_df)
y_test <- test_df[,target]

set.seed(2020)
lambda_values <- 10^seq(2, -10, length = 100)
cvfit <- cv.glmnet(X_train, y_train, family = "binomial", lambda = lambda_values,
                  alpha = 1, intercept=TRUE)
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

probabilities <- predict(cvfit, newx = X_test, s = "lambda.min", type = "response")
roc_curve <- roc(y_test, probabilities[,1])

optimal_threshold <- coords(roc_curve, x = "best",  best.weights = c(1,0.25),
                            ret = c("threshold", "precision", "recall"))
print(optimal_threshold)

final_predictions <- ifelse(probabilities[,1] > optimal_threshold[1,1], 1, 0)

# Compute the confusion matrix
confusion_matrix <- table(Predicted = final_predictions, Actual = y_test)
print(confusion_matrix)

thresholds = c(optimal_threshold[1,1])
f1_score <- sapply(thresholds, function(t) calculate_f1(t, probabilities[,1], y_test))
print(f1_score)


refit_df <- preprocess(model_df, cat_ftrs, num_ftrs)
X <- model.matrix(college_grad_flag ~. + povstatus:parents_edustatus_w1 +
                    mother_fin_security_w2345:father_in_jail_w1 + child_attend_kindergarten:hs_avg_score, 
                  data = refit_df)
y <- model_df[,target]

final_model <- glmnet(X, y, family="binomial", lambda = cvfit$lambda.min, alpha=1, intercept = TRUE)
print(coef(final_model))




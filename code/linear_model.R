############################### PRE-DEFINED FUNCTIONS/SETTINGS ###############################
library(ggplot2)
library(boot)
library(pscl)
library(glmnet)
library(caTools)
library(pROC)
library(dplyr)

preprocess <- function(df, cat_ftrs, num_ftrs){
  for(feature in cat_ftrs){
    df[[feature]] <- as.factor(df[[feature]])
  }
  for(feature in num_ftrs){
    df[[feature]] <- scale(df[[feature]])
  }
  return(df)
}

calc_confidence <- function(mean, standard_dev, alpha=0.95, type=2){
  z_score = qnorm(1-(1-alpha)/type)
  return (c(mean-z_score*standard_dev, mean+z_score*standard_dev))
}

calculate_f1 <- function(threshold, probs, actual) {
  predicted <- ifelse(probs > threshold, 1, 0)
  confusion_matrix <- table(Predicted = predicted, Actual = actual)
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  f1 <- 2 * precision * recall / (precision + recall)
  return(f1)
}

par(mar = c(5, 4, 6.5, 2)) # setting plot margins to be a bit wider
set.seed(1)

############################### BUILD AND MEASURE GOF OF BASE MODEL ###############################


df <- read.csv("data/preprocessed/inference_base.csv")
df$college_grad_flag <- ifelse(df$child_edu_level == "Completed college/grad", 1, 0)

cat_ftrs <- c("child_attend_kindergarten","povstatus", "child_gender_w1",
              "parents_edustatus_w1","mother_fin_security_w2345", "father_in_jail_w1")
num_ftrs <- c("hs_avg_score")
ftrs <- c(cat_ftrs, num_ftrs)
target <- c("college_grad_flag")

model_df <- df[,c(target, ftrs)]
model_df <- model_df[complete.cases(model_df),]

base_model <- glm(college_grad_flag ~. + povstatus:parents_edustatus_w1 + 
                    mother_fin_security_w2345:father_in_jail_w1 + child_attend_kindergarten:hs_avg_score,
                  family = "binomial", data = preprocess(model_df, cat_ftrs, num_ftrs))
base_model_summary <- summary(base_model)
base_model_coeff <- base_model_summary$coefficients

gof_measures <- pR2(base_model)
print(base_model_summary)
print(gof_measures)

############################### BUILD AND MEASURE PREDICTIVE LLR ###############################


data_index <- 1:nrow(model_df)
train_index <- sample(data_index, size = floor(0.8 * length(data_index)))
train_df <- preprocess(model_df[train_index,], cat_ftrs, num_ftrs)
test_df <- preprocess(model_df[-train_index,], cat_ftrs, num_ftrs)

X_train <- model.matrix(college_grad_flag ~povstatus:parents_edustatus_w1 +
                          mother_fin_security_w2345:father_in_jail_w1 + child_attend_kindergarten:hs_avg_score + .,
                        data = train_df)
y_train <- train_df[,target]
X_test <- model.matrix(college_grad_flag ~. + povstatus:parents_edustatus_w1 +
                         mother_fin_security_w2345:father_in_jail_w1 + child_attend_kindergarten:hs_avg_score,
                       data = test_df)
y_test <- test_df[,target]

lambda_values <- 10^seq(2, -10, length = 100)
cvfit <- cv.glmnet(X_train, y_train, family = "binomial", lambda = lambda_values,
                  alpha = 1, intercept=TRUE)

plot(cvfit$glmnet.fit, "lambda", label = TRUE, 
     main = "Feature coefficients vs. regularization (lambda)")
plot(cvfit, main = "Binomial deviance vs. regularization (lambda)")


probabilities <- predict(cvfit, newx = X_test, s = "lambda.min", type = "response")
roc_curve <- roc(y_test, probabilities[,1])
plot(roc_curve, main = "ROC curve")

optimal_threshold <- coords(roc_curve, x = "best",  best.weights = c(1,0.25),
                            ret = c("threshold", "precision", "recall"))
print(optimal_threshold)

final_predictions <- ifelse(probabilities[,1] > optimal_threshold[1,1], 1, 0)

confusion_matrix <- table(Predicted = final_predictions, Actual = y_test)
print(confusion_matrix)

thresholds = c(optimal_threshold[1,1])
f1_score <- sapply(thresholds, function(t) calculate_f1(t, probabilities[,1], y_test))
print(f1_score)

############################### CALIBRATION PLOT ###############################


calib_data = data.frame(prob = probabilities[,1], 
                        bin = cut(probabilities[,1], 
                                  breaks = 10),
                        class = y_test)

calib_grouped_data = summarise(group_by(calib_data, bin), 
                               observed = sum(class)/n(), expected = sum(prob)/n(), 
                               se = sqrt(observed*(1-observed)/n()))

calib_plot = ggplot(calib_grouped_data, aes(expected, observed)) + 
  geom_point() +
  geom_errorbar(aes(ymax = observed+1.96*se, ymin = observed-1.96*se)) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", color = "red") +
  xlab("Expected proportion") + ylab("Observed proportion") +
  scale_x_continuous(minor_breaks = seq(0, 0.8, 0.025), breaks = seq(0, 0.8, 0.1)) + 
  ggtitle("Calibration plot") + 
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8, face = "bold"), 
        axis.text.x = element_text(size=8, face = "bold"),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(0.92, 0.12), 
        legend.text = element_text(size=9, hjust=0.5, face = "bold"), 
        legend.title = element_text(size=10,face = "bold"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.just = "right",
        strip.text.x = element_text(size = 9, face = "bold"),
        title = element_text(size=11))

ggsave("data/plots/llr-calib-plot.png", 
       dpi=300, width=2000, height=1200, units="px", bg="transparent")
print(calib_plot)


############################### BASE MODEL FEATURE COEFFICIENTS ###############################

sig_ftrs  = c()
point_est = c()
ci_lower = c()
ci_upper = c()

for (i in 1:length(ftrs)+1){
  if (base_model_coeff[i,"Pr(>|z|)"]>0.05){
    next
  }
  conf = calc_confidence(base_model_coeff[i, "Estimate"], base_model_coeff[i, "Std. Error"])
  cat("The confidence interval of the feature ", i, " is: [", round(conf[1], 3), ", ", round(conf[2], 3), "]\n", sep='')
  point_est = c(point_est, base_model_coeff[i, "Estimate"])
  ci_lower  = c(ci_lower, conf[1])
  ci_upper = c(ci_upper, conf[2])
  sig_ftrs = c(sig_ftrs, ftrs[i-1])
}

coeff_plot = ggplot() +
  geom_point(aes(x = sig_ftrs, y = point_est)) +
  geom_errorbar(aes(x = sig_ftrs, ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  xlab("Variables") + ylab("Estimate") + 
  labs(title = "Feature coefficients of logistic regression model")+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8, face = "bold"), 
        axis.text.x = element_text(angle = 30, vjust = 0.5, size=8, face = "bold"),
        axis.title.y = element_text(size=10, face="bold"),
        axis.title.x = element_text(size=10, face="bold"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = c(0.92, 0.12), 
        legend.text = element_text(size=9, hjust=0.5, face = "bold"), 
        legend.title = element_text(size=10,face = "bold"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.just = "right",
        strip.text.x = element_text(size = 9, face = "bold"),
        title = element_text(size=11))
ggsave("data/plots/basemodel-coeff-plot.png", 
       dpi=300, width=2000, height=1200, units="px", bg="transparent")

print(coeff_plot)



library(ggplot2)
library(dplyr)
library(tidyr)
library(treemap)


# target <- "hs_avg_score"

df <- read.csv("data/preprocessed/inference_base.csv")

linear_model <- lm(hs_avg_score ~ LEAD + AGE + SEX, data = base_df)
print(summary(linear_model))

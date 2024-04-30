# load the required libraries
library(ggplot2)

# load the csv data file
df <- read.csv("data/preprocessed/inference_base.csv")

# get low-income families
df_low_inc <- df[df$povstatus == "low-income",]

# boxplot of hs_avg_score by parents_edustatus_w1 (Good Plot!!)
ggplot(df, aes(x=povstatus, y=hs_avg_score, fill=parents_edustatus_w1)) +
  geom_boxplot() +
  ggtitle("Boxplot of hs_avg_score by parents_edustatus_w1") +
  xlab("parents_edustatus_w1") +
  ylab("hs_avg_score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# boxplot of high school gpa by child_edu_level
ggplot(df_low_inc, aes(x=child_edu_level, y=hs_avg_score)) +
  geom_boxplot() +
  ggtitle("Boxplot of hs_avg_score by child_edu_level") +
  xlab("child_edu_level") +
  ylab("hs_avg_score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

library(haven)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

# read in the full dataset 
df <- read.csv("../data/preprocessed/inference_base.csv")

# dimensions of df 
cat("Dimensions of dataframe:", dim(df))

# chart 1 - TARGET distribution -------
ggplot(df, aes(x = hs_avg_score)) +
  geom_histogram(binwidth = 0.25, fill = "skyblue", color = "black") +
  ggtitle("Histogram of High School GPA") +
  xlab("GPA Score") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
  
# chart 2 - Box plot of GPA by Child Edu Status and Kindergarten Attendance -----

# Define the desired order of categories for child_edu_level
desired_order <- c("Completed college","Attended some college", "Completed high school", 
                   "Did not complete high school")

# Convert child_edu_level into a factor variable with the desired order
df$child_edu_level <- factor(df$child_edu_level, levels = desired_order)

# box plot 
ggplot(subset(df, complete.cases(child_edu_level, child_attend_kindergarten)), aes(x=child_edu_level, y = hs_avg_score, fill=child_attend_kindergarten)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Kindergarten Attendance Impact on High School GPA", y = "GPA Score",fill="Kindergarten Attended", x='Child Educational Attainment')+
  scale_fill_manual(values = c("yes" = "skyblue", "no" = "salmon"))+
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# chart 3 - Child education outcomes by parent eduction 

# Summarize data for Child Edu barplot 
summary <- df[!is.na(df$child_edu_level),] %>% group_by(povstatus, parents_edustatus_w1, child_edu_level) %>%
  summarise(count = n(), .groups = 'drop') 
summary$child_edu_num <- factor(summary$child_edu_level, 
                                levels=c("Completed college","Attended some college",
                                         "Completed high school", NA))

summary <- summary %>% arrange(povstatus, parents_edustatus_w1, child_edu_num) %>% 
  group_by(povstatus, parents_edustatus_w1) %>%
  mutate(total = sum(count), percent = (count/total),
         cum_count = cumsum(count), 
         cum_percent = cum_count / total) %>% ungroup()

# bar plot for Child Edu Level by income and parent education 
ggplot(data = summary[!is.na(summary$child_edu_num), ], 
                   aes(fill = parents_edustatus_w1, y = cum_percent, x = child_edu_num)) +
  geom_bar(position = "dodge", stat = "identity",  colour = "black") + facet_wrap(~povstatus) +
  scale_fill_manual(values = c("lightblue","darkorange")) + 
  scale_color_manual(values = c('steelblue4','sienna4')) + coord_flip() +
  theme(axis.text.y = element_text(angle = 45, vjust = 1, hjust=1)) + 
  xlab('Child Education') + ylab('Percentage') +
  ggtitle('Variation in Education Outcomes by Parents\' Education')+
  labs(fill='Parents\'s Education Status')+
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
#, "Did not complete high school"

# chart 4 - High School GPA by Family Income 
ggplot(df[!is.na(df$povstatus), ], aes(x=povstatus, y=hs_avg_score, fill=parents_edustatus_w1)) +
  geom_boxplot() +
  ggtitle("Child High School GPA by Parents\' Education") +
  xlab("Family Income") +
  ylab("GPA Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# distributino of gpa and edu level 
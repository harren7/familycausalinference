library(haven)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(corrplot)
library(scales)

# read in the full dataset 
df <- read.csv("data/preprocessed/inference_base.csv")
custom_format <- function(x) {
  ifelse(x == floor(x), as.integer(x), format(x, nsmall = 1))
}

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
desired_order <- c("Completed college/grad","Some college, tech", "Completed high school", 
                   "Less than high school")

# Convert child_edu_level into a factor variable with the desired order
df$child_edu_level <- factor(df$child_edu_level, levels = desired_order)

# box plot 
ggplot(subset(df, complete.cases(child_edu_level, child_attend_kindergarten)), 
       aes(x=child_edu_level, y = hs_avg_score, fill=child_attend_kindergarten)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Kindergarten Attendance Impact on High School GPA", y = "GPA Score",
       fill="Kindergarten Attended", x='Child Educational Attainment')+
  scale_fill_manual(values = c("yes" = "skyblue", "no" = "salmon"))+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8), 
        axis.text.x = element_text(size=8),
        plot.title = element_text(hjust = 0.5),
        legend.position = c(0.92, 0.12), legend.text = element_text(size=9, hjust=0.5), 
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.just = "right",
        strip.text.x = element_text(size = 9),
        title = element_text(size=11)) + 
  labs(fill = "Kindergarten") + 
  scale_x_discrete(labels = wrap_format(20), limits=rev)
ggsave("data/plots/ka-hsgpa.png", 
       dpi=300, width=2000, height=1200, units="px", bg="transparent")


# chart 3 - Child education outcomes by parent eduction 

# Summarize data for Child Edu barplot 
summary <- df[!is.na(df$child_edu_level),] %>% group_by(povstatus, parents_edustatus_w1, child_edu_level) %>%
  summarise(count = n(), .groups = 'drop') 
summary$child_edu_num <- factor(summary$child_edu_level, 
                                levels=c("Completed college/grad","Some college, tech",
                                         "Completed high school", NA))

summary <- summary %>% arrange(povstatus, parents_edustatus_w1, child_edu_num) %>% 
  group_by(povstatus, parents_edustatus_w1) %>%
  mutate(total = sum(count), percent = (count/total),
         cum_count = cumsum(count), 
         cum_percent = cum_count / total) %>% ungroup()

# bar plot for Child Edu Level by income and parent education 

ggplot(data = summary[!is.na(summary$child_edu_num),], 
                   aes(fill = parents_edustatus_w1, y = cum_percent, x = child_edu_num)) +
  geom_bar(position = "dodge", stat = "identity",  colour = "black") + facet_wrap(~povstatus) +
  scale_fill_manual(values = c("lightblue","darkorange")) + 
  scale_color_manual(values = c('steelblue4','sienna4')) + coord_flip() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8), 
        axis.text.x = element_text(size=8),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom", legend.text = element_text(size=9), 
        strip.text.x = element_text(size = 9),
        title = element_text(size=11)) + 
  scale_x_discrete(labels = wrap_format(15)) +
  xlab('') + ylab('Percentage') +
  ggtitle('Variation in Education Outcomes by Parents\' Education')+
  labs(fill='') +
  scale_y_continuous(labels = custom_format)

ggsave("data/plots/education-outcome-by-parents-edu.png", 
       dpi=300, width=2000, height=1200, units="px", bg="transparent")

# chart 4 - High School GPA by Family Income 
ggplot(df[!is.na(df$povstatus), ], aes(x=povstatus, y=hs_avg_score, fill=parents_edustatus_w1)) +
  geom_boxplot() +
  ggtitle("Child High School GPA by Parents\' Education") +
  xlab("Family Income") +
  ylab("GPA Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


summary <- df[!is.na(df$child_edu_level),] %>% group_by(cm1ethrace_w1, child_edu_level) %>%
  summarise(count = n(), .groups = 'drop') 
summary$child_edu_num <- factor(summary$child_edu_level, 
                                levels=c("Completed college/grad","Some college, tech",
                                         "Completed high school", "Less than high school"))

summary <- summary %>% arrange(cm1ethrace_w1, child_edu_num) %>% 
  group_by(cm1ethrace_w1) %>%
  mutate(total = sum(count), percent = (count/total),
         cum_count = cumsum(count), 
         cum_percent = cum_count / total) %>% ungroup()
summary$Race <- summary$cm1ethrace_w1
summary$EduMetric <- ifelse(summary$child_edu_level == "Less than high school", 
                            summary$percent, summary$cum_percent)

ggplot(data = summary[(!is.na(summary$child_edu_num) & !is.na(summary$Race)),], 
       aes(fill = Race, y = EduMetric, x = child_edu_num, colour = Race)) +
  geom_bar(position = "dodge", stat = "identity", width=0.66) +
  scale_fill_manual(values = c('steelblue','sienna2', 'seagreen3')) +
  scale_color_manual(values = c('steelblue4','sienna4', 'seagreen4')) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=8), 
        axis.text.x = element_text(size=8),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=9), 
        strip.text.x = element_text(size = 9),
        title = element_text(size=11)) +
  scale_x_discrete(labels = wrap_format(15), limits=rev) +
  xlab('Education outcome') + ylab('Percentage') +
  ggtitle("Educational outcomes by race")

ggsave("data/plots/education-outcome-by-race.png", 
       dpi=300, width=2000, height=1200, units="px", bg="transparent")



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


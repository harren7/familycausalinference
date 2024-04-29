################################### SETUP OF GPA BASE FEATURES ###################################

gpa_base_df <- read_dta("data/raw/wave6/FF_wave6_2020v2.dta")
base_ftrs <- c("idnum","k6b20a","k6b20b","k6b20c","k6b20d")
# a. Grade in English or language arts
# b. Grade in Math
# c. Grade in History or social studies
# d. Grade in Science
gpa_base_df <- gpa_base_df[,base_ftrs]

convert_grades <- function(x) {
  factor(ifelse(x == 1, 'A',
                ifelse(x == 2, 'B',
                       ifelse(x == 3, 'C',
                              ifelse(x == 4, 'D or lower', NA)))), levels = c('A', 'B', 'C', 'D or lower'))
}

# Apply this function to each of the grading columns (excluding 'idnum')
grade_cols <- setdiff(names(gpa_base_df), "idnum")
gpa_base_df[grade_cols] <- lapply(gpa_base_df[grade_cols], convert_grades)

# Create a new dataset that has a column that takes an average of the 4 subjects

average_gpa <- gpa_base_df

convert_num <- function(x) {
  ifelse(x == "A", 4,
         ifelse(x == "B", 3,
                ifelse(x == "C", 2,
                       ifelse(x == "D or lower", 1, NA))))
}

average_gpa[paste0(grade_cols, "_num")] <- lapply(average_gpa[grade_cols], convert_num)

average_gpa[paste0(grade_cols, "_num")] <- lapply(average_gpa[grade_cols], convert_num)

# Calculate the average of the numeric columns
average_gpa$hs_avg_score <- rowMeans(average_gpa[paste0(grade_cols, "_num")], na.rm = TRUE)
write.csv(average_gpa, "data/preprocessed/gpa_averaged.csv", row.names = FALSE)


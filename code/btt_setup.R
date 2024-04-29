library(haven)
library(tidyr)
library(dplyr)

################################### SETUP OF BASE FEATURES (Parent income and education) ###################################

base_df <- read_dta("data/raw/wave1/FF_wave1_2020v2.dta")
base_ftrs <- c("idnum","cm1povca","cf1povca")
base_df <- base_df[,base_ftrs]

def_poverty_status <- function(pinc){
  if (is.na(pinc) || pinc<0){
    return(NA)
  }
  else if (pinc<=3){
    return("low-income")
  }
  else{
    return("high-income")
  }
}

build_poverty_col <- function(df, minc, finc){
  parent_max <- apply(base_df[, c(minc, finc)], 1, max)
  status <- sapply(parent_max, def_poverty_status)
  return(status)
}

base_df["povstatus"] <- build_poverty_col(base_df, "cm1povca", "cf1povca")

################################### SETUP OF TREATMENT FEATURES ###################################

def_child_edu <- function(cedu){
  if (is.na(cedu) || cedu<0){
    return(NA)
  }
  else if (cedu==1){
    return("yes")
  }
  else{
    return("no")
  }
}

build_child_edu_col <- function(df, mcedu, fcedu){
  parent_max <- apply(df[, c(mcedu, fcedu)], 1, max)
  status <- sapply(parent_max, def_child_edu)
  return(status)
}

treatment_df <- read_dta("data/raw/wave4/FF_wave4_2020v2.dta")
treatment_ftrs <- c("idnum","f4b7a","m4b7a")
treatment_df <- treatment_df[,treatment_ftrs]
treatment_df["child_attend_kindergarten"] <- build_child_edu_col(treatment_df, "m4b7a", "f4b7a")

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

write.csv(gpa_base_df, "data/preprocessed/gpa_raw.csv", row.names=FALSE)

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

# Write the data frame with averages to CSV
write.csv(average_gpa, "data/preprocessed/gpa_averaged.csv", row.names = FALSE)


################################### CONSOLIDATING BASE, TREATMENT###################################

consolidated_df <- merge(base_df, treatment_df, by.x = "idnum", by.y = "idnum")
consolidated_df <- merge(consolidated_df, average_gpa, by.x = "idnum", by.y = "idnum")
consolidated_df <- consolidated_df[,c("idnum","child_attend_kindergarten","povstatus","hs_avg_score")]
write.csv(consolidated_df, "data/preprocessed/base_treatment_target.csv", row.names=FALSE)

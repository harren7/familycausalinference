library(haven)
library(tidyr)
library(dplyr)

# grab wave1 data 
base_df <- read_dta("../data/raw/wave1/FF_wave1_2020v2.dta")

# function to convert int to yes, no, or NA
basic_convert <- function(feat_val){
  if (feat_val < 0){
    return(NA)
  }
  else if (feat_val==2 || feat_val == 0){
    return("No")
  }
  else{
    return("Yes")
  }
}

# function to convert the race feature 
race_convert <- function(race_val){
  if (race_val == 1){
    return("white")
  }
  else if (race_val == 2){
    return("black")
  }
  else if (race_val == 3){
    return("hispanic")
  }
  else{
    return(NA)
  }
}

# function to remove missing values in age 
age_convert <- function(age_val){
  if (age_val < 0){
    return(NA)
  }
  else{
    return(age_val)
  }
}

# function to combine father and mother marriage response 
build_marry_col <- function(df, m_ans, f_ans) {
  marry_max <- pmax(df[[m_ans]], df[[f_ans]], na.rm = TRUE)
  status <- sapply(marry_max, basic_convert)
  return(status)
}

# add marry question 
base_df['marry_w1'] <- build_marry_col(base_df, "m1b9", "f1b9")

# grab all features 
final_df <- base_df[, c("marry_w1", "cm1ethrace", "cf1ethrace", "cf1span", "cm1span","cm1age","cf1age")]

# rename features 
names(final_df) <- c("marry_w1", "cm1ethrace_w1", "cf1ethrace_w1", "cf1span_w1", "cm1span_w1","cm1age_w1","cf1age_w1")

# convert numeric features to str
final_df['cm1span_w1'] <- sapply(final_df$cm1span_w1, basic_convert)
final_df['cf1span_w1'] <- sapply(final_df$cf1span_w1, basic_convert)
final_df['cm1ethrace_w1'] <- sapply(final_df$cm1ethrace_w1, race_convert)
final_df['cf1ethrace_w1'] <- sapply(final_df$cf1ethrace_w1, race_convert)
final_df['cm1age_w1'] <- sapply(final_df$cm1age_w1, age_convert)
final_df['cf1age_w1'] <- sapply(final_df$cf1age_w1, age_convert)

# show final df 
head(final_df)

dim(final_df)



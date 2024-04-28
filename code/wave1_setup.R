library(haven)
library(tidyr)
library(dplyr)

# grab wave1 data 
w1_df <- read_dta("data/raw/wave1/FF_wave1_2020v2.dta")
w1_ftrs <- c("idnum","f1k3","m1j3", "f1k5", "m1j4", "cm1bsex", 
             "cf1finjail", "cm1finjail", "cm1edu", "cf1edu", 
             "cm1ethrace", "cf1ethrace", "m1b9", "f1b9",
             "cf1span", "cm1span","cm1age","cf1age")

w1_df <- w1_df[,w1_ftrs]

def_pretax_inc <- function(ptinc){
  if (is.na(ptinc) || ptinc<0){
    return (NA)
  }
  else{
    return (ptinc)
  }
}

build_pretax_col <- function(df, ptinc){
  status <- apply(df[,c(ptinc)], 1, def_pretax_inc)
  return(status)
}

def_saved_inc <- function(saveinc){
  if (is.na(saveinc) || saveinc<0){
    return(NA)
  }
  else if(saveinc==1){
    return("Some")
  }
  else if(saveinc==2){
    return("Just enough")
  }
  else{
    return("Not enough")
  }
}

build_save_inc_col <- function(df, saveinc){
  status <- apply(df[,c(saveinc)], 1, def_saved_inc)
  return(status)
}

def_child_gender <- function(gender){
  if (is.na(gender) || gender<0){
    return(NA)
  }
  else if(gender==1){
    return("Male")
  }
  else{
    return("Female")
  }
}

build_child_gender_col <- function(df, gender){
  status <- apply(df[,c(gender)], 1, def_child_gender)
  return(status)
}

def_father_in_jail <- function(jail_state){
  if (is.na(jail_state) || jail_state<0){
    return (NA)
  }
  else if(jail_state==1){
    return("Yes")
  }
  else{
    return("No")
  }
}

build_father_jail_col <- function(df, mfinjail, ffinfail){
  jail_state <- apply(df[, c(mfinjail, ffinfail)], 1, max)
  status <- sapply(jail_state, def_father_in_jail)
  return(status)
}


def_edu_status <- function(pedu){
  if (is.na(pedu) || pedu<0){
    return(NA)
  }
  else if (pedu<=2){
    return("Neither parent attended college")
  }
  else{
    return("At least one parent attended college")
  }
}

build_edu_col <- function(df, medu, fedu){
  parent_max <- apply(df[, c(medu, fedu)], 1, max)
  status <- sapply(parent_max, def_edu_status)
  return(status)
}


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

w1_df["father_mothly_pretax_inc_w1"] <- build_pretax_col(w1_df, "f1k3")
w1_df["mother_mothly_pretax_inc_w1"] <- build_pretax_col(w1_df, "m1j3")
w1_df["father_mothly_saved_inc_w1"] <- build_save_inc_col(w1_df, "f1k5")
w1_df["mother_mothly_saved_inc_w1"] <- build_save_inc_col(w1_df, "m1j4")
w1_df["child_gender_w1"] <- build_child_gender_col(w1_df, "cm1bsex")
w1_df["father_in_jail_w1"] <- build_father_jail_col(w1_df, "cf1finjail", "cm1finjail")
w1_df["parents_edustatus_w1"] <- build_edu_col(w1_df, "cm1edu", "cf1edu")
w1_df['cm1ethrace_w1'] <- sapply(w1_df$cm1ethrace, race_convert)
w1_df['cf1ethrace_w1'] <- sapply(w1_df$cf1ethrace, race_convert)
w1_df['marry_w1'] <- build_marry_col(w1_df, "m1b9", "f1b9")
w1_df['cm1span_w1'] <- sapply(w1_df$cm1span, basic_convert)
w1_df['cf1span_w1'] <- sapply(w1_df$cf1span, basic_convert)
w1_df['cm1age_w1'] <- sapply(w1_df$cm1age, age_convert)
w1_df['cf1age_w1'] <- sapply(w1_df$cf1age, age_convert)

# grab all features 
final_df <- w1_df[, c("idnum","father_mothly_pretax_inc_w1", "mother_mothly_pretax_inc_w1", 
                      "father_mothly_saved_inc_w1", "mother_mothly_saved_inc_w1", 
                      "child_gender_w1","father_in_jail_w1","parents_edustatus_w1", 
                      "cm1ethrace_w1", "cf1ethrace_w1", "marry_w1", "cm1span_w1",
                      "cf1span_w1", "cm1age_w1", "cf1age_w1")]
write.csv(final_df, "data/preprocessed/wave1_ftrs.csv", row.names=FALSE)


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

def_child_edu_compare <- function(vec){
  mcedu <- vec[1]
  fcedu <- vec[2]
  
  if(mcedu<0 & fcedu<0){
    return(NA)
  }
  else if(mcedu<0 & fcedu>0){
    return(fcedu)
  }
  else if(mcedu>0 & fcedu<0){
    return(mcedu)
  }
  else{
    return(min(c(mcedu,fcedu)))
  }
}

def_child_edu <- function(cedu){
  if (is.na(cedu) | cedu<0){
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
  parent_max <- apply(df[, c(mcedu, fcedu)], 1, def_child_edu_compare)
  status <- sapply(parent_max, def_child_edu)
  return(status)
}

treatment_df <- read_dta("data/raw/wave4/FF_wave4_2020v2.dta")
treatment_ftrs <- c("idnum","f4b7a","m4b7a","f4b8","m4b8")
treatment_df <- treatment_df[,treatment_ftrs]
treatment_df["child_attend_kindergarten"] <- build_child_edu_col(treatment_df, "m4b8", "f4b8")



################################### CONSOLIDATING BASE, TREATMENT, TARGET###################################

consolidated_df <- merge(base_df, treatment_df, by.x = "idnum", by.y = "idnum")
consolidated_df <- consolidated_df[,c("idnum","child_attend_kindergarten","povstatus")]
write.csv(consolidated_df, "data/preprocessed/base_treatment_target.csv", row.names=FALSE)

################################### CONSOLIDATING BTT, GPA, EDU, WAVE[1,4]###################################

btt_df <- read.csv("data/preprocessed/base_treatment_target.csv")
w1_df <- read.csv("data/preprocessed/wave1_ftrs.csv")
w3_df <- read.csv("data/preprocessed/wave3_ftrs.csv")
w4_df <- read.csv("data/preprocessed/wave4_ftrs.csv")
fin_df <- read.csv("data/preprocessed/finhelp_ftrs.csv")

edustate_df <- read.csv("data/preprocessed/childedu_target.csv")
gpa_df <- read.csv("data/preprocessed/gpa_averaged.csv")
gpa_df <- gpa_df[,c("idnum","hs_avg_score")]


btt_ftrs_df <- merge(btt_df, w1_df, by.x = "idnum", by.y = "idnum")
btt_ftrs_df <- merge(btt_ftrs_df, w3_df, by.x = "idnum", by.y = "idnum")
btt_ftrs_df <- merge(btt_ftrs_df, w4_df, by.x = "idnum", by.y = "idnum")
btt_ftrs_df <- merge(btt_ftrs_df, fin_df, by.x = "idnum", by.y = "idnum")
btt_ftrs_df <- merge(btt_ftrs_df, gpa_df, by.x = "idnum", by.y = "idnum")
btt_ftrs_df <- merge(btt_ftrs_df, edustate_df, by.x = "idnum", by.y = "idnum")

write.csv(btt_ftrs_df, "data/preprocessed/inference_base.csv", row.names=FALSE)


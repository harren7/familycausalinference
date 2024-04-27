library(haven)
library(tidyr)
library(dplyr)

base_df <- read_dta("data/raw/wave1/FF_wave1_2020v2.dta")
base_ftrs <- c("idnum","cm1edu","cf1edu","cm1povca","cf1povca")
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

base_df["povstatus"] <- build_poverty_col(base_df, "cm1povca", "cf1povca")
base_df["edustatus"] <- build_edu_col(base_df, "cm1edu", "cf1edu")

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

w1_df <- read_dta("data/raw/wave1/FF_wave1_2020v2.dta")

w1_ftrs <- c("idnum","m1b9","f1b9","cm1ethrace","cf1ethrace","cf1span",
             "cm1span","f1k3","f1k5","m1j3","m1j4","cm1bsex","cf1finjail",
             "cm1finjail","cm1age","cf1age","cf1hhinc","cm1hhinc")

w1_df <- w1_df[,w1_ftrs]

consolidated_df <- merge(base_df, treatment_df, by.x = "idnum", by.y = "idnum")

write.csv(consolidated_df, "data/preprocessed/pov_edu_base.csv", row.names=FALSE)

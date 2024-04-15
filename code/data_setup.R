library(haven)
library(tidyr)
library(dplyr)

w1df <- read_dta("data/raw/wave1/FF_wave1_2020v2.dta")
basedf <- w1df[,c("idnum","cm1edu", "cf1edu","cm1povca", "cf1povca")]

def_poverty_status <- function(pinc){
  if (is.na(pinc) || pinc<0){
    return("missing")
  }
  else if (pinc<=3){
    return("low-income")
  }
  else{
    return("high-income")
  }
}

build_poverty_col <- function(df, minc, finc){
  parent_max <- apply(basedf[, c(minc, finc)], 1, max)
  status <- sapply(parent_max, def_poverty_status)
  return(status)
}

def_edu_status <- function(pedu){
  if (is.na(pedu) || pedu<0){
    return("missing")
  }
  else if (pedu<=2){
    return("Neither parent attended college")
  }
  else{
    return("At least one parent attended college")
  }
}

build_edu_col <- function(df, medu, fedu){
  parent_max <- apply(basedf[, c(medu, fedu)], 1, max)
  status <- sapply(parent_max, def_edu_status)
  return(status)
}

basedf["povstatus"] <- build_poverty_col(basedf, "cm1povca", "cf1povca")
basedf["edustatus"] <- build_edu_col(basedf, "cm1edu", "cf1edu")
write.csv(basedf, "data/preprocessed/pov_edu_base.csv", row.names=FALSE)

library(haven)
library(tidyr)
library(dplyr)


w2_df <- read_dta("data/raw/wave2/FF_wave2_2020v2.dta")
w2_df <- w2_df[,c("idnum","f2g2","f2g3","m2g2","m2g3")]

w3_df <- read_dta("data/raw/wave3/FF_wave3_2020v2.dta")
w3_df <- w3_df[,c("idnum","f3h1e","f3h1f","m3h1e","m3h1f")]

w4_df <- read_dta("data/raw/wave4/FF_wave4_2020v2.dta")
w4_df <- w4_df[,c("idnum","f4h1e","f4h1f","m4h1e","m4h1f")]

w5_df <- read_dta("data/raw/wave5/FF_wave5_2020v2.dta")
w5_df <- w5_df[,c("idnum","f5e1e","f5e1f","m5e1e","m5e1f")]

fm_ftrs <- c("f2g2","f3h1e","f4h1e","f5e1e")
ff_ftrs <- c("f2g3","f3h1f","f4h1f","f5e1f")
mm_ftrs <- c("m2g2","m3h1e","m4h1e","m5e1e")
mf_ftrs <- c("m2g3","m3h1f","m4h1f","m5e1f")

base_df <- merge(w2_df, w3_df, by.x = "idnum", by.y = "idnum")
base_df <- merge(base_df, w4_df, by.x = "idnum", by.y = "idnum")
base_df <- merge(base_df, w5_df, by.x = "idnum", by.y = "idnum")

def_edu_status <- function(edu){
  if(is.na(edu) | edu<0){
    return(NA)
  }
  else if(edu<=3){
    return("Neither parent attended college")
  }
  else{
    return("At least one parent attended college")
  }
}

build_edu_col <- function(df, vec){
  max_resp <- apply(df[, vec], 1, max)
  status <- sapply(max_resp, def_edu_status)
  return(status)
}

base_df["fparents_edustatus_w2345"] <- build_edu_col(base_df, c(fm_ftrs, ff_ftrs))
base_df["mparents_edustatus_w2345"] <- build_edu_col(base_df, c(mm_ftrs, mf_ftrs))
base_df <- base_df[,c("idnum","fparents_edustatus_w2345","mparents_edustatus_w2345")]
write.csv(base_df, "data/preprocessed/gen0edu_ftrs.csv", row.names = FALSE)


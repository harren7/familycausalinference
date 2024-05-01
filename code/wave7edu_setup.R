library(haven)
library(tidyr)
library(dplyr)

df <- read_dta("data/raw/wave7/FF_wave7_2024v1.dta")
df <- df[,c("idnum","ck7edu")]
df <- df[order(df$idnum),]

def_edu_level <- function(edustate){
  if (is.na(edustate) | edustate<0){
    return (NA)
  }
  else if (edustate == 1){
    return ("Less than high school")
  }
  else if (edustate == 2){
    return ("Completed high school")
  }
  else if (edustate == 3){
    return ("Some college, tech")
  }
  else {
    return ("Completed college/grad")
  }
}

build_edu_col <- function(df, edustate){
  status <- apply(df[,c(edustate)], 1, def_edu_level)
  return(status)
}

df["child_edu_level"] <- build_edu_col(df, "ck7edu")
df <- df[,c("idnum","child_edu_level")]
write.csv(df, "data/preprocessed/childedu_target.csv", row.names=FALSE)

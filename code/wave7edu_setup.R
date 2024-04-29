library(haven)
library(tidyr)
library(dplyr)

df <- read_dta("data/raw/wave7/FF_wave7_2024v1.dta")
df <- df[,c("idnum","k7b1")]
df <- df[order(df$idnum),]

def_edu_level <- function(edustate){
  if (is.na(edustate) || edustate<0 || edustate==103){
    return (NA)
  }
  else if (edustate <= 12){
    return ("Did not complete high school")
  }
  else if (edustate == 13){
    return ("Completed high school")
  }
  else if (edustate <= 15 || edustate <= 102){
    return ("Attended some college, certification program, or trade school")
  }
  else {
    return ("Completed college")
  }
}

build_edu_col <- function(df, edustate){
  status <- apply(df[,c(edustate)], 1, def_edu_level)
  return(status)
}

df["child_edu_level"] <- build_edu_col(df, "k7b1")
df <- df[,c("idnum","child_edu_level")]
write.csv(df, "data/preprocessed/childedu_target.csv", row.names=FALSE)

w3_df <- read_dta("data/raw/wave3/FF_wave3_2020v2.dta")
w3_ftrs <- c("idnum","f3i30","m3i30")
w3_df <- w3_df[,w3_ftrs]

def_youth_cor <- function(youth_cor){
  if (is.na(youth_cor) || youth_cor < 0){
    return(NA)
  }
  else if (youth_cor == 1){
    return("yes")
  }
  else{
    return("no")
  }
}

w3_df["f_youth_cor_w3"] <- w3_df$f3i30 %>% sapply(def_youth_cor)
w3_df["m_youth_cor_w3"] <- w3_df$m3i30 %>% sapply(def_youth_cor)
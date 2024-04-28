w3_df <- read_dta("data/raw/wave3/FF_wave3_2020v2.dta")
w3_ftrs <- c("idnum","f3i30","m3i30", "p3m12_x", "p3m15_x", "p3m36")
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

def_child_beh <- function(x) {
  factor(ifelse(x == 0, 'Not true',
                ifelse(x == 1, 'Somewhat or sometimes true',
                       ifelse(x == 2, "Very true or often true", NA))), levels = c('Not true', 'Somewhat or sometimes true', 
                                                                                   "Very true or often true"))
}

cols <- c("p3m12_x", "p3m15_x", "p3m36")
w3_df[cols] <- lapply(w3_df[cols], def_child_beh)

colnames(w3_df)[colnames(w3_df) == 'p3m12_x'] <- 'c_neat_work_w3'
colnames(w3_df)[colnames(w3_df) == 'p3m15_x'] <- 'c_curious_w3'
colnames(w3_df)[colnames(w3_df) == 'p3m36'] <- 'c_interest_w3'

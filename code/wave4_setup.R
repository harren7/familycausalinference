w4_df <- read_dta("data/raw/wave4/FF_wave4_2020v2.dta")
w4_ftrs <- c("idnum","f4b19","m4b19")
w4_df <- w4_df[,w4_ftrs]

def_quit <- function(quit){
  if (is.na(quit) || quit < 0){
    return(NA)
  }
  else if (quit == 1){
    return("yes")
  }
  else{
    return("no")
  }
}

w4_df["f_quit_w4"] <- w4_df$f4b19 %>% sapply(def_quit)
w4_df["m_quit_w4"] <- w4_df$m4b19 %>% sapply(def_quit)
library(haven)

wave1 <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave1/FF_wave1_2020v2.dta")
wave2 <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave2/FF_wave2_2020v2.dta")
wave3 <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave3/FF_wave3_2020v2.dta")
wave4 <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave4/FF_wave4_2020v2.dta")
wave5 <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave5/FF_wave5_2020v2.dta")
wave6 <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave6/FF_wave6_2020v2.dta")

dim_list <- list(dim(wave1), dim(wave2), dim(wave3), dim(wave4), dim(wave5), dim(wave6))
dim_list
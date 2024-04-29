library(haven)
library(tidyr)
library(dplyr)

### MOTHER'S FINANCIAL SECURITY 

# Data from Wave 2,3,4,5
w2_df <- read_dta("../data/raw/wave2/FF_wave2_2020v2.dta")
w3_df <- read_dta("../data/raw/wave3/FF_wave3_2020v2.dta")
w4_df <- read_dta("../data/raw/wave4/FF_wave4_2020v2.dta")
w5_df <- read_dta("../data/raw/wave5/FF_wave5_2020v2.dta")

# Financial features from wave 2,3,4,5
fin_ftrs_w2 <- c("idnum","m2b34","m2b34a","m2g5","m2g5a1","m2g5a2",
                 "m2g5a3","m2g5a4","m2g5a5","m2g5a6","m2g5a7",
                 "m2g5a8","m2g5b","m2g5c")
fin_ftrs_w3 <- c("idnum","m3h2","m3h2b","m3h2c")
fin_ftrs_w4 <- c("idnum", "m4h2","m4h2b","m4h2c")
fin_ftrs_w5 <- c("idnum","m5e2","m5e2b","m5e2c")

# filter data on needed columns 
w2_df <- w2_df[,fin_ftrs_w2]
w3_df <- w3_df[,fin_ftrs_w3]
w4_df <- w4_df[,fin_ftrs_w4]
w5_df <- w5_df[, fin_ftrs_w5]

# Check 1 - are all these features of the same datatype?
print("Datatypes in wave 2")
print(sapply(w2_df, typeof))

print("Datatypes in wave 3")
print(sapply(w3_df, typeof))

print("Datatypes in wave 4")
print(sapply(w4_df, typeof))

print("Datatypes in wave 5")
print(sapply(w5_df, typeof))

# Merge the financial features in one dataframe
fin_df <- merge(w2_df, w3_df, by.x = "idnum", by.y = "idnum")
fin_df <- merge(fin_df, w4_df, by.x = "idnum", by.y = "idnum")
fin_df <- merge(fin_df, w5_df, by.x = "idnum", by.y = "idnum")

# Check 2 - Change numerical values to strings, but do the same numbers correspond to the same labels? 
for (f in fin_ftrs_w2) {
  unique_values <- unique(w2_df[[f]])
  # Check if -9 is in the unique values
  if (-9 %in% unique_values) {
    # Extract label for -9 if it exists
    label_for_minus_9 <- unique_values[match(-9, unique_values)]
    print(label_for_minus_9)
    # Check if the label for -9 is "Not in wave"
    if (grepl("-9 Not in wave", label_for_minus_9)) {
      cat("For feature:", f, "-9 corresponds to 'Not in wave'\n")
    } else {
      cat("For feature:", f, "-9 does not correspond to 'Not in wave'\n")
    }
  } else {
    cat("For feature:", f, "-9 not found in unique values\n")
  }
}

convert_fin <- function(x) {
  factor(ifelse(x == 1, 'Yes',
                ifelse(x == 2, 'No',
                       ifelse(x == -2, "Don't know", NA))), levels = c('Yes', 'No', "Don't know"))
}

# Apply this function to each of the financial features
fincols <- setdiff(names(fin_df), "idnum")
fin_df[fincols] <- lapply(fin_df[fincols], convert_fin)

# check if dataframe looks right 
head(fin_df)
print(dim(fin_df))

# If any aid has ever been received, we will consider it a Yes
# No's are ranked higher than Don't know or NA
determine_financial_security <- function(row) {
  if ("Yes" %in% row) {
    return("Yes")
  } else if ("No" %in% row) {
    return("No")
  } else if ("Don't know" %in% row) {
    return("Don't know")
  } else {
    return(NA)
  } 
}

fin_df$mother_fin_security_w2345 <- apply(fin_df[, -1], 1, function(row) determine_financial_security(as.list(row)))
final_fin_df <- fin_df[, c("idnum", "mother_fin_security_w2345")]

# check dim of final df 
print(dim(final_fin_df))

# save to csv 
write.csv(final_fin_df, "../data/preprocessed/finhelp_ftrs_mother.csv", row.names=FALSE)


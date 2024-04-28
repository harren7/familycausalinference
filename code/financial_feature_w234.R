library(haven)
library(tidyr)
library(dplyr)

### FATHER'S FINANCIAL SECURITY 

# Data from Wave 2,3,4
w2_df <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave2/FF_wave2_2020v2.dta")
w3_df <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave3/FF_wave3_2020v2.dta")
w4_df <- read_dta("/Users/sarah_prakriti_peters/Documents/DATA2020/FFdata/wave4/FF_wave4_2020v2.dta")

# Financial features from wave 2,3,4
fin_ftrs_w2 <- c("idnum","f2h11", "f2h9", "f2h7b","f2h7a","f2h8a3","f2h8a1","f2h8a2","f2h10","f2h9c",
             "f2h6","f2g7a1","f2g7a2","f2g7a3","f2g7a4","f2g7a5","f2g7a6","f2g7a7","f2g7a8",
             "f2g7b","f2h7c","f2h8b2","f2h8c2","f2g7c","f2h9d2","f2h8b3","f2h8c3","f2c19","f2h9d1",
             "f2h12","f2h8b1","f2h10a","f2h8c1","f2h10b","f2b29b","f2h9a","f2h10c","f2h9b2","f2h9b1")
fin_ftrs_w3 <- c("idnum","f3h2")
fin_ftrs_w4 <- c("idnum", "f4h2")

w2_df <- w2_df[,fin_ftrs_w2]
w3_df <- w3_df[,fin_ftrs_w3]
w4_df <- w4_df[,fin_ftrs_w4]

# Check 1 - are all these features of the same datatype?
print("Datatypes in wave 2")
print(sapply(w2_df, typeof))

print("Datatypes in wave 3")
print(sapply(w3_df, typeof))

print("Datatypes in wave 4")
print(sapply(w4_df, typeof))

# Merge the financial features in one dataframe
fin_df <- merge(w2_df, w3_df, by.x = "idnum", by.y = "idnum")
fin_df <- merge(fin_df, w4_df, by.x = "idnum", by.y = "idnum")

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

fin_df$father_fin_security_w234 <- apply(fin_df[, -1], 1, function(row) determine_financial_security(as.list(row)))
final_fin_df <- fin_df[, c("idnum", "father_fin_security_w234")]

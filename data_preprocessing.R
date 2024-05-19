# 0. Packages -------------------------------------------------------------
rm(list = ls())
library(dplyr)
library(ggplot2)
library(car)

# 1. Importing data -------------------------------------------------------

setwd("/Users/dmitryvagin/Documents/Education/Data Science and Society/4-thesis/R")
tran_lvl <- read.csv("dcpc_2022_tranlevel_public_xls.csv", sep = ",", header = TRUE)
ind_lvl <- read.csv("dcpc_2022_indlevel_public_xls.csv", sep = ",", header = TRUE)
day_lvl <- read.csv("dcpc_2022_daylevel_public_xls.csv", sep = ",", header = TRUE)

# 2. Selecting variables --------------------------------------------------

tran_lvl_short <- tran_lvl[, c("id", "diary_day", "pi", "device", "merch", "bill", "in_person", 
                               "payment", "amnt")]
ind_lvl_short <- ind_lvl[, c("id", "age", "highest_education", "work_occupation", 
                             "gender", "hhincome", 
                             "race", "marital_status", "statereside", "hh_size", 
                             "paypref_b1", "paypref_web", "paypref_inperson")]
day_lvl_short <- day_lvl[, c("id", "diary_day", "carry_csh", "carry_chk", "carry_cc",
                             "carry_dc", "carry_prepaid", "carry_banp", "carry_obbp", 
                             "carry_monord", "carry_paypal", "carry_acnt2acnt", "carry_oth", 
                             "carry_none", "carry_coins")]

# 3. Merging data ---------------------------------------------------------

dt <- merge(tran_lvl_short, day_lvl_short, by = c("id", "diary_day"), all.x = TRUE)
dt <- merge(dt, ind_lvl_short, by = "id", all.x = TRUE)

# 4. Variables transformation ---------------------------------------------

# 4.1. "pi" ---------------------------------------------------------------

class(dt$pi)
table(dt$pi, useNA = "always") # 2826 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$pi), ]
#
pi <- as.data.frame(table(tran_lvl_short$pi))
pi$Var1 <- factor(pi$Var1)
# bar chart of "pi"
ggplot(data = pi, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of payment instrument use")
# 0 - Multiple payment methods
# 1 - Cash
# 2 - Check
# 3 - Credit card
# 4 - Debit card
# 5 - Prepaid/gift/EBT card
# 6 - Bank account number payment
# 7 - Online banking bill payment
# 8 - Money order
# 10 - PayPal
# 11 - Account-to-account transfer
# 13 - Other payment method
# 14 - Deduction from income
#
# combine 13,11,10,0,14,8 into one group 22
dt$pi <- car::recode(dt$pi, "0=22; 1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=22; 10=22; 11=22; 13=22; 14=22")
dt$pi <- factor(dt$pi, labels = c("cash", "check", "ccard", "dcard", "prepaid",
                                  "banp", "obbp", "other"))
table(dt$pi, useNA = "always")

# 4.2. "device" -----------------------------------------------------------

class(dt$device)
table(dt$device, useNA = "always") # 92 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$device),]
#
device <- as.data.frame(table(dt$device))
device$Var1 <- factor(device$Var1)
# bar chart of "device"
ggplot(data = device, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of device use")
# 1 - Computer
# 2 - Tablet
# 3 - Mobile phone
# 4 - Landline phone
# 5 - Mail or delivery service
# 6 - Some other device not listed
# 7 - No device
# 8 - E-Zpass or other electronic toll device
#
dt$device <- factor(dt$device, labels = c("device_1", "device_2", "device_3", "device_4",
                                          "device_5", "device_6", "device_7", "device_8"))
table(dt$device, useNA = "always")

# 4.3. "merch" ------------------------------------------------------------

class(dt$merch)
table(dt$merch, useNA = "always") # 3 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$merch),]
#
merch <- as.data.frame(table(dt$merch))
merch$Var1 <- factor(merch$Var1)
# bar chart of "merch"
ggplot(data = merch, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of merch payments")
# 1 - Grocery stores, convenience stores without gas stations, pharmacies
# 2 - Gas stations
# 3 - Sit-down restaurants and bars
# 4 - Fast food restaurants, coffee shops, cafeterias, food trucks
# 5 - General merchandise stores, department stores, other stores, online shopping
# 6 - General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.
# 7 - Arts, entertainment, recreation
# 8 - Utilities not paid to the government: electricity, natural gas, water, sewer, trash, heating oil
# 9 - Taxis, airplanes, delivery
# 10 - Telephone, internet, cable or satellite tv, video or music streaming services, movie theaters
# 11 - Building contractors, plumbers, electricians, HVAC, etc.
# 12 - Professional services: legal, accounting, architectural services; veterinarians; photographers or photo processers
# 13 - Hotels, motels, RV parks, campsites
# 14 - Rent for apartments, homes, or other buildings, real estate companies, property managers, etc.
# 15 - Mortgage companies, credit card companies, banks, insurance companies, stock brokers, IRA funds, mutual funds, credit unions, sending remittances
# 16 - Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.
# 17 - Charitable or religious donations
# 18 - Hospital, doctor, dentist, nursing homes, etc.
# 19 - Government taxes or fees
# 20 - Schools, colleges, childcare centers
# 21 - Public transportation and tolls
dt$merch <- factor(dt$merch, labels = c("merch_1", "merch_2", "merch_3", "merch_4",
                                        "merch_5", "merch_6", "merch_7", "merch_8",
                                        "merch_9", "merch_10", "merch_11", "merch_12",
                                        "merch_13", "merch_14", "merch_15", "merch_16",
                                        "merch_17", "merch_18", "merch_19", "merch_20",
                                        "merch_21"))
table(dt$merch, useNA = "always")

# 4.4. "bill" -------------------------------------------------------------

class(dt$bill)
table(dt$bill, useNA = "always") # 2 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$bill),]
dt$bill <- factor(dt$bill, labels = c("bill_no", "bill_yes"))
table(dt$bill, useNA = "always")

# 4.5. "in_person" --------------------------------------------------------

class(dt$in_person)
table(dt$in_person, useNA = "always") # 7 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$in_person),]
dt$in_person <- factor(dt$in_person, labels = c("in_person_no", "in_person_yes"))
table(dt$in_person, useNA = "always")

# 4.6. "payment" ----------------------------------------------------------

class(dt$payment)
table(dt$payment, useNA = "always") # only 1 category, 0 missing values
# delete this variable from the dataset
dt$payment <- NULL

# 4.7. "amnt" -------------------------------------------------------------

class(dt$amnt)
sum(is.na(dt$amnt)) # 0 missing values
summary(dt$amnt)
# take natural logarithm of "amnt"
dt$amnt_log <- log(dt$amnt)
summary(dt$amnt_log)
# treating outliers of "amnt_log"
boxplot(dt$amnt_log)
lower_q_amnt <- boxplot.stats(dt$amnt_log)$stats[1]
upper_q_amnt <- boxplot.stats(dt$amnt_log)$stats[5]
nrow(dt[dt$amnt_log < lower_q_amnt, ]) # 61 "low" outliers
nrow(dt[dt$amnt_log > upper_q_amnt, ]) # 348 "high" outliers
dt <- subset(dt, (dt$amnt_log >= lower_q_amnt) & (dt$amnt_log <= upper_q_amnt))
# normalise "amnt_log" from 0 to 1
dt$amnt_log <- (dt$amnt_log - min(dt$amnt_log))/(max(dt$amnt_log) - min(dt$amnt_log))
summary(dt$amnt_log)
hist(dt$amnt_log)
# delete amnt
dt$amnt <- NULL

# 4.8. "carry_csh" --------------------------------------------------------

class(dt$carry_csh)
table(dt$carry_csh, useNA = "always") # 22 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$carry_csh),]
dt$carry_csh <- factor(dt$carry_csh, labels = c("carry_cash_no", "carry_cash_yes"))
table(dt$carry_csh, useNA = "always")

# 4.9. "carry_chk" --------------------------------------------------------

class(dt$carry_chk)
table(dt$carry_chk, useNA = "always") # 0 missing values
dt$carry_chk <- factor(dt$carry_chk, labels = c("carry_check_no", "carry_check_yes"))
table(dt$carry_chk, useNA = "always")

# 4.10. "carry_cc" --------------------------------------------------------

class(dt$carry_cc)
table(dt$carry_cc, useNA = "always") # 0 missing values
dt$carry_cc <- factor(dt$carry_cc, labels = c("carry_credit_card_no", "carry_credit_card_yes"))
table(dt$carry_cc, useNA = "always")

# 4.11. "carry_dc" --------------------------------------------------------

class(dt$carry_dc)
table(dt$carry_dc, useNA = "always") # 0 missing values
dt$carry_dc <- factor(dt$carry_dc, labels = c("carry_debit_card_no", "carry_debit_card_yes"))
table(dt$carry_dc, useNA = "always")

# 4.12. "carry_prepaid" ---------------------------------------------------

class(dt$carry_prepaid)
table(dt$carry_prepaid, useNA = "always") # 0 missing values
dt$carry_prepaid <- factor(dt$carry_prepaid, labels = c("carry_prepaid_no", "carry_prepaid_yes"))
table(dt$carry_prepaid, useNA = "always")

# 4.13. "carry_banp" ------------------------------------------------------

class(dt$carry_banp)
table(dt$carry_banp, useNA = "always") # 0 missing values
dt$carry_banp <- factor(dt$carry_banp, labels = c("carry_banp_no", "carry_banp_yes"))
table(dt$carry_banp, useNA = "always")

# 4.14. "carry_obbp" ------------------------------------------------------

class(dt$carry_obbp)
table(dt$carry_obbp, useNA = "always") # 0 missing values
dt$carry_obbp <- factor(dt$carry_obbp, labels = c("carry_obbp_no", "carry_obbp_yes"))
table(dt$carry_obbp, useNA = "always")

# 4.15. "carry_monord" ----------------------------------------------------

class(dt$carry_monord)
table(dt$carry_monord, useNA = "always") # 0 missing values
dt$carry_monord <- factor(dt$carry_monord, labels = c("carry_money_order_no", "carry_money_order_yes"))
table(dt$carry_monord, useNA = "always")

# 4.16. "carry_paypal" ----------------------------------------------------

class(dt$carry_paypal)
table(dt$carry_paypal, useNA = "always") # 0 missing values
dt$carry_paypal <- factor(dt$carry_paypal, labels = c("carry_paypal_no", "carry_paypal_yes"))
table(dt$carry_paypal, useNA = "always")

# 4.17. "carry_acnt2acnt" -------------------------------------------------

class(dt$carry_acnt2acnt)
table(dt$carry_acnt2acnt, useNA = "always") # 0 missing values
dt$carry_acnt2acnt <- factor(dt$carry_acnt2acnt, labels = c("carry_acnt2acnt_no", "carry_acnt2acnt_yes"))
table(dt$carry_acnt2acnt, useNA = "always")

# 4.18. "carry_oth" -------------------------------------------------------

class(dt$carry_oth)
table(dt$carry_oth, useNA = "always") # 0 missing values
dt$carry_oth <- factor(dt$carry_oth, labels = c("carry_other_no", "carry_other_yes"))
table(dt$carry_oth, useNA = "always")

# 4.19. "carry_none" ------------------------------------------------------

class(dt$carry_none)
table(dt$carry_none, useNA = "always") # 0 missing values
dt$carry_none <- factor(dt$carry_none, labels = c("carry_none_no", "carry_none_yes"))
table(dt$carry_none, useNA = "always")

# 4.20. "carry_coins" -----------------------------------------------------

class(dt$carry_coins)
table(dt$carry_coins, useNA = "always") # 1 missing value
# delete missing data using listwise method
dt <- dt[!is.na(dt$carry_coins),]
dt$carry_coins <- factor(dt$carry_coins, labels = c("carry_coins_no", "carry_coins_yes"))
table(dt$carry_coins, useNA = "always")

# 4.21. "age" --------------------------------------------------------------

class(dt$age)
sum(is.na(dt$age)) # 105 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$age),]
summary(dt$age)
# treating outliers of "age"
boxplot(dt$age)
lower_q_age <- boxplot.stats(dt$age)$stats[1]
upper_q_age <- boxplot.stats(dt$age)$stats[5]
nrow(dt[dt$age < lower_q_age, ]) # 0 "low" outliers
nrow(dt[dt$age > upper_q_age, ]) # 5 "high" outliers
dt <- subset(dt, (dt$age >= lower_q_age) & (dt$age <= upper_q_age))
# normalise "age" from 0 to 1
dt$age <- (dt$age - min(dt$age))/(max(dt$age) - min(dt$age))
summary(dt$age)
hist(dt$age)

# 4.22. "highest_education" ------------------------------------------------

class(dt$highest_education)
table(dt$highest_education, useNA = "always") # 7 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$highest_education),]
#
educ <- as.data.frame(table(dt$highest_education))
educ$Var1 <- factor(educ$Var1)
# bar chart of "educ"
ggplot(data = educ, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of education levels")
# 1 - Less than 1st grade
# 2 - 1st, 2nd, 3rd, or 4th grade
# 3 - 5th or 6th grade
# 4 - 7th or 8th grade
# 5 - 9th grade
# 6 - 10th grade
# 7 - 11th grade
# 8 - 12 grade - no diploma
# 9 - High school graduate or GED
# 10 - Some college but no degree
# 11 - Associate degree in college - occupational or vocational program
# 12 - Associate degree in college - academic program
# 13 - Bachelors degree
# 14 - Masters degree
# 15 - Professional school degree
# 16 - Doctorate degree
#
# combine 1,2,3,4,5,6,7,8 into one group 22
dt$highest_education <- car::recode(dt$highest_education, 
                                    "1=22; 2=22; 3=22; 4=22; 5=22; 6=22; 7=22; 8=22; 9=9; 10=10; 11=11; 12=12; 13=13; 14=14; 15=15; 16=16")
dt$highest_education <- factor(dt$highest_education, 
                               labels = c("educ_9", "educ_10", "educ_11", "educ_12", 
                                          "educ_13", "educ_14", "educ_15", "educ_16",
                                          "educ_22"))
table(dt$highest_education, useNA = "always")

# 4.23. "work_occupation" -------------------------------------------------

class(dt$work_occupation)
table(dt$work_occupation, useNA = "always") # too many NAs
# delete this variable from the dataset
dt$work_occupation <- NULL

# 4.24. "gender" ----------------------------------------------------------

class(dt$gender)
table(dt$gender, useNA = "always") # no missing values
dt$gender <- factor(dt$gender, labels = c("female", "male"))

# 4.25. "hhincome" --------------------------------------------------------

class(dt$hhincome)
table(dt$hhincome, useNA = "always") # 12 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$hhincome),]
#
hhincome <- as.data.frame(table(dt$hhincome))
hhincome$Var1 <- factor(hhincome$Var1)
# bar chart of "hhincome"
ggplot(data = hhincome, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of household income levels")
# 1 - Less than 5,000
# 2 - 5,000 to 7,499
# 3 - 7,500 to 9,999
# 4 - 10,000 to 12,499
# 5 - 12,500 to 14,999
# 6 - 15,000 to 19,999
# 7 - 20,000 to 24,999
# 8 - 25,000 to 29,999
# 9 - 30,000 to 34,999
# 10 - 35,000 to 39,999
# 11 - 40,000 to 49,999
# 12 - 50,000 to 59,999
# 13 - 60,000 to 74,999
# 14 - 75,000 to 99,999
# 15 - 100,000 to 149,999
# 16 - 150,000 or more
#
# combine 1,2,3,4,5,6 into one group 22
dt$hhincome <- car::recode(dt$hhincome, 
                           "1=22; 2=22; 3=22; 4=22; 5=22; 6=22; 7=7; 8=8; 9=9; 10=10; 11=11; 12=12; 13=13; 14=14; 15=15; 16=16")
dt$hhincome <- factor(dt$hhincome, 
                      labels = c("hhincome_7", "hhincome_8", "hhincome_9",
                                 "hhincome_10", "hhincome_11", "hhincome_12",
                                 "hhincome_13", "hhincome_14", "hhincome_15",
                                 "hhincome_16", "hhincome_22"))
table(dt$hhincome, useNA = "always")

# 4.26. "race" ------------------------------------------------------------

class(dt$race)
table(dt$race, useNA = "always") # 85 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$race),]
#
race <- as.data.frame(table(dt$race))
race$Var1 <- factor(race$Var1)
# bar chart of "race"
ggplot(data = race, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of race")
# 1 - Selected WHITE only
# 2 - Selected BLACK or AFRICAN AMERICAN only
# 3 - Selected AMERICAN INDIAN OR ALASKA NATIVE only
# 4 - Selected ASIAN only
# 5 - Selected NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER only
# 6 - Selected some combination of the above
dt$race <- factor(dt$race, labels = c("race_1", "race_2", "race_3",
                                      "race_4", "race_5", "race_6"))
table(dt$race, useNA = "always")

# 4.27. "marital_status" --------------------------------------------------

class(dt$marital_status)
table(dt$marital_status, useNA = "always") # 0 missing values
#
marital_status <- as.data.frame(table(dt$marital_status))
marital_status$Var1 <- factor(marital_status$Var1)
# bar chart of "marital_status"
ggplot(data = marital_status, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of marital status")
# 1 - Married (spouse lives with me)
# 2 - Married (spouse lives elsewhere)
# 3 - Separated
# 4 - Divorced
# 5 - Widowed
# 6 - Never married
dt$marital_status <- factor(dt$marital_status, 
                            labels = c("marital_status_1", "marital_status_2", 
                                       "marital_status_3", "marital_status_4",
                                       "marital_status_5", "marital_status_6"))
table(dt$marital_status, useNA = "always")

# 4.28. "statereside" -----------------------------------------------------

class(dt$statereside)
table(dt$statereside, useNA = "always") # no missing values
#
statereside <- as.data.frame(table(dt$statereside))
statereside$Var1 <- factor(statereside$Var1)
# bar chart of "marital_status"
ggplot(data = statereside, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of residence state")
#
dt$statereside <- factor(dt$statereside,
                         labels = c("state_1", "state_2", "state_3", "state_4",
                                    "state_5", "state_6", "state_7", "state_8",
                                    "state_9", "state_10", "state_11", "state_12",
                                    "state_13", "state_14", "state_15", "state_16",
                                    "state_17", "state_18", "state_19", "state_20",
                                    "state_21", "state_22", "state_23", "state_24",
                                    "state_25", "state_26", "state_27", "state_28",
                                    "state_29", "state_30", "state_31", "state_32",
                                    "state_33", "state_34", "state_35", "state_36",
                                    "state_37", "state_38", "state_39", "state_40",
                                    "state_41", "state_42", "state_43", "state_44",
                                    "state_45", "state_46", "state_47", "state_48",
                                    "state_49", "state_50", "state_51"))
table(dt$statereside, useNA = "always")

# 4.29. "hh_size" ---------------------------------------------------------

class(dt$hh_size)
table(dt$hh_size, useNA = "always") # too many NAs
# delete this variable from the dataset
dt$hh_size <- NULL

# 4.30. "paypref_b1" ------------------------------------------------------

class(dt$paypref_b1)
table(dt$paypref_b1, useNA = "always") # 4 missing values
# delete missing data using listwise method
dt <- dt[!is.na(dt$paypref_b1),]
#
paypref_b1 <- as.data.frame(table(dt$paypref_b1))
paypref_b1$Var1 <- factor(paypref_b1$Var1)
# bar chart of "paypref_b1"
ggplot(data = paypref_b1, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of payment preferences for bill payments")
# 1 - Cash
# 2 - Check
# 3 - Credit card
# 4 - Debit card
# 5 - Prepaid/gift/EBT card
# 6 - Bank account number payment
# 7 - Online banking bill payment
# 8 - Money order
# 10 - PayPal
# 11 - Account-to-account transfer
# 13 - Other payment method
dt$paypref_b1 <- factor(dt$paypref_b1, labels = c("pref_bill_1", "pref_bill_2",
                                                  "pref_bill_3", "pref_bill_4",
                                                  "pref_bill_5", "pref_bill_6",
                                                  "pref_bill_7", "pref_bill_8",
                                                  "pref_bill_10", "pref_bill_11",
                                                  "pref_bill_13"))
table(dt$paypref_b1, useNA = "always")

# 4.31. "paypref_web" -----------------------------------------------------

class(dt$paypref_web)
table(dt$paypref_web, useNA = "always") # 1382 missing values
# replace NAs with 28 (not answered)
dt$paypref_web <- replace(dt$paypref_web, is.na(dt$paypref_web), 28)
#
paypref_web <- as.data.frame(table(dt$paypref_web))
paypref_web$Var1 <- factor(paypref_web$Var1)
# bar chart of "paypref_web"
ggplot(data = paypref_web, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of payment preferences for web payments")
# 1 - Cash
# 2 - Check
# 3 - Credit card
# 4 - Debit card
# 5 - Prepaid/gift/EBT card
# 6 - Bank account number payment
# 7 - Online banking bill payment
# 10 - PayPal
# 11 - Account-to-account transfer
# 13 - Other payment method
# 28 - No answer given
dt$paypref_web <- factor(dt$paypref_web, labels = c("pref_web_1", "pref_web_2",
                                                  "pref_web_3", "pref_web_4",
                                                  "pref_web_5", "pref_web_6",
                                                  "pref_web_7", "pref_web_10", 
                                                  "pref_web_11", "pref_web_13",
                                                  "pref_web_28"))
table(dt$paypref_web, useNA = "always")

# 4.32. "paypref_inperson" ------------------------------------------------

class(dt$paypref_inperson)
table(dt$paypref_inperson, useNA = "always") # no missing values
#
paypref_inperson <- as.data.frame(table(dt$paypref_inperson))
paypref_inperson$Var1 <- factor(paypref_inperson$Var1)
# bar chart of "paypref_inperson"
ggplot(data = paypref_inperson, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.2) +
  ggtitle("Frequency of payment preferences for in-person payments")
# 1 - Cash
# 2 - Check
# 3 - Credit card
# 4 - Debit card
# 5 - Prepaid/gift/EBT card
# 6 - Bank account number payment
# 7 - Online banking bill payment
# 8 - Money order
# 10 - PayPal
# 11 - Account-to-account transfer
# 13 - Other payment method
#
dt$paypref_inperson <- factor(dt$paypref_inperson, 
                              labels = c("paypref_inperson_1", "paypref_inperson_2", 
                                         "paypref_inperson_3", "paypref_inperson_4", 
                                         "paypref_inperson_5", "paypref_inperson_6", 
                                         "paypref_inperson_7", "paypref_inperson_8", 
                                         "paypref_inperson_10", "paypref_inperson_11", 
                                         "paypref_inperson_13"))
table(dt$paypref_inperson, useNA = "always")

# 5. Exporting final dataset ----------------------------------------------

dt$diary_day <- NULL
write.csv(dt, "dt_final.csv", row.names = FALSE)

# 6. Creating training and test datasets ----------------------------------

rm(list = ls())
dt <- read.csv("dt_final.csv", sep = ",", header = TRUE)

id <- as.data.frame(table(dt$id)) # 4079 respondents
set.seed(22)
# Allocating 20% of respondents (not observations!) to hold-out
which_set <- sample(c(rep("train", 3263), rep("test", 816)))
id$which_set <- which_set
id$Freq <- NULL
colnames(id) <- c("id", "which_set")
#
dt <- merge(dt, id, by = "id", all.x = TRUE)
dt_test <- dt[dt$which_set == "test",] # test set
dt_train <- dt[dt$which_set == "train",] # training set
#
dt$which_set <- NULL
dt_train$which_set <- NULL
dt_test$which_set <- NULL
#
# training and test sets are similar in terms of "pi" distribution
round(prop.table(table(dt_train$pi))*100, 1)
round(prop.table(table(dt_test$pi))*100, 1) 

# 7. Creating cross-validation folds --------------------------------------

# 5 folds
id_train <- as.data.frame(table(dt_train$id)) # 3263 respondents
set.seed(22)
which_fold <- sample(c(rep("fold_1", 653), 
                              rep("fold_2", 653),
                              rep("fold_3", 653),
                              rep("fold_4", 652),
                              rep("fold_5", 652)))
id_train$which_fold <- which_fold
id_train$Freq <- NULL
colnames(id_train) <- c("id", "which_fold")
#
dt_train <- merge(dt_train, id_train, by = "id", all.x = TRUE)
dt_train_fold_1 <- dt_train[dt_train$which_fold == "fold_1",]
dt_train_fold_2 <- dt_train[dt_train$which_fold == "fold_2",]
dt_train_fold_3 <- dt_train[dt_train$which_fold == "fold_3",]
dt_train_fold_4 <- dt_train[dt_train$which_fold == "fold_4",]
dt_train_fold_5 <- dt_train[dt_train$which_fold == "fold_5",]
#
dt_train$which_fold <- NULL
dt_train_fold_1$which_fold <- NULL
dt_train_fold_2$which_fold <- NULL
dt_train_fold_3$which_fold <- NULL
dt_train_fold_4$which_fold <- NULL
dt_train_fold_5$which_fold <- NULL
#
# folds are similar in terms of "pi" distribution
round(prop.table(table(dt_train_fold_1$pi))*100, 1)
round(prop.table(table(dt_train_fold_2$pi))*100, 1)
round(prop.table(table(dt_train_fold_3$pi))*100, 1)
round(prop.table(table(dt_train_fold_4$pi))*100, 1)
round(prop.table(table(dt_train_fold_5$pi))*100, 1)

# 8. Exporting splitted datasets ------------------------------------------

write.csv(dt_train, "dt_train.csv", row.names = FALSE)
write.csv(dt_test, "dt_test.csv", row.names = FALSE)
write.csv(dt_train_fold_1, "dt_train_fold_1.csv", row.names = FALSE)
write.csv(dt_train_fold_2, "dt_train_fold_2.csv", row.names = FALSE)
write.csv(dt_train_fold_3, "dt_train_fold_3.csv", row.names = FALSE)
write.csv(dt_train_fold_4, "dt_train_fold_4.csv", row.names = FALSE)
write.csv(dt_train_fold_5, "dt_train_fold_5.csv", row.names = FALSE)

# 9. Other ----------------------------------------------------------------

round(prop.table(table(dt$pi))*100,3)
round(prop.table(table(dt_train$pi))*100,3)
round(prop.table(table(dt_test$pi))*100,3)
round(prop.table(table(dt_train_fold_1$pi))*100,3)
round(prop.table(table(dt_train_fold_2$pi))*100,3)
round(prop.table(table(dt_train_fold_3$pi))*100,3)
round(prop.table(table(dt_train_fold_4$pi))*100,3)
round(prop.table(table(dt_train_fold_5$pi))*100,3)

rm(list = ls())
dt_train <- read.csv("dt_train.csv", sep = ",", header = TRUE)
dt_train_amnt_up <- dt_train[dt_train$amnt_log >= 0.5,]
dt_train_amnt_down <- dt_train[dt_train$amnt_log < 0.5,]
round(prop.table(table(dt_train_amnt_up$pi))*100, 3)
round(prop.table(table(dt_train_amnt_down$pi))*100, 3)

dt_train_inpers_yes <- dt_train[dt_train$in_person == "in_person_yes",]
dt_train_inpers_no <- dt_train[dt_train$in_person == "in_person_no",]
round(prop.table(table(dt_train_inpers_yes$pi))*100, 3)
round(prop.table(table(dt_train_inpers_no$pi))*100, 3)

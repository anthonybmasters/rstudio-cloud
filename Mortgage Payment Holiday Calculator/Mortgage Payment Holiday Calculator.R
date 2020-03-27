## Install packages
library(tidyverse)

## Create functions
mortgage_payment <- function(loan_value, term_month, int_rate){
   df <- dplyr::tibble(.rows = term_month+1,
                      month = 0:term_month,
                      interest = 0,
                      repayment = 0,
                      debt = loan_value,
                      tot_int = 0)
  std_paym <- round(df$debt[1]*(int_rate/12)/(1-(1+int_rate/12)^(-term_month)),2)
  for (i in 2:nrow(df)){
   df$interest[i] = round((int_rate/12)*df$debt[i-1],2)
   df$repayment[i] = dplyr::case_when(
     df$debt[i-1] <= 0 ~ 0, #if the mortgage is redeemed, there is no payment
     df$debt[i-1] + df$interest[i] < std_paym ~ df$debt[i-1] + df$interest[i],
     i == nrow(df) ~ df$debt[i-1] + df$interest[i], #the final payment
     TRUE ~ std_paym
   )
   df$debt[i] = df$debt[i-1] + df$interest[i] - df$repayment[i]
   df$tot_int[i] = df$tot_int[i-1] + df$interest[i]
  }
  return(df)}

mortgage_overpayment <- function(loan_value, term_month, int_rate,
                                 overpay_value, overpay_start, overpay_period){
   df <- dplyr::tibble(x = 0:term_month,
                       interest = 0,
                       repayment = 0,
                       debt = loan_value,
                       tot_int = 0) %>%
      dplyr::rename(month = x)
   std_paym <- round(df$debt[1]*(int_rate/12)/(1-(1+int_rate/12)^(-term_month)),2)
   over_paym <- round(overpay_value/overpay_period, 2)
   over_start <- overpay_start + 1
   over_end <- overpay_start + overpay_period
   for (i in 2:nrow(df)){
      df$interest[i] = round((int_rate/12)*df$debt[i-1],2)
      df$repayment[i] = dplyr::case_when(
         df$debt[i-1] <= 0 ~ 0, #if the mortgage is redeemed, there is no payment
         df$debt[i-1] + df$interest[i] < df$repayment[i-1] ~ df$debt[i-1] + df$interest[i],
         i == nrow(df) ~ df$debt[i-1] + df$interest[i], #the final payment
         i >= over_start & i <= over_end ~ std_paym + over_paym, #overpayment
         TRUE ~ std_paym)
      df$debt[i] = df$debt[i-1] + df$interest[i] - df$repayment[i]
      df$tot_int[i] = df$tot_int[i-1] + df$interest[i]  
   }
   return(df)}

mortgage_holiday <- function(loan_value, term_month, int_rate,
                             holiday_start, holiday_period){
   df <- dplyr::tibble(month = 0:term_month,
                       interest_holiday = 0,
                       repayment_holiday = 0,
                       debt_holiday = loan_value,
                       tot_int_holiday = 0,
                       interest_current = 0,
                       repayment_current = 0,
                       debt_current = loan_value,
                       tot_int_current = 0)
   std_paym_A <- round(df$debt_current[1]*(int_rate/12)/(1-(1+int_rate/12)^(-term_month)),2)
   hol_start <- holiday_start + 1
   hol_end <- holiday_start + holiday_period
   for (i in 2:nrow(df)){
      # Calculating interest
      df$interest_holiday[i] = round((int_rate/12)*df$debt_holiday[i-1],2)
      df$interest_current[i] = round((int_rate/12)*df$debt_current[i-1],2)
      
      # Repayment step
      std_paym_B <- round(df$debt_holiday[hol_end]*(int_rate/12)/(1-(1+int_rate/12)^(-term_month+hol_end-1)),2)
      df$repayment_holiday[i] = dplyr::case_when(
         df$debt_holiday[i-1] <= 0 ~ 0, #if the mortgage is redeemed, there is no payment
         df$debt_holiday[i-1] + df$interest_holiday[i] < df$repayment_holiday[i-1] ~ df$debt_holiday[i-1] + df$interest_holiday[i],
         i == nrow(df) ~ df$debt_holiday[i-1] + df$interest_holiday[i], #the final payments
         i < hol_start ~ std_paym_A, #the first period
         i >= hol_start & i <= hol_end ~ 0, #payment holiday
         i > hol_end ~ std_paym_B, #after the holiday
         TRUE ~ std_paym_A)
      df$repayment_current[i] = dplyr::case_when(
         df$debt_current[i-1] <= 0 ~ 0, #if the mortgage is redeemed, there is no payment
         df$debt_current[i-1] + df$interest_current[i] < std_paym_A ~ df$debt_current[i-1] + df$interest_current[i],
         i == nrow(df) ~ df$debt_current[i-1] + df$interest_current[i], #the final payment
         TRUE ~ std_paym_A)
      
      # Debt step
      df$debt_holiday[i] = df$debt_holiday[i-1] + df$interest_holiday[i] - df$repayment_holiday[i]
      df$debt_current[i] = df$debt_current[i-1] + df$interest_current[i] - df$repayment_current[i]
      
      # Total interest step
      df$tot_int_holiday[i] = df$tot_int_holiday[i-1] + df$interest_holiday[i]
      df$tot_int_current[i] = df$tot_int_current[i-1] + df$interest_current[i]  
   }
   return(df)}
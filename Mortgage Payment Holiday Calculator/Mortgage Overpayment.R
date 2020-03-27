## Install packages
library(tidyverse)

## Set the theme
theme_clean <- theme_bw(base_family="Calibri") + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic", margin = margin(b=12)),
        plot.caption = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_clean)

## Create functions
mortgage_payment <- function(loan_value, term_month, int_rate){
  df <- dplyr::tibble(month = 0:term_month,
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
  df <- dplyr::tibble(month = 0:term_month,
                      interest = 0,
                      repayment = 0,
                      debt = loan_value,
                      tot_int = 0)
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

## Create the tables
mortgage_A_df <- mortgage_payment(250000, 240, 0.035) %>%
  rename_at(vars(-month), list(~str_replace(., "$", "_A")))
mortgage_B_df <- mortgage_overpayment(250000, 240, 0.035,
                                      10000, 12, 1) %>%
  rename_at(vars(-month), list(~str_replace(., "$", "_B")))
mortgage_C_df <- mortgage_overpayment(250000, 240, 0.035,
                                      10000, 12, 50) %>%
  rename_at(vars(-month), list(~str_replace(., "$", "_C")))
mortgage_D_df <- mortgage_overpayment(250000, 240, 0.035,
                                      10000, 60, 1) %>%
  rename_at(vars(-month), list(~str_replace(., "$", "_D")))
mortgage_comparison_df <- full_join(mortgage_A_df, mortgage_B_df,
                                    by = "month")
mortgage_comparison_df <- full_join(mortgage_comparison_df, mortgage_C_df,
                                    by = "month")
mortgage_comparison_df <- full_join(mortgage_comparison_df, mortgage_D_df,
                                    by = "month")
mortgage_saving_df <- mortgage_comparison_df %>%
  dplyr::mutate("Overpaying £10k in month 12" = tot_int_A - tot_int_B,
                "Overpaying £200 in each month 12-61" = tot_int_A - tot_int_C,
                "Overpaying £10k in month 60" = tot_int_A - tot_int_D) %>%
  dplyr::select("month", 18:20) %>%
  pivot_longer(cols = 2:4,
               names_to = "Scenario",
               values_to = "Value",
               values_drop_na = TRUE)

## Creating the graph
mortgage_saving_gg <- ggplot(data = mortgage_saving_df,
                             aes(x = month, y = Value,
                                 group = Scenario)) +
  geom_step(aes(color = Scenario),
            size = 1.2) +
  labs(title = "Paying off a mortgage bit by bit reduces total interest",
       subtitle = "Difference in total interest paid by month [£], versus making no overpayment.",
       caption = "Assumptions: Mortgage loan value of £250,000,for 240 months, with an annual 3.5% interest.",
       x = "Month",
       y = "")
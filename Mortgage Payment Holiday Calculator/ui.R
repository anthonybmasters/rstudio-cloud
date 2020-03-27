## ui.R for Mortgages Payment Holiday Calculator
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

dashboardPage(
  dashboardHeader(title = "Mortgages Payment Holiday Calculator"),
  dashboardSidebar(
    numericInput("loan_value", "Mortgage borrowed amount [£]:",
                 value = 250000, min = 0),
    numericInput("term_month", "Mortgage term in months:",
                 value = 240, min = 1, max = 360),
    numericInput("interest_rate_pc", "Annual interest rate [%]:",
                 value = 3.5, min = 0, max = 20),
    numericInput("holiday_start", "Month payment holiday starts:",
                 value = 13, min = 2),
    sliderInput("holiday_period", "Payment holiday in months:",
                value = 3, min = 1, max = 6),
    actionButton("calculate", "Calculate")
    ),

  dashboardBody(
              fluidRow(
                valueBoxOutput("monthly_payment_before", width = 3),
                valueBoxOutput("monthly_payment_after", width = 3),
                valueBoxOutput("monthly_payment_difference", width = 3),
                valueBoxOutput("total_interest_difference", width = 3)
              )
      )
)
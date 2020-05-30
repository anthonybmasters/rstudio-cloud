## ui.R for Mortgages Payment Holiday Calculator
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

## Set plotting theme
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

## Header, Sidebar, Body

dashboard_header <- dashboardHeader(title = "Payment Holiday")

dashboard_sidebar <- dashboardSidebar(
  numericInput("loan_value", "Mortgage borrowed amount [£]:",
               value = 250000, min = 0),
  numericInput("term_month", "Mortgage term in months:",
               value = 240, min = 1, max = 360),
  numericInput("interest_rate_pc", "Annual interest rate [%]:",
               value = 3.5, min = 0, max = 20),
  numericInput("holiday_start", "Month payment holiday starts:",
               value = 13, min = 2, max = 354),
  sliderInput("holiday_period", "Payment holiday in months:",
              value = 3, min = 1, max = 6),
  actionButton("calculate", "Calculate"),
  sidebarMenu(
    menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
    menuItem("Table", tabName = "table", icon = icon("table")),
    menuItem("Notes", tabName = "notes", icon = icon("clipboard-list"))
    )
)

dashboard_body <- dashboardBody(
  tabItems(
    tabItem("calculator",
            fluidRow(
              valueBoxOutput("monthly_payment_before", width = 3),
              valueBoxOutput("monthly_payment_after", width = 3),
              valueBoxOutput("monthly_payment_difference", width = 3),
              valueBoxOutput("total_interest_difference", width = 3)
            ),
            fluidRow(
              box(width = 12, status = "info", solidHeader = TRUE,
                  title = "Mortgage Payments by Month",
              plotOutput("monthly_repayment_graph"))
            )
    ),
    
    tabItem("table",
            h2("Mortgage Statistics by Month"),
            downloadButton("downloadCsv", "Download as CSV"),
            dataTableOutput("monthly_repayment_table")
            ),
    
    tabItem("notes",
            h1("Notes"),
            p("Anthony B. Masters built this mortgage payment holiday calculator. This is version 0.2.",
              br(),
              "This version added the static graph, table, and notes."),
            p("The inputs are:"),
              tags$ul(
                tags$li("Mortgage borrowed amount: The amount that you have borrowed for the mortgage loan."),
                tags$li("Mortgage term in months: The length of the loan in months. This value can be between 1 and 360 months."),
                tags$li("Annual interest rate: The interest rate on the mortgage. This value divided by 12 is the monthly interest."),
                tags$li("Holiday start: the month number that the payment holiday starts. This value can be between 2 and 353 months."),
                tags$li("Holiday period: how long in months that the payment holiday lasts for. This is between 1 and 6 months.")
              ),
            p("These values may be found on your mortgage statement and digital records."),
            p("A mortgage payment holiday is not an interest holiday. Taking a holiday means higher repayments afterwards. This calculator works out those values."),
            p("This calculator is free-to-use, and does not constitute financial advice."),
            p("The calculator assumes a constant annual interest rate until the end of the mortgage."),
            
            h2("Code"),
            p("The code is available on GitHub:",
              br(),
              a(href="https://github.com/anthonybmasters/rstudio-cloud/tree/master/Mortgage%20Payment%20Holiday%20Calculator",
                     "Mortgage Payment Holiday Calculator code"))
            )
    )
  )

## Make the Page
  
dashboardPage(header = dashboard_header,
              sidebar = dashboard_sidebar,
              body = dashboard_body)
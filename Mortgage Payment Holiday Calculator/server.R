## Server.R for Mortgages Payment Holiday Calculator
library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)
source('Mortgage Payment Holiday Calculator.R')

function(input, output, session){
  
  values <- reactiveValues(
    loan_value = 250000,
    term_month = 240,
    interest_rate = 3.5/100,
    holiday_start = 13,
    holiday_period = 3
  )
  
  observeEvent(input$calculate, {
    values$loan_value <- input$loan_value
    values$term_month <- input$term_month
    values$interest_rate <- input$interest_rate_pc/100
    values$holiday_start <- input$holiday_start
    values$holiday_period <- input$holiday_period
      })
  
  mortgage_table <- eventReactive(input$calculate, {
    mortgage_holiday(input$loan_value,
                     input$term_month,
                     input$interest_rate_pc/100,
                     input$holiday_start,
                     input$holiday_period)
                   }, ignoreNULL = FALSE)
  
  pound <- scales::dollar_format(prefix = "£", largest_with_cents = 1)
  
  paym_before <- reactive({mortgage_table()$repayment_holiday[2]})
  paym_after <- reactive({(mortgage_table()$repayment_holiday[values$holiday_start + values$holiday_period + 1])})
  paym_diff <- reactive({paym_after() - paym_before()})
  tot_int_diff <- reactive({mortgage_table()$tot_int_holiday[values$term_month+1] - mortgage_table()$tot_int_current[values$term_month+1]})
  
  #Our outputs are then shown
  
  output$monthly_payment_before <- renderValueBox({
    valueBox(
      value = paym_before() %>% pound(),
      subtitle = "Monthly payment before",
      color = "yellow")
  })
  
  output$monthly_payment_after <- renderValueBox({
    valueBox(
      value = paym_after() %>% pound(),
      subtitle = "Monthly payment after",
      color = "blue")
  })
  
  output$monthly_payment_difference <- renderValueBox({
    valueBox(
      value = paym_diff() %>% pound(),
      subtitle = "Difference in monthly payments",
      color = "green")
  })
  
  output$total_interest_difference <- renderValueBox({
    valueBox(
      value = tot_int_diff() %>% pound(),
      subtitle = "Difference in total interest paid",
      color = "purple")
  })
  
  output$monthly_repayment_graph <- renderPlot({
    mortgage_table() %>%
      plot_payment_holiday()
  })
  
  output$downloadCsv <- downloadHandler(
    filename = "mortgage_table.csv",
    content = function(file) {
      write.csv(mortgage_table(), file)
    },
    contentType = "text/csv"
  )
  
  output$monthly_repayment_table <- renderDataTable({
    mortgage_table()
  })
  }
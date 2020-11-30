## Purpose
This R code creates a graph of confirmed COVID-19 cases in Wiltshire, by specimen date.
A second file creates a heat-map of confirmed cases in England, per 100,000 estimated people in each age group.
A third file uses Public Health England's *ukcovid19* package in R.

## Data Sources
I use the confirmed cases by specimen date (pillars 1 and 2), from the Public Health England data dashboard:
- Public Health England: https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv

Since this is the latest file, I have also uploaded a copy of the file I used to make the graph.

For the second file, I used the week 29 edition of the PHE COVID-19 national reports:
- Public Health England: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/922239/Weekly_COVID19_report_data_w39_v2.xlsx

For the third file, I used the *ukcovid19* package in R:
- Public Health England: https://github.com/publichealthengland/coronavirus-dashboard-api-R-sdk

## R Markdown
There are R Markdown pages to view:
- Coronavirus cases in Wiltshire: https://rpubs.com/anthonybmasters/coronavirus-wiltshire
- PHE Case Rates by Age Group: https://rpubs.com/anthonybmasters/phe-case-rates-by-age-group
- PHE Coronavirus package *ukcovid19*: https://rpubs.com/anthonybmasters/phe-ukcovid19-package

## Purpose
The purpose of this R code is make animated graphs, showing revisions to the ONS modelled positivity and incidence rates in England.
This is the daily modelled estimates with 95% credible intervals from the University of Oxford.
The daily incidence model is distinct to the official published estimate of new COVID-19 infections each day.

## Data sources
The source for each series of daily modelled estimates is the Office for National Statistics.
There is an graph of estimated COVID-19 incidence in England in most weekly reports, shown by publication date.
- 30th October:	https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/30october2020
- 6th November: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/6november2020
- 13th November:	https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/13november2020
- 20th November:	https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/20november2020
- 4th December:	https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/4december2020

I also drew the model estimates for the positivity rate in England from tables 7a and 7b in the ONS daily file:
- Data file (11th December): https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata

## ONS Methods Note
There is a methods note for the ONS COVID-19 Infection Survey, which describes the University of Oxford modelling:
- ONS: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/methodologies/covid19infectionsurveypilotmethodsandfurtherinformation#modelling

## R Markdown
There is an R Markdown page to view:
- 1st version: https://rpubs.com/anthonybmasters/ons-covid-19-model-estimates
- 2nd version: https://rpubs.com/anthonybmasters/ons-model-estimates-covid19

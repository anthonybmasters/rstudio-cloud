## Install packages
library(tidyverse)
install.packages("waffle", repos = "https://cinc.rud.is")
library(hrbrthemes)
library(waffle)
packageVersion("waffle") #should be '1.0.1'

## Make the data frames
test_one_df <- tribble(~Result, ~Count,
                       "True Positive", 18,
                       "False Negative", 2,
                       "False Positive", 8,
                       "True Negative", 72)
test_two_df <- tribble(~Result, ~Count,
                       "True Positive", 14,
                       "False Negative", 6,
                       "False Positive", 1,
                       "True Negative", 79)
test_three_df <- tribble(~Result, ~Count,
                       "True Positive", 9,
                       "False Negative", 1,
                       "False Positive", 18,
                       "True Negative", 72)

## Make the plotting function
plot_pictogram <- function(test_df, plot_title, plot_subtitle){
  test_gg <- test_df %>% ggplot(aes(label = Result,
                                    values = Count)) +
    geom_pictogram(n_rows = 10, aes(colour = Result),
                   make_proportional = TRUE) +
    coord_equal() +
    theme_ipsum_rc(grid="") +
    scale_colour_manual(name = NULL,
                        values = c("#5BC0EB", "#E28413", "#9BC53D", "#C3423F")) +
    scale_label_pictogram(name = NULL,
                          values = c("asterisk", "user", "user", "asterisk")) +
    theme_enhance_waffle() +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         caption = "This is an illustrative example of 100 people. Actual numbers of false results will vary.")
  return(test_gg)
}

## Make graphs
test_one_gg <- plot_pictogram(test_one_df,
                              "In this test, 8 out of 26 positive test results are false",
                              "Assumptions: Viral prevalance is 20%. The false positive and false negatives rates are both 1 in 10.")
test_two_gg <- plot_pictogram(test_two_df,
                              "In this sample, 20 people have the virus, with only 15 positive results",
                              "Assumptions: Viral prevalence is 20%. The false positive rate is 1 in 80, but the false negative rate is 30%.")
test_three_gg <- plot_pictogram(test_three_df,
                                "If you have a positive test result, the probability of having the virus is 1 in 3",
                                "Assumptions: Viral prevalence is 10%. The false positive rate is 20%, and the false negative rate is 10%.")
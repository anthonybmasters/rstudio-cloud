## Packages and themes
library(tidyverse)
library(readxl)
library(mgcv)

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

## Draw and tidy data
uk_polling_df <- read_excel("House Effects/UK General Election Polling - 2020-08-30.xlsx")

uk_poll_tidy_df <- uk_polling_df %>%
  dplyr::select(1:12) %>%
  dplyr::rename(Conservatives = con,
         Labour = lab,
         "Liberal Democrats" = ldem,
         "Scottish National Party" = snp,
         "Green Party" = grn,
         Others = oth) %>%
  pivot_longer(cols = 7:12,
                      names_to = "party", values_to = "VI_share")

uk_poll_tidy_df$party = factor(uk_poll_tidy_df$party,
                               levels = c("Conservatives", "Labour", "Liberal Democrats",
                                          "Scottish National Party", "Green Party", "Others"))

uk_poll_avg_df <- uk_polling_df %>%
  group_by(company) %>%
  summarise(count = n(), across(6:11, mean))

uk_poll_exc_df <- uk_polling_df %>%
  mutate(days_since_ge = as.numeric(difftime(fw_end, "2019-12-12",
                                             units = "days")),
         con_exc = con - mean(uk_poll_avg_df$con),
         lab_exc = lab - mean(uk_poll_avg_df$lab),
         ldem_exc = ldem - mean(uk_poll_avg_df$ldem),
         snp_exc = snp - mean(uk_poll_avg_df$snp),
         grn_exc = grn - mean(uk_poll_avg_df$grn))

# This is a helper function for extracting confidence intervals and returning a tidy data frame
# Adapted from Sam Clifford: https://rdrr.io/github/samclifford/mgcv.helper/src/R/confint.gam.R
confint.gam <- function(object, parm = NULL, level = 0.95, ...) {
  obj.s <- mgcv::summary.gam(object)
  
  E <- obj.s$p.coeff %>%
    tibble::tibble(Estimate = .,
                   term=names(.)) %>%
    dplyr::select(., term, Estimate)
  
  SE <- obj.s$se %>%
    tibble::tibble(se = .,
                   term = names(.)) %>%
    dplyr::select(., term, se)
  if (is.null(parm)){
    parm <- E$term
  }
  nu <- obj.s$residual.df
  my.tbl <- dplyr::inner_join(E, SE, by = "term") %>%
    dplyr::filter(., term %in% parm) %>%
    dplyr::mutate(.,
                  Statistic = Estimate/se,
                  L = Estimate +
                    se * stats::qt(df = nu,
                                   p = (1 - level) / 2),
                  U = Estimate +
                    se * stats::qt(df = nu,
                                   p = 1 - (1 - level) / 2),
                  estimate = round(100*Estimate, digits = 1),
                  lower = round(100*L, digits = 1),
                  upper = round(100*U, digits = 1))
  names(my.tbl)[3] <- "Std_Error"
  names(my.tbl)[5] <- sprintf("%.1f%%",
                              100*(1-level)/2)
  names(my.tbl)[6] <- sprintf("%.1f%%",
                              100*(1-(1-level)/2))
  return(my.tbl)
  
}

## Creating the models
con_exc_model <- gam(formula = con_exc ~ 0 + s(days_since_ge) + company,
                     data = uk_poll_exc_df)
lab_exc_model <- gam(formula = lab_exc ~ 0 + s(days_since_ge) + company,
                     data = uk_poll_exc_df)
ldem_exc_model <- gam(formula = ldem_exc ~ 0 + s(days_since_ge) + company,
                     data = uk_poll_exc_df)
snp_exc_model <- gam(formula = snp_exc ~ 0 + s(days_since_ge) + company,
                      data = uk_poll_exc_df)
grn_exc_model <- gam(formula = grn_exc ~ 0 + s(days_since_ge) + company,
                     data = uk_poll_exc_df)

## Produce the tables
con_exc_table <- con_exc_model %>% confint.gam()
lab_exc_table <- lab_exc_model %>% confint.gam()
ldem_exc_table <- ldem_exc_model %>% confint.gam()
snp_exc_table <- snp_exc_model %>% confint.gam()
grn_exc_table <- grn_exc_model %>% confint.gam()

con_exc_table %>% dplyr::select(1, 7:9)
lab_exc_table %>% dplyr::select(1, 7:9)
ldem_exc_table %>% dplyr::select(1, 7:9)
snp_exc_table %>% dplyr::select(1, 7:9)
grn_exc_table %>% dplyr::select(1, 7:9)

## Making the graph
uk_poll_tidy_gg <- uk_poll_tidy_df %>%
  filter(party %in% c("Conservatives", "Labour")) %>%
  ggplot(aes(x = fw_end, y = 100*VI_share, color = company)) +
  geom_point(size = 3) +
  facet_wrap(~party) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  scale_color_manual(values = c("#E6007E", "#ff5733", "#374FA1", "#969696", "#00BCF2",
                                "#000000", "#F3ACB3", "#FECE4E", "#7C64C3")) +
  labs(title = "All companies with regular polls show similar trends for Conservative and Labour shares.",
       subtitle = "Headline vote intention estimates by British Polling Council members [%], by fieldwork end-date.",
       x = "Fieldwork end-date",
       y = "",
       caption = "Data: Wikipedia; British Polling Council member company archives [as at 1st September 2020].")
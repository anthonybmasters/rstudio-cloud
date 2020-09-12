## Packages and themes
library(tidyverse)
library(readxl)
library(mgcv)
library(gt)

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

company_colours <- c("BMG Research" = "#E6007E",
                     "Deltapoll" = "#ff5733",
                     "Ipsos MORI" = "#374FA1",
                     "Kantar Public" = "#969696",
                     "Opinium" = "#00BCF2",
                     "Redfield & Wilton Strategies" = "#000000",
                     "Savanta ComRes" = "#F3ACB3",
                     "Survation" = "#FECE4E",
                     "YouGov" = "#7C64C3")

## Draw and tidy data
uk_polling_df <- read_excel("House Effects/UK General Election Polling - 2020-09-14.xlsx")

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
  summarise(count = n(),
            across(6:12, mean))

uk_poll_count_df <- uk_poll_avg_df %>%
  dplyr::select("company", "count")

uk_poll_exc_df <- uk_polling_df %>%
  mutate(days_since_ge = as.numeric(difftime(fw_end, "2019-12-12",
                                             units = "days")),
         con_exc = con - mean(uk_poll_avg_df$con),
         lab_exc = lab - mean(uk_poll_avg_df$lab),
         ldem_exc = ldem - mean(uk_poll_avg_df$ldem),
         snp_exc = snp - mean(uk_poll_avg_df$snp),
         grn_exc = grn - mean(uk_poll_avg_df$grn),
         con_lead_excl = con_lead - mean(uk_poll_avg_df$con_lead))

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
                  est = round(100*Estimate, digits = 1),
                  low = round(100*L, digits = 1),
                  upp = round(100*U, digits = 1))
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
con_lead_exc_model <- gam(formula = con_lead_exc ~ 0 + s(days_since_ge) + company,
                          data = uk_poll_exc_df)

## Produce the table
con_exc_table <- con_exc_model %>% confint.gam() %>%
  dplyr::select(1, 7:9)
lab_exc_table <- lab_exc_model %>% confint.gam() %>%
  dplyr::select(1, 7:9)
ldem_exc_table <- ldem_exc_model %>% confint.gam() %>%
  dplyr::select(1, 7:9)
snp_exc_table <- snp_exc_model %>% confint.gam() %>%
  dplyr::select(1, 7:9)
grn_exc_table <- grn_exc_model %>% confint.gam() %>%
  dplyr::select(1, 7:9)
con_lead_exc_table <- con_lead_exc_model %>% confint.gam() %>%
  dplyr::select(1, 7:9)

house_effects_tbl <- con_exc_table %>%
  inner_join(lab_exc_table, by = "term",
             suffix = c("_con", "_lab")) %>%
  inner_join(ldem_exc_table, by = "term") %>%
  inner_join(snp_exc_table, by = "term",
             suffix = c("_ldem", "_snp")) %>%
  inner_join(grn_exc_table, by = "term") %>%
  inner_join(con_lead_exc_table, by = "term",
             suffix = c("_grn", "_lead")) %>%
  dplyr::mutate(term = str_remove(term, "company")) %>%
  dplyr::rename(company = term) %>%
  inner_join(uk_poll_count_df, by = "company")

house_effects_source_note <- paste0("Data: Wikipedia; British Polling Council member company archives [as at ",
                                    max(uk_poll_tidy_df$fw_end),
                                    "].")
house_effects_subtitle <- paste0("Figures are in percentage points relative to the industry average. Polling fieldwork between: ",
                                 min(uk_poll_tidy_df$fw_start),
                                 " and ",
                                 max(uk_poll_tidy_df$fw_end),
                                 ".")

house_effects_gt <- house_effects_tbl %>%
  gt() %>%
  cols_merge(columns = vars(est_con, low_con, upp_con),
             hide_columns = vars(low_con, upp_con),
             pattern = "<b>{1}</b><br>({2}, {3})") %>%
  cols_merge(columns = vars(est_lab, low_lab, upp_lab),
             hide_columns = vars(low_lab, upp_lab),
             pattern = "<b>{1}</b><br>({2}, {3})") %>%
  cols_merge(columns = vars(est_ldem, low_ldem, upp_ldem),
             hide_columns = vars(low_ldem, upp_ldem),
             pattern = "<b>{1}</b><br>({2}, {3})") %>%
  cols_merge(columns = vars(est_snp, low_snp, upp_snp),
             hide_columns = vars(low_snp, upp_snp),
             pattern = "<b>{1}</b><br>({2}, {3})") %>%
  cols_merge(columns = vars(est_grn, low_grn, upp_grn),
             hide_columns = vars(low_grn, upp_grn),
             pattern = "<b>{1}</b><br>({2}, {3})") %>%
  cols_merge(columns = vars(est_lead, low_lead, upp_lead),
             hide_columns = vars(low_lead, upp_lead),
             pattern = "<b>{1}</b><br>({2}, {3})") %>%
  cols_move_to_start(columns = vars(company, count)) %>%
  cols_label(company = "Polling Company",
             count = "Count",
             est_con = "CON",
             est_lab = "LAB",
             est_ldem = "LDEM",
             est_snp = "SNP",
             est_grn = "GRN",
             est_lead = "CON Lead") %>%
  tab_header(title = md("**Estimated House Effects in UK/GB Vote Intention Polls.**"),
             subtitle = house_effects_subtitle) %>%
  tab_source_note(source_note = house_effects_source_note) %>%
  opt_row_striping(row_striping = TRUE) %>%
  cols_align(align = "right",
             columns = vars(count))

## Making the graph
uk_poll_tidy_gg <- uk_poll_tidy_df %>%
  filter(party %in% c("Conservatives", "Labour")) %>%
  ggplot(aes(x = fw_end, y = 100*VI_share, color = company)) +
  geom_point(size = 3) +
  facet_wrap(~party) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  scale_color_manual(values = company_colours) +
  labs(title = "All companies with regular polls show similar trends for Conservative and Labour shares.",
       subtitle = "Headline vote intention estimates by British Polling Council members [%], by fieldwork end-date.",
       x = "Fieldwork end-date",
       y = "",
       caption = house_effects_source_note)
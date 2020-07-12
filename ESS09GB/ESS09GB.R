## Packages and Themes
library(haven)
library(sjlabelled)
library(janitor)
library(tidyverse)
library(survey)

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

## Import and Tidy Data
ESS09GB_sav <- read_sav("ESS09GB/ESS9GB.sav")

ESS09GB_sav_df <- ESS09GB_sav %>% 
  mutate_all(sjlabelled::as_label)  # replaces with informative labels

ess09gb_unweighted <- svydesign(data = ESS09GB_sav_df,
                                ids = ~1, weights = NULL)
ess09gb_poststrat <- svydesign(data = ESS09GB_sav_df,
                               ids = ~1, weights = ~pspwght)

svytable(formula = ~vteumbgb,
         design = ess09gb_unweighted, Ntotal = 100)

svytable(formula = ~vteumbgb,
         design = ess09gb_poststrat, Ntotal = 100)

ESS09GB_EUmem_df <- ESS09GB_sav_df %>%
  filter(cntry == "United Kingdom" &vteumbgb != "NA") %>%
  group_by(vteumbgb) %>%
  summarise(unweighted = n(),
            design = sum(dweight),
            post_strat = sum(pspwght)) %>%
  mutate(unweighted = 100*unweighted/sum(unweighted),
         design = 100*design/sum(design),
         post_strat = 100*post_strat/sum(post_strat))

ESS09GB_EUmem_tidy_df <- ESS09GB_EUmem_df %>%
  mutate(EUmembershipchoice =
           case_when(vteumbgb == "Remain a member of the European Union" ~ "Remain",
                     vteumbgb == "Leave the European Union" ~ "Leave",
                     TRUE ~ "Would Not Valid Vote")) %>%
  group_by(EUmembershipchoice) %>%
  summarise(Unweighted = sum(unweighted),
            Design = sum(design),
            "Post-Stratification" = sum(post_strat)) %>%
  pivot_longer(cols = 2:4,
               names_to = "Weights",
               values_to = "Share")

ESS09GB_EUmem_tidy_df$EUmembershipchoice <- factor(ESS09GB_EUmem_tidy_df$EUmembershipchoice,
                                                   levels = c("Leave", "Would Not Valid Vote", "Remain"))
ESS09GB_EUmem_tidy_df$Weights <- factor(ESS09GB_EUmem_tidy_df$Weights,
                                        levels = c("Unweighted", "Design", "Post-Stratification"))

## Create the graph
ESS09GB_EUmem_tidy_gg <- ESS09GB_EUmem_tidy_df %>%
  ggplot(aes(x = Weights, y = Share,
             fill = EUmembershipchoice)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = round(Share)),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(name = "EU Referendum vote intention", 
                    values = c("#016fa4", "#797b74", "#fdcd2c")) +
  labs(title = "Applying weights increased the Remain share central estimate in the UK's hypothetical EU membership question",
       subtitle = "Question: 'If there were to be a new referendum tomorrow, would you vote for the UK to remain a member of the European Union or leave the European Union?' [%]. Excludes Don't Knows.",
       caption = "Data: European Social Survey (NatCen). 2,204 UK adults (15+), interviewed face-to-face in 31st August 2018 to 22nd February 2019.",
       x = "Weights",
       y = "")
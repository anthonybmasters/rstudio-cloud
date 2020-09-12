## Packages and Themes
library(tidyverse)
library(tidybayes)

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

## Setting priors and inputting data
set.seed(1988) #this seed is arbitrary
xA <- 300; nA <- 1000; alphaA <- 1; betaA <- 1
xB <- 340; nB <- 1000; alphaB <- 1; betaB <- 1
number_sims <- 50000

alphaA_post <- alphaA + xA; betaA_post <- betaA + nA - xA
alphaB_post <- alphaB + xB; betaB_post <- betaB + nB - xB

pA <- rbeta(number_sims, alphaA_post, betaA_post)
pB <- rbeta(number_sims, alphaB_post, betaB_post)

pB_minus_pA <- pB - pA %>% as_tibble()

## Producing summaries
pB_minus_pA_ci <- pB_minus_pA %>%
  mean_qi(value*100)

pB_minus_pA_prob <- sum(pB_minus_pA > 0) / number_sims
pB_minus_pA_BF <- pB_minus_pA_prob / (1 - pB_minus_pA_prob)

muA_post <- alphaA_post / (alphaA_post + betaA_post)
muB_post <- alphaB_post / (alphaB_post + betaB_post)
diff_approx_var <- muA_post * (1 - muA_post) / (alphaA_post + betaA_post + 1) +
  muB_post * (1 - muB_post) / (alphaB_post + betaB_post + 1)

pB_minus_pA_approx_ci <- tibble(mean = muB_post - muA_post,
                                lower = muB_post - muA_post - qnorm(0.975)*sqrt(diff_approx_var),
                                upper = muB_post - muA_post + qnorm(0.975)*sqrt(diff_approx_var))

## Making the graph
pB_minus_pA_title <- paste0("The probability that B has higher conversion than A is ~",
                          round(pB_minus_pA_prob*100, digits = 0), "%")
pB_minus_pA_subtitle <- paste0("The posterior distribution of the difference between two proportions, based on ",
                          number_sims, " numerical simulations. The test sample has ",
                          nA + nB,
                          " total users, with uniform priors.")
pB_minus_pA_caption <- paste0("Both proportions have uniform priors, Beta(1, 1). Data: version A: ",
                              xA, " conversions, ", nA ," users; page B: ",
                              xB, " conversions, ", nB ," users.")

pB_minus_pA_gg <- pB_minus_pA %>%
  ggplot(aes(x = value*100,
             fill = stat(x > 0))) +
  stat_halfeye(point_interval = mean_qi) +
  labs(title = pB_minus_pA_title,
       subtitle = pB_minus_pA_subtitle,
       x = "Arithmetic difference between B and A [percentage points]",
       y = "",
       caption = pB_minus_pA_caption) + 
  scale_fill_discrete(name = "Difference (B minus A)",
                      labels = c("Less than 0", "More than 0"))
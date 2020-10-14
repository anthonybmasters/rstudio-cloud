## Packages and Themes
library(tidyverse)

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

## Set inputs and create functions
set.seed(2020)

number_sims <- 10000
population_size <- 1000
sample_size <- 100
population_mean <- 0
population_sd <- 1
k <- population_size / sample_size
fpc_star <- (population_size - sample_size) / population_size

# Based on Glen Meeden's R function:
# http://users.stat.umn.edu/~gmeeden/classes/5201/handouts/syssmpR1.html
varp_sys <- function(y, n, k)
  {N <- length(y)
  if(N != n*k) stop("The length(y) is not n times k.")
  dummy <- seq(1, (n-1)*k + 1, k)
  data_mean <- mean(y)
  ans <- 0
  for(i in 1:k){
    ans <- ans + (mean(y[dummy + (i-1)]) - data_mean)^2
    }
  ans_sys <- ans / k
  return(ans_sys)
}

# Functions use constants set in the inputs section
function_list <- function(x){c(mean = mean(x),
                               std_err_sys = sqrt(varp_sys(x, sample_size, k)),
                               std_err_srs = sqrt(fpc_star*var(x)/sample_size))}

## Create data
sims_list_df <- replicate(n = number_sims,
                          rnorm(n = population_size,
                                mean = population_mean,
                                sd = population_sd),
                          simplify = FALSE)

sims_summary_df <- sapply(sims_list_df, function_list) %>%
  t() %>%
  as_tibble() %>%
  mutate(ratio = std_err_sys / std_err_srs)

## Make graph
sims_eff_count <- sims_summary_df %>%
  filter(ratio < 1) %>%
  count() %>% as.numeric()

sims_title <- paste0("Systematic sampling was more precise than simple sampling in ",
                     round(100*sims_eff_count/number_sims),
                     "% of simulations.")

sims_subtitle <- paste0("Standard errors for simple random samples (without replacement) versus 1-in-",
                        k,
                        " systematic samples of the same size. Each dot is one of ",
                        number_sims,
                        " simulations.")

sims_caption <- paste0("Each simulation is: ",
                       population_size,
                       " observations of an N(",
                       population_mean,
                       ", ",
                       population_sd^2,
                       ") random variable. Standard errors in simple random sampling are more consistent: the axes have different limits.")

sims_summary_gg <- sims_summary_df %>%
  ggplot(mapping = aes(x = std_err_srs,
                       y = std_err_sys,
                       colour = stat(x > y))) +
  geom_point(alpha = 0.2) +
  scale_colour_discrete(name = "Relative efficiency",
                        labels = c("Simple random sampling has a lower standard error",
                                   "Systematic sampling has a lower standard error")) +
  labs(title = sims_title,
       subtitle = sims_subtitle,
       caption = sims_caption,
       x = "Standard error (simple random sampling)",
       y = "Standard error (systematic sampling)") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

jitter_title = paste0("In ",
                      round(100*sims_eff_count/number_sims),
                      "% of simulations, systematic sampling had a lower standard error than simple random sampling.")

jitter_subtitle = paste0("The ratio of standard errors: for 1-in-",
                         k,
                         " systematic sampling divided by simple random sampling. Each dot is one of ",
                         number_sims,
                         " simulations.")

jitter_caption = paste0("Each simulation is: ",
                        population_size,
                        " observations of an N(",
                        population_mean,
                        ", ",
                        population_sd^2,
                        ") random variable.")

sims_jitter_gg <- sims_summary_df %>%
  ggplot(aes(x = sample_size,
             y = ratio)) +
  geom_violin() +
  geom_jitter(alpha = 0.1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = jitter_title,
       subtitle = jitter_subtitle,
       caption = jitter_caption,
       x = "Standard error (systematic random sampling) / Standard error (simple random sampling)",
       y = "")
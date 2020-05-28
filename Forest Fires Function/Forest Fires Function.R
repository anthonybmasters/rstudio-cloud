## Install packages
library(igraph)
library(tidyverse)

## Adapted from comment: https://stackoverflow.com/questions/58949373/how-can-i-shorten-the-runtime-of-for-loops-and-if-statements-in-r-while-using-ig

## Set the plotting theme
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

## Plotting function
plot_sim_tidy <- function(sim_tidy_df){
  sim_tidy_df %>% ggplot() +
    geom_line(aes(x = time,
                  y = med),
              size = 1.5,
              color = "red") +
    geom_ribbon(aes(x = time,
                    ymin = lower_80,
                    ymax = upper_80),
                    fill = "grey12",
                alpha = 0.2) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(title = "Median on-fire or burnt trees over time",
         subtitle = "Median number of on-fire or burnt trees across time-steps, with 80% centred intervals.",
         caption = "Data: Forest fire simulations using lattice graphs with Moore neighbourhoods.",
         x = "Time Step",
         y = "")
}

## Forest fire simulation function
forest_fire_sim <- function(simulations = 100,
                            time_steps = 50,
                            fire_probability = 0.8,
                            burn_probability = 0.2,
                            fixed_start = TRUE,
                            forest_length = 20,
                            forest_density = 1)
{
  # Make the blank forest
  extra_edges_df <- tibble(x = as.numeric(1:(forest_length^2-forest_length)),
                           w = case_when(x %% forest_length == 1 ~ NA_real_,
                                         TRUE ~ x + forest_length - 1),
                           z = case_when(x %% forest_length == 0 ~ NA_real_,
                                         TRUE ~ x + forest_length + 1)) %>%
    pivot_longer(cols = 2:3,
                 names_to = "column",
                 values_to = "y",
                 values_drop_na = TRUE) %>%
    select("x", "y")
  extra_edges_v <- cbind(extra_edges_df$x, extra_edges_df$y) %>% t() %>% c()
  
  blank_forest <- graph.lattice(
    length = forest_length,
    dim = 2) %>% #to make a square forest
    add_edges(edges = extra_edges_v)
  V(blank_forest)$color <- "grey"
  sim_results <- tibble()
  
  for (x in 1:simulations) {
    # Each iteration has different starting forests with fixed tree density
    G <- blank_forest
    V(G)$color[sample(forest_length^2, (forest_length^2*forest_density))] <- "green"
    
    # Fires could start at a random tree or in the centre
    if(fixed_start){
      V(G)$color[round(forest_length^2/2)-(forest_length/2) ] <- "red" }
    else{
      V(G)$color[sample(forest_length^2, 1)] <- "red" }
    
    # Set the initial simulation data row
    sim_counts <- tibble(alive = length(which(V(G)$color == "green")),
                          fire = length(which(V(G)$color == "red")),
                          burnt = length(which(V(G)$color == "black")),
                          time = 0,
                          simulation = x)
    
    for(h in 1:time_steps){
      # Burn out trees that are on fire using burn_probability
      trees_on_fire <- V(G)[V(G)$color %in% c("red", "orange")] 
      V(G)$color[trees_on_fire[runif(length(trees_on_fire), 0, 1) <= burn_probability]] <- "black"
      
      # Set green neighbour trees of burning trees on fire with fire_probability
      ignited <-  V(G)$color == "red"
      possible_fires <- adjacent_vertices(G, ignited, mode = "total")
      
      # The check is for unique vertices
      possible_fires <- unique(unlist(possible_fires))
      tree_fires <- possible_fires[runif(length(possible_fires), 0, 1) <= fire_probability]
      
      # Store the prior burning trees as orange
      V(G)$color[ignited] <- "orange"
      
      # Set all identified neighbours of flaming trees on fire at once
      V(G)$color[tree_fires][V(G)$color[tree_fires] == "green"] <- "red" 
      
      # Add new row to the simulation data
      sim_counts <- sim_counts %>%
        add_row(alive = length(which(V(G)$color == "green")),
                fire = length(which(V(G)$color %in% c("red", "orange"))),
                burnt = length(which(V(G)$color == "black")),
                time = h,
                simulation = x)
    }
    sim_results <- bind_rows(sim_results, sim_counts)
  }
  sim_results <- bind_rows(sim_results, sim_counts)
  
  # Create the summary table and graph
  sim_tidy_df <- sim_results %>%
    mutate(fire_or_burnt = fire + burnt) %>%
    group_by(time) %>%
    summarise(med = median(fire_or_burnt),
              lower_80 = quantile(fire_or_burnt, prob = 0.1, na.rm = TRUE),
              upper_80 = quantile(fire_or_burnt, prob = 0.9, na.rm = TRUE))
  sim_tidy_gg <- plot_sim_tidy(sim_tidy_df)
  
  # Publish the results
  plot(sim_tidy_gg)
  print(sim_results)
}

## For testing
set.seed(6347)
forest_fire_sim(fire_probability = 0.5,
                burn_probability = 0.1,
                time_steps = 15,
                simulations = 10)
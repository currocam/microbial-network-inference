source("renv/activate.R")
library(tidyverse)
library(igraph)
library(ggsci)
library(ggraph)
source("figures/theme.R")

infiles <- list.files("steps/fits/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) %>% str_remove(".Rds")
set.seed(100)
data <- map(infiles, read_rds) %>%
  bind_rows() %>%
  select(true_graph, seed, graph_type) |>
  group_by(graph_type) |>
  slice_sample(n = 1)
  
data$true_graph[[1]] |>
  ggraph(layout = 'kk') + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) + 
  geom_node_point()

ggsave(
  "figures/11-network_plots/cluster_100.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)

data$true_graph[[2]] |>
  ggraph(layout = 'kk') + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) + 
  geom_node_point()


ggsave(
  "figures/11-network_plots/hub_100.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)


data$true_graph[[3]] |>
  ggraph(layout = 'kk') + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) + 
  geom_node_point()

set.seed(500)
data <- map(infiles, read_rds) %>%
  bind_rows() %>%
  select(true_graph, seed, graph_type) |>
  group_by(graph_type) |>
  slice_sample(n = 1)
data$true_graph[[3]] |>
  ggraph(layout = 'kk') + 
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) + 
  geom_node_point()


ggsave(
  "figures/11-network_plots/random_500.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height
)


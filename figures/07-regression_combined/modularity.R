source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")
source("src/network_metrics.R")

infiles <- list.files("steps/fits_combined/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |>
  bind_rows() |>
  rowwise() |>
  mutate(
    inferred = BDgraph::select(bdgraph, cut = 0.5) %>% 
      graph_from_adjacency_matrix(mode = "undirected") %>%
      list(),
    true_mod = compute_modularity(true_graph),
    inferred_mod = compute_modularity(inferred),
    difference_mod = true_mod - inferred_mod
  )

ggplot(data, aes(x = n, y = difference_mod)) +
  geom_boxplot(aes(group = n)) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("Modularity error") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/07-regression_combined/modularity_boxplot.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

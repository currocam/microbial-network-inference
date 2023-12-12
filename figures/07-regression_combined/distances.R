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
    spearman = cor(
      compute_distance(true_graph),
      compute_distance(inferred),
      method = "spearman"
    )
  )

ggplot(data, aes(x = n, y = spearman)) +
  geom_boxplot(aes(group = n)) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("Spearman correlation of the distances") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/07-regression_combined/distances_spearman.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

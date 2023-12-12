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
    mean_hub_error = summary_hub_score_square_error
    (true_graph, inferred, mean)
  )

ggplot(data, aes(x = n, y = mean_hub_error)) +
  geom_boxplot(aes(group = n)) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("MSE of the hub-score") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/07-regression_combined/hub_score.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")
source("src/network_metrics.R")

infiles <- list.files("steps/fits/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |>
  bind_rows() |>
  pivot_longer(
    cols = c("bdgraph_graph", "pulsar_graph"),
    names_to = "method", values_to = "inferred"
  ) |>
  rowwise() |>
  filter(gen != "nbinom_shr") |>
  mutate(
    spearman = cor(
      compute_distance(true_graph),
      compute_distance(inferred),
      method = "spearman"
    )
  )

ggplot(data, aes(x = n, y = spearman, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  facet_wrap(~gen) +
  scale_colour_discrete(labels = c("Bayesian", "SpiecEASI")) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("Spearman correlation of the distances") +
  labs(color = "") +
  theme(legend.position = "bottom")

ggsave(
  "figures/02-regression/distances_spearman.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

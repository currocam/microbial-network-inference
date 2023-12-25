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

data |>
  filter(gen == "normal") |>
  ggplot(aes(x = n, y = spearman, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  scale_colour_discrete(labels = c("Bayesian", "SpiecEASI")) +
  theme_classic() +
  xlab("Number of samples") +
  ylab("Spearman correlation of the distances") +
  ylim(0, 1) +
  labs(color = "") +
  theme(legend.position = "none")

ggsave(
  "figures/02-regression/distances_spearman_normal.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

data |>
  filter(gen == "nbinom") |>
  ggplot(aes(x = n, y = spearman, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  scale_colour_discrete(labels = c("Bayesian", "SpiecEASI")) +
  theme_classic() +
  ylim(0, 1) +
  xlab("Number of samples") +
  ylab("Spearman correlation of the distances") +
  labs(color = "") +
  theme(legend.position = "right")

ggsave(
  "figures/02-regression/distances_spearman_counts.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

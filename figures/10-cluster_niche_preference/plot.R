source("renv/activate.R")
library(tidyverse)
library(igraph)
library(ggsci)
source("figures/theme.R")
source("src/network_metrics.R")

infiles <- list.files("steps/niche_preference/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |>
  bind_rows() |>
  pivot_longer(
    cols = c("bdgraph_graph", "pulsar_graph"),
    names_to = "method", values_to = "inferred"
  ) |>
  rowwise() |>
  filter(graph_type == "cluster") |>
  filter(gen != "nbinom_shr") |>
  mutate(
    ami = compute_adjusted_mutual_information(
      true_graph, inferred
    )
  )

ggplot(data, aes(x = n, y = ami, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  scale_color_nejm(labels = c("Bayesian", "SpiecEASI")) +
  theme_bw() +
  xlab("Number of samples") +
  ylab("Adjusted mutual information") +
  ylim(0, 1) +
  labs(color = "") +
  theme(legend.position = "none")

ggsave(
  "figures/10-cluster_niche_preference/cluster_ami.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

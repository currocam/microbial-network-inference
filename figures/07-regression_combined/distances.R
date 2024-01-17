source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")
library(ggsci)
source("src/network_metrics.R")

infiles <- list.files("steps/fits_combined/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |>
  bind_rows() |>
  rowwise() |>
  mutate(
    method = "combined",
    spearman = cor(
      compute_distance(true_graph),
      compute_distance(combined),
      method = "spearman"
    )
  )

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
  ) |>
  bind_rows(data)

data |>
  filter(gen == "nbinom") |>
  mutate(method = fct_relevel(method, "combined", after = Inf)) |>
  filter(method != "combined") |>
  ggplot(aes(x = n, y = spearman, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method)), position = position_dodge2(padding = .1)) +
  theme_bw() +
  xlab("Number of samples") +
  ylab("Spearman correlation of the distances") +
  scale_color_nejm(labels = c("Bayesian", "SpiecEASI", "Combined")) +
  labs(color = "") +
  theme(legend.position = "none")

ggsave(
  "figures/07-regression_combined/distances_spearman.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

source("renv/activate.R")
library(tidyverse)
library(igraph)
library(ggsci)
source("figures/theme.R")
source("src/network_metrics.R")

infiles <- list.files("steps/fits_combined/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |>
  bind_rows() |>
  rowwise() |>
  mutate(
    method = "combined",
    true_mod = compute_modularity(true_graph),
    inferred_mod = compute_modularity(combined),
    difference_mod = true_mod - inferred_mod
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
    true_mod = compute_modularity(true_graph),
    inferred_mod = compute_modularity(inferred),
    difference_mod = true_mod - inferred_mod
  ) |>
  bind_rows(data)

data |>
  filter(gen == "nbinom") |>
  filter(method != "combined") |>
  #mutate(method = fct_relevel(method, "combined", after = Inf)) |>
  ggplot(aes(x = n, y = difference_mod, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  theme_bw() +
  scale_y_reverse() +
  xlab("Number of samples") +
  ylab("Modularity error") +
  scale_color_nejm(labels = c("Bayesian", "SpiecEASI", "Combined")) +
  labs(color = "") +
  theme(legend.position = "none")+
  ylim(-1, 1)

ggsave(
  "figures/07-regression_combined/modularity_boxplot.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

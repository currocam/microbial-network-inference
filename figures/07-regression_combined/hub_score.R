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
    mean_hub_error = summary_hub_score_square_error
    (true_graph, combined, mean)
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
    mean_hub_error = summary_hub_score_square_error
    (true_graph, inferred, mean)
  ) |>
  bind_rows(data)

data |>
  filter(gen == "nbinom") |>
  filter(method != "combined") |>
  # mutate(method = fct_relevel(method, "combined", after = Inf)) |>
  ggplot(aes(x = n, y = mean_hub_error, colour = method)) +
  geom_boxplot(aes(group = paste0(n, method))) +
  theme_bw() +
  #scale_y_reverse() +
  xlab("Number of samples") +
  ylab("MSE of the hub-score") +
  scale_color_nejm(labels = c("Bayesian", "SpiecEASI", "Combined")) +
  labs(color = "") +
  theme(legend.position = "none")

ggsave(
  "figures/07-regression_combined/hub_score.pdf",
  units = "mm",
  width = fig.witdh, height = fig.height,
)

source("renv/activate.R")
library(tidyverse)
library(igraph)
library(ggsci)
source("figures/theme.R")

infiles <- list.files("steps/ranked_edges/", ".Rds", full.names = TRUE)
names(infiles) <- basename(infiles) %>% str_remove(".Rds")
data <- map(infiles, read_rds) %>%
  bind_rows() %>%
  rowwise() %>%
  pivot_longer(
    cols = c("train_bdgraph_prob", "pulsar_graph"),
    names_to = "method", values_to = "inferred"
  )


topK_ranked_precision <- function(actual, pred, ks = seq(1, 500)) {
  actual <- as.matrix(actual) |> as.matrix()
  pred <- pred[upper.tri(pred)]
  actual <- actual[upper.tri(actual)]
  pred_order <- order(pred, decreasing = TRUE)
  map(ks, \(k) {
    tp <- sum(actual[pred_order[1:k]])
    fp <- k - tp
    tp / (tp + fp)
  }) |>
    as.numeric()
}

topK_ranked_threshold <- function(actual, pred, ks = seq(1, 500)) {
  actual <- as.matrix(actual) |> as.matrix()
  pred <- pred[upper.tri(pred)]
  actual <- actual[upper.tri(actual)]
  pred_order <- order(pred, decreasing = TRUE)
  pred[pred_order[ks]]
}

topK_ranked_precision_random <- function(actual, ks = seq(1, 500)) {
  actual <- as.matrix(actual) |> as.matrix()
  actual <- actual[upper.tri(actual)]
  map(ks, \(k) map(1:10, \(seed){
    set.seed(seed)
    tp <- sample(actual, size = k) |> sum()
    fp <- k - tp
    tp / (tp + fp)
  }) |>
    as.numeric()) |>
    reduce(c)
}


random_precision <- data |>
  dplyr::select(seed, true_graph, method) |>
  pmap(\(seed, true_graph, method) {
    tibble(
      seed = seed, n = "Random", method = method,
      k = map(seq(1, 1000), rep, 10) |> reduce(c)
    ) |>
      mutate(precision = topK_ranked_precision_random(true_graph, seq(1, 1000)))
  }) |>
  bind_rows()

data <- data |>
  dplyr::select(seed, n, true_graph, inferred, method) |>
  pmap(\(seed, n, true_graph, inferred, method) {
    tibble(seed = seed, n = n, method = method, k = seq(1, 1000)) |>
      mutate(
        precision = topK_ranked_precision(true_graph, inferred, k),
        threshold = topK_ranked_threshold(true_graph, inferred, k)
      )
  }) |>
  bind_rows()

data <- data |>
  mutate(
    n = paste0("n=", n)
  ) |>
  bind_rows(random_precision) |>
  mutate(
    method = if_else(method == "pulsar_graph", "SpiecEASI", "Bayesian"),
    n = fct_inorder(n)
  )
set.seed(100)
p1 <- data |>
  # mutate(k_int = cut_interval(k, n = 300)) |>
  # group_by(k_int, method, n) |>
  # mutate(k = mean(k), precision = mean(precision)) |>
  ggplot(aes(x = k, y = precision, colour = n)) +
  geom_smooth() +
  geom_point(
    data = data |>
      mutate(k_int = cut_interval(k, n = 300)) |>
      # filter(k > 10) |>
      # filter(threshold > 0.49, threshold < 0.51) |>
      group_by(k_int, method, n) |>
      slice_sample(n = 1) |>
      ungroup() |>
      slice_sample(n = 600),
    shape = 1, size = 0.8
  ) +
  facet_grid(~method) +
  scale_color_nejm() +
  theme_bw() +
  xlab("Top k-ranked edges") +
  ylab("Precision") +
  labs(color = "Number of samples") +
  ylim(c(0, 1)) +
  coord_cartesian(xlim = c(0, 500)) +
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    legend.key = element_blank()
  )

ggsave(
  "figures/09-ranked_edges/plot.pdf",
  p1,
  units = "mm",
  width = fig.witdh, height = fig.height
)

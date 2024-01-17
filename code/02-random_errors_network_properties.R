#! Rscript --vanilla
source("renv/activate.R")
library(igraph)
library(tidyverse)
library(huge)
library(furrr)

source("src/network_metrics.R")

modify_graph <- function(true_graph, n_missing, n_spurious) {
  edges_not_in_g <- complementer(true_graph) %>% as_edgelist()
  edges_in_g <- as_edgelist(true_graph) %>% apply(1, paste0, collapse = "|")
  if (n_missing > length(edges_in_g)) {
    return(NULL)
  }
  if (n_missing > 0) {
    true_graph <- delete_edges(true_graph, sample(edges_in_g, n_missing))
  }
  if (n_spurious > 0) {
    indexes <- sample(1:nrow(edges_not_in_g), n_spurious)
    true_graph <- true_graph + edges(t(edges_not_in_g[indexes, ]) %>% as.vector())
  }
  true_graph
}

run <- function(graph, seed) {
  set.seed(seed)
  d <- 100
  max_edges <- 100 * 99 / 2
  size <- case_match(
    graph,
    "random" ~ round(max_edges * runif(1, 0.01, 0.1)),
    "hub" ~ sample(1:10, size = 1),
    "cluster" ~ 0
  )
  if (size == 0) {
    size <- NULL
  }
  g <- BDgraph::bdgraph.sim(
    p = 100, graph = graph, size = size
  ) %>%
    pluck("G") %>%
    graph_from_adjacency_matrix(mode = "undirected")
  n_edges_fully_connected <- d * (d - 1) / 2
  n_edges <- E(g) %>% length()

  res <- expand_grid(
    true_graph = list(g),
    n_missing = round(runif(n = 100, 0, 1) * n_edges),
    n_spurious = round(runif(n = 100, 0, 1) * (n_edges_fully_connected - n_edges))
  )

  plan(multisession, workers = 8)
  graphs <- future_pmap(
    res, modify_graph,
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )

  res$true_graph <- NULL

  tp <- n_edges - res$n_missing
  res$precision <- tp / (tp + res$n_spurious)
  res$recall <- tp / (tp + res$n_missing)
  res$f1 <- 2 * tp / (2 * tp + res$n_spurious + res$n_missing)

  res$modularity_error <- future_map(graphs, compute_modularity, .progress = TRUE) %>%
    as.numeric()
  res$modularity_error <- res$modularity_error - compute_modularity(g)

  true_distances <- compute_distance(g)
  res$spearman <- future_map(graphs, \(g) {
    compute_distance(g) %>% cor(true_distances, method = "spearman")
  }, .progress = TRUE) %>%
    as.numeric()

  res$ami <- future_map(
    graphs,
    .progress = TRUE, .options = furrr_options(seed = TRUE),
    \(est) compute_adjusted_mutual_information(g, est)
  ) %>%
    as.numeric()
  res
}

walk(100:115, \(seed) run("random", seed) %>%
  write_rds(paste0("steps/random_errors/random_", seed, ".Rds")))

walk(100:115, \(seed) run("hub", seed) %>%
  write_rds(paste0("steps/random_errors/hub_", seed, ".Rds")))

walk(100:115, \(seed) run("cluster", seed) %>%
  write_rds(paste0("steps/random_errors/cluster_", seed, ".Rds")))

#! Rscript --vanilla
source("renv/activate.R")
library(igraph)
library(tidyverse)
library(BDgraph)
library(VGAM)
library(compositions)

source("src/network_metrics.R")
source("src/train.R")

simulate_graphs_from_plinks <- function(fit, n, seed) {
  set.seed(seed)
  sim_graph <- matrix(0, 100, 100)
  probs <- as.numeric(fit$p_links[upper.tri(fit$p_links)])
  indicator <- map(probs, \(p) rbinom(n, 1, p)) |> reduce(rbind)
  graphs <- apply(indicator, 2, \(ind){
    sim_graph <- sim_graph * 0
    sim_graph[upper.tri(sim_graph)] <- ind
    graph_from_adjacency_matrix(sim_graph, mode = "undirected")
  })
  graphs
}

run <- function(graph, seed) {
  set.seed(seed)
  # We are going to simulate graphs from 5% to 15% dense
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
  gen <- bdgraph.sim(
    p = 100, graph = graph, n = 1000, type = "nbinom", size = size
  )
  depth_factor <- runif(1000, 0.75, 1.25)
  counts <- as.data.frame(round(gen$data * depth_factor) + 1)
  true_graph <- gen$G |>
    graph_from_adjacency_matrix(mode = "undirected")

  true_mod <- compute_modularity(true_graph)
  true_dist <- compute_distance(true_graph)

  res <- tibble(.rows = 0)

  walk(c(50, 100, 200, 500), .progress = TRUE, \(n){
    data <- counts |>
      slice_sample(n = n) |>
      clr() |>
      as.matrix()
    fit_bd <- bdgraph(
      data = data,
      method = "ggm",
      iter = 50000,
      cores = 8
    )
    res <<- bind_rows(
        res, map(1:100, \(i){
          g <- simulate_graphs_from_plinks(fit_bd, 1, i)[[1]]
          list(
            modularity_error = true_mod - compute_modularity(g),
            spearman = compute_distance(g) |> cor(true_dist, method = "spearman"),
            hub_error = compute_mean_error_hub_score(g, true_graph)
          )
        }) |>
          bind_rows() |>
          map(quantile, seq(0, 1, 0.025)) |>
          bind_rows(.id = "metric") |>
          mutate(n = n, method = "bdgraph")
      )
  })
  res
}

run2 <- function(graph, seed) {
  set.seed(seed)
  # We are going to simulate graphs from 5% to 15% dense
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
  gen <- bdgraph.sim(
    p = 100, graph = graph, n = 1000, type = "nbinom", size = size
  )
  depth_factor <- runif(1000, 0.75, 1.25)
  counts <- as.data.frame(round(gen$data * depth_factor) + 1)
  true_graph <- gen$G |>
    graph_from_adjacency_matrix(mode = "undirected")

  true_mod <- compute_modularity(true_graph)
  true_dist <- compute_distance(true_graph)

  res <- tibble(.rows = 0)

  walk(c(50, 100, 200, 500), .progress = TRUE, \(n){
    data <- counts |>
      slice_sample(n = n) |>
      clr() |>
      as.matrix()
    fit_bd <- bdgraph(
      data = data,
      method = "ggm",
      iter = 50000,
      cores = 8
    )
    res <<- bind_rows(
        res, map(1:100, \(i){
          g <- simulate_graphs_from_plinks(fit_bd, 1, i)[[1]]
          list(
            ami = compute_adjusted_mutual_information(g, true_graph)
          )
        }) |>
          bind_rows() |>
          map(quantile, seq(0, 1, 0.025)) |>
          bind_rows(.id = "metric") |>
          mutate(n = n, method = "bdgraph")
      )
  })
  res
}

walk(100:104, \(seed) run("random", seed) |>
  write_rds(paste0("steps/network_properties_bma/random_", seed, ".Rds")))

walk(100:104, \(seed) run2("cluster", seed) |>
  write_rds(paste0("steps/network_properties_bma/cluster_", seed, ".Rds")))

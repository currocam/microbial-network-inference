#! Rscript --vanilla
source("renv/activate.R")
library(igraph)
library(tidyverse)
library(huge)
library(BDgraph)
library(VGAM)
library(compositions)
library(pulsar)

source("src/network_metrics.R")
source("src/train.R")

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
      save = TRUE,
      cores = 8
    )
    n_samples <- length(fit_bd$graph_weights)
    random_samples <- 1:n_samples |>
      sample(size = 5000, replace = TRUE, prob = fit_bd$graph_weights)

    res <<- bind_rows(
      res, map(random_samples, \(i){
        g <- bdgraph2igraph(fit_bd$sample_graphs[[i]])
        list(
          modularity_error = true_mod - compute_modularity(g),
          spearman = compute_distance(g) |> cor(true_dist, method = "spearman"),
          # ami = compute_adjusted_mutual_information(g, true_graph),
          hub_error = compute_mean_error_hub_score(g, true_graph)
        )
      }) |>
        bind_rows() |>
        map(quantile, seq(0, 1, 0.025)) |>
        bind_rows(.id = "metric") |>
        mutate(n = n, method = "bdgraph")
    )
  })

  walk(c(50, 100, 200, 500), .progress = TRUE, \(n){
    data <- counts |>
      slice_sample(n = n) |>
      clr() |>
      as.data.frame()
    res <<- bind_rows(
      res,
      map(1:50, .progress = TRUE, \(seed){
        set.seed(1000 + seed)
        g <- slice_sample(data, n = n, replace = TRUE) |>
          train_pulsar()
        list(
          modularity_error = true_mod - compute_modularity(g),
          spearman = compute_distance(g) |> cor(true_dist, method = "spearman"),
          # ami = compute_adjusted_mutual_information(g, true_graph),
          hub_error = compute_mean_error_hub_score(g, true_graph)
        )
      }) |>
        bind_rows() |>
        map(quantile, seq(0, 1, 0.025)) |>
        bind_rows(.id = "metric") |>
        mutate(n = n, method = "pulsar")
    )
  })
  res
}

walk(100:104, \(seed) run("random", seed) |>
  write_rds(paste0("steps/credibility_confidence_intervals/random_", seed, ".Rds")))

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
      save = TRUE,
      cores = 8
    )
    n_samples <- length(fit_bd$graph_weights)
    random_samples <- 1:n_samples |>
      sample(size = 5000, replace = TRUE, prob = fit_bd$graph_weights)

    res <<- bind_rows(
      res, map(random_samples, \(i){
        g <- bdgraph2igraph(fit_bd$sample_graphs[[i]])
        list(
          # modularity_error = true_mod -compute_modularity(g),
          # spearman = compute_distance(g) |> cor(true_dist, method = "spearman"),
          ami = compute_adjusted_mutual_information(g, true_graph)
          # hub_error = compute_mean_error_hub_score(g, true_graph)
        )
      }) |>
        bind_rows() |>
        map(quantile, seq(0, 1, 0.025)) |>
        bind_rows(.id = "metric") |>
        mutate(n = n, method = "bdgraph")
    )
  })

  walk(c(50, 100, 200, 500), .progress = TRUE, \(n){
    data <- counts |>
      slice_sample(n = n) |>
      clr() |>
      as.data.frame()
    res <<- bind_rows(
      res,
      map(1:50, .progress = TRUE, \(seed){
        set.seed(1000 + seed)
        g <- slice_sample(data, n = n, replace = TRUE) |>
          train_pulsar()
        list(
          # modularity_error = true_mod -compute_modularity(g),
          # spearman = compute_distance(g) |> cor(true_dist, method = "spearman"),
          ami = compute_adjusted_mutual_information(g, true_graph)
          # hub_error = compute_mean_error_hub_score(g, true_graph)
        )
      }) |>
        bind_rows() |>
        map(quantile, seq(0, 1, 0.025)) |>
        bind_rows(.id = "metric") |>
        mutate(n = n, method = "pulsar")
    )
  })
  res
}

walk(100:104, \(seed) run2("cluster", seed) |>
  write_rds(paste0("steps/credibility_confidence_intervals/cluster_", seed, ".Rds")))

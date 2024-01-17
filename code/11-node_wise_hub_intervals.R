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

  true_hubs <- hub_score(true_graph, scale = TRUE)$vector
  
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

    walk(random_samples, \(i){
        g <- bdgraph2igraph(fit_bd$sample_graphs[[i]])
        hubs <- hub_score(g, scale = TRUE)$vector
        res <<- bind_rows(
          res,
          tibble(
            method = "bdgraph",
            n = n,
            seed = i,
            hub = list(hubs)
          )
        )
  })
  })

  walk(c(50, 100, 200, 500), .progress = TRUE, \(n){
    data <- counts |>
      slice_sample(n = n) |>
      clr() |>
      as.data.frame()
    walk(1:100, \(seed){
        set.seed(1000 + seed)
        g <- slice_sample(data, n = n, replace = TRUE) |>
          train_pulsar()
        hubs <- hub_score(g, scale = TRUE)$vector
        res <<- bind_rows(
          res,
          tibble(
            method = "pulsar",
            n = n,
            seed = seed,
            hub = list(hubs)
          )
        )
    })
  })
  bind_rows(
    res,
    tibble(
      method = "true",
      n = 0,
      seed = 0,
      hub = list(true_hubs)
    )
  ) 
}

#run("random", 666) |> write_rds("steps/node_wise_intervals/random.Rds")

walk(666:676, \(seed){
  run("random", seed) |> write_rds(
    glue::glue("steps/node_wise_intervals/random_{seed}.Rds")
  )
})
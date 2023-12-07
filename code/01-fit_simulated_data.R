#! Rscript --vanilla
source("renv/activate.R")
library(huge)
library(igraph)
library(pulsar)
library(BDgraph)
library(tidyverse)
library(VGAM)
library(compositions)

train_pulsar <- function(data) {
  data <- as.matrix(data)
  max_lamda <- getMaxCov(cor(data))
  path <- getLamPath(max_lamda, max_lamda * 0.001, len = 50)
  hugeargs <- list(lambda = path, verbose = FALSE)
  fit <- pulsar(
    data,
    fun = huge::huge, fargs = hugeargs,
    rep.num = 20, criterion = "stars", thresh = 0.05,
    lb.stars = TRUE, ub.stars = TRUE, ncores = 8
  )
  print(dim(data))
  fit$est$path[[fit$stars$opt.index]] %>%
    graph_from_adjacency_matrix(mode = "undirected")
}

train_bdgraph <- function(data) {
  data <- as.matrix(data)
  fit <- bdgraph(
    data = data,
    method = "ggm",
    iter = 10000,
    cores = 8
  )
  print(dim(data))
  fit %>%
    BDgraph::select(cut = 0.5) %>%
    graph_from_adjacency_matrix(mode = "undirected")
}


run <- function(graph, seed) {
  set.seed(seed)
  # Define the sample sizes we are going to use
  sample_sizes <- c(25, 100, 200, 400, 1000)
  # We are going to simulate graphs from 5% to 15% dense if random graph
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
    p = 100, graph = graph, n = 1000, type = "Gaussian", size = size
  )
  t_graph_normal <- gen$G %>%
    graph_from_adjacency_matrix(mode = "undirected")
  normal_data <- as.data.frame(gen$data)

  gen <- bdgraph.sim(
    p = 100, graph = graph, n = 1000, type = "nbinom", size = size
  )
  t_graph_counts <- gen$G %>%
    graph_from_adjacency_matrix(mode = "undirected")
  depth_factor <- runif(1000, 0.75, 1.25)
  counts <- as.data.frame(round(gen$data * depth_factor) + 1)

  res_normal <- expand_grid(
    seed = seed, graph_type = graph,
    true_graph = list(t_graph_normal),
    n = sample_sizes, gen = "normal"
  )
  res_normal$pulsar_graph <- res_normal$n %>%
    map(\(n) slice_sample(normal_data, n = n) %>% train_pulsar())

  res_normal$bdgraph_graph <- res_normal$n %>%
    map(\(n) slice_sample(normal_data, n = n) %>% train_bdgraph())

  res_negb <- expand_grid(
    seed = seed, graph_type = graph,
    true_graph = list(t_graph_counts),
    n = sample_sizes, gen = "nbinom"
  )
  res_negb$pulsar_graph <- res_negb$n %>%
    map(\(n) slice_sample(counts, n = n) %>%
      clr() %>%
      train_pulsar())

  res_negb$bdgraph_graph <- res_negb$n %>%
    map(\(n) slice_sample(counts, n = n) %>%
      clr() %>%
      train_bdgraph())

  bind_rows(res_normal, res_negb)
}

expand_grid(
  type = c("random", "hub", "cluster"),
  seed = 600:610
) %>%
  pwalk(.progress = TRUE, \(type, seed) {
    outfile <- paste0("steps/fits/", type, "_", seed, ".Rds")
    print(outfile)
    if (!file.exists(outfile)) {
      run(type, seed) %>% write_rds(outfile)
    }
  })

tibble(
  type = "random",
  seed = 600:630
) %>%
  pwalk(.progress = TRUE, \(type, seed) {
    outfile <- paste0("steps/fits/", type, "_", seed, ".Rds")
    print(outfile)
    if (!file.exists(outfile)) {
      run(type, seed) %>% write_rds(outfile)
    }
  })

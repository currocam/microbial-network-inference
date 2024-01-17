bdgraph2igraph <- function(g) {
  p <- 100
  vec_g <- c(rep(0, p * (p - 1) / 2))
  vec_g[which(unlist(strsplit(as.character(g), "")) == 1)] <- 1
  m <- matrix(0, p, p)
  m[upper.tri(m)] <- vec_g
  graph_from_adjacency_matrix(m, mode = "undirected")
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

train_pulsar <- function(data) {
  data <- as.matrix(data)
  max_lamda <- getMaxCov(data)
  path <- getLamPath(max_lamda, max_lamda * 0.001, len = 50)
  hugeargs <- list(lambda = path, verbose = FALSE)
  fit <- pulsar(
    data,
    fun = huge::huge, fargs = hugeargs,
    rep.num = 20, criterion = "stars", thresh = 0.05,
    lb.stars = TRUE, ub.stars = TRUE, ncores = 8
  )
  print(dim(data))
  fit$est$path[[fit$stars$opt.index]] |>
    graph_from_adjacency_matrix(mode = "undirected")
}

train_pulsar_confidence_scores <- function(data) {
  data <- as.matrix(data)
  max_lamda <- getMaxCov(data)
  path <- getLamPath(max_lamda, max_lamda * 0.001, len = 50)
  hugeargs <- list(lambda = path, verbose = FALSE)
  fit <- pulsar(
    data,
    fun = huge::huge, fargs = hugeargs,
    rep.num = 100, criterion = "stars", thresh = 0.05,
    lb.stars = TRUE, ub.stars = TRUE, ncores = 8
  )
  print(dim(data))
  fit$stars$merge[[fit$stars$opt.index]]
}

train_bdgraph_prob <- function(data) {
  data <- as.matrix(data)
  fit <- bdgraph(
    data = data,
    method = "ggm",
    iter = 20000,
    cores = 8
  )
  print(dim(data))
  fit$p_links
}

sample_from_posterior <- function(x) {
  1:length(x$sample_graphs) |>
    sample(size = 1000, replace = TRUE, prob = x$graph_weights) |>
    map(\(i) bdgraph2igraph(x$sample_graphs[[i]]))
}

train_pulsar_niche <- function(data) {
  data <- as.matrix(data)
  max_lamda <- getMaxCov(data)
  path <- getLamPath(max_lamda, max_lamda * 0.001, len = 50)
  hugeargs <- list(lambda = path, verbose = FALSE, method = "glasso")
  fit <- pulsar(
    data,
    fun = huge::huge, fargs = hugeargs,
    rep.num = 20, criterion = "stars", thresh = 0.05,
    lb.stars = TRUE, ub.stars = TRUE, ncores = 8
  )
  print(dim(data))
  precision <- fit$est$icov[[fit$stars$opt.index]]
  g <- (precision < 0) * fit$est$path[[fit$stars$opt.index]]
  graph_from_adjacency_matrix(g, mode = "undirected")
}

train_bdgraph_niche <- function(data) {
  data <- as.matrix(data)
  fit <- bdgraph(
    data = data,
    method = "ggm",
    iter = 10000,
    cores = 8
  )
  print(dim(data))
  g <- (fit$K_hat < 0) * BDgraph::select(fit, cut = 0.5)
  graph_from_adjacency_matrix(g, mode = "undirected")
}


train_combined <- function(data) {
  start <- train_pulsar(data) |>
    as.matrix() |>
    as.matrix()
  fit <- bdgraph(
    data = data,
    method = "ggm",
    g.start = start,
    iter = 10000,
    cores = 8
  )
  print(dim(data))
  fit %>%
    BDgraph::select(cut = 0.5) %>%
    graph_from_adjacency_matrix(mode = "undirected")
}

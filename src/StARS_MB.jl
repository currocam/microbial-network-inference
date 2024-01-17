# A basic implementation of Meinshausen and Buhlmann (2006) for estimating
# an sparse inverse covariance matrix with StARS selection
using GLMNet
using RData
import CodecBzip2
using BenchmarkTools
using LightGraphs
using StatsBase
using Random
using LinearAlgebra
using StatsBase
using RCall


# Compute the graphical lasso using Meinshausen and Buhlmann (2006)
# The function takes a matrix X and a grid of lambda values
# It expect to recieve a vector of empty graphs, one for each lambda
# that will be filled with the edges of the estimated graph
function mb!(X, lambdas, graphs)
  n_features = size(X, 2)
  n_lambdas = length(lambdas)
  for i in 1:n_features
    y = X[:, i]
    train = X[:, filter(j -> j != i, 1:n_features)]
    model = glmnet(train, y, alpha=1, lambda=lambdas)

    for lambda_index in 1:n_lambdas
      non_zero_indices = findall(x -> x != 0, model.betas[:, lambda_index])
      for p in non_zero_indices
        j = p < i ? p : p + 1
        add_edge!(graphs[lambda_index], i, j)
      end
    end
  end
end

# Compute the path of lambdas using the same heuristic as pulsar
function lambda_path(X, lambda_min_ratio, nlambda)
  # Compute correlation matrix
  mat = cor(X) - Matrix{Float64}(I, size(X, 2), size(X, 2))
  max_lambda = max(maximum(mat), -minimum(mat))
  min_lambda = max_lambda * lambda_min_ratio
  exp10.(range(log10(min_lambda), log10(max_lambda), length=nlambda))
end

# Select the optimal lambda based on the stability selection procedure
# β is the threshold for the average instability
# α is the threshold of inclusion for the final graph
function StARS(X, n_subsamples, β=0.05)
  n, n_features = size(X)
  # Create path of lambdas
  path = lambda_path(X, 0.001, 100)
  # Create empty graphs
  empty_graphs = [SimpleDiGraph(n_features) for _ in 1:length(path)]
  # Create empty scores, number of times we have an edge between two nodes
  scores = [zeros(Float64, n_features, n_features) for _ in 1:length(path)]
  # Decide subsample ratio
  subsample_ratio = n > 144 ? 10 * sqrt(n) / n : 0.8
  # Draw subsamples
  for _ in 1:n_subsamples
    random_indexes = sample(1:n, Int(round(subsample_ratio * n)), replace=false)
    subsample_graphs = copy(empty_graphs)
    mb!(X[random_indexes, :], path, subsample_graphs)
    for lambda_index in 1:length(path)
      g = subsample_graphs[lambda_index]
      scores[lambda_index] += (adjacency_matrix(g) .+ adjacency_matrix(g)') .== 2
    end
  end
  # Normalize scores
  scores ./= n_subsamples
  # Compute instability from scores
  instability = Vector{Matrix{Float64}}(undef, length(path))
  for index in 1:length(path)
    instability[index] = 2 .* scores[index] .- scores[index] .* scores[index]
  end
  # Choose optimal lambda based on instability and beta threshold
  ave_instability = sum.(instability)
  ave_instability ./= n_features * (n_features - 1)
  opt_index = findfirst(x -> x < β, ave_instability)

  # Fit final model
  g = SimpleDiGraph(n_features)
  for i in 1:n_features
    y = X[:, i]
    train = X[:, filter(j -> j != i, 1:n_features)]
    model = glmnet(train, y, alpha=1, lambda=[path[opt_index]])
    non_zero_indices = findall(x -> x != 0, model.betas[:, 1])
    for p in non_zero_indices
      j = p < i ? p : p + 1
      add_edge!(g, i, j)
    end
  end
  opt_graph = SimpleGraph(n_features)
  for i in 1:n_features
    for j in i:n_features
      if has_edge(g, i, j) && has_edge(g, j, i)
        add_edge!(opt_graph, i, j)
      end
    end
  end
  return opt_graph
end

## Example
R"""
z <- huge::huge.generator(n = 100, d = 100, graph ="random")
theta <- as.matrix(z$theta)
data <- as.matrix(z$data)
"""
@rget theta
@rget data
true_graph = SimpleGraph(theta .!= 0)
opt = StARS(data, 20)
tp = length(intersect(edges(opt), edges(true_graph)))
fp = length(edges(opt)) - tp
fn = length(edges(true_graph)) - tp
precision = tp / (tp + fp)
recall = tp / (tp + fn)

function lasso(X, Y, λ)
  n, p = size(X)
  β = zeros(p)
  β_old = zeros(p)
  # Cache  some computations
  XTY = X' * Y
  XTX = X' * X
  # Initialize β
  for j in 1:p
    update = (1 / n) * XTY[j]
    β[j] = sign(update) * max(abs(update) - λ, 0)
  end
  β_old .= β
  # While not converged
  while true
    for j in 1:p
      update = (1 / n) * (XTY[j] - XTX[j, k] * β[k]) + β[j]
      β[j] = sign(update) * max(abs(update) - λ, 0)
    end
    if norm(β - β_old) < 1e-6
      break
    end
    β_old .= β
  end


end

# Add intercept to data
X = [ones(size(data, 1)) data]
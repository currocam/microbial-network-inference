source("renv/activate.R")
library(tidyverse)
library(igraph)
source("figures/theme.R")
source("src/network_metrics.R")

infiles <- list.files("steps/fits/", ".Rds", full.names = TRUE) 
names(infiles) <- basename(infiles) |> str_remove(".Rds")

data <- map(infiles, read_rds) |> 
  bind_rows() |>
  pivot_longer(
    cols = c( "bdgraph_graph", "pulsar_graph"), 
    names_to = "method", values_to = "inferred"
  ) |>
  rowwise() |>
  filter(graph_type == "cluster") |>
  filter(gen != "nbinom_shr") |>
  mutate(
    ami = compute_adjusted_mutual_information(
      true_graph, inferred
    )
  )

ggplot(data, aes(x = n, y = ami, colour = method))+
  geom_boxplot(aes(group = paste0(n, method)))+
  facet_wrap(~gen)+
  scale_colour_discrete(labels = c("Bayesian", "SpiecEASI"))+
  theme_classic()+
  xlab("Number of samples")+
  ylab("Adjusted mutual information")+
  labs(color='')+
  theme(legend.position = "bottom")

ggsave(
  "figures/02-regression/cluster_ami.pdf",units = "mm",
  width = fig.witdh, height = fig.height,
)

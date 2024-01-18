# Assessing reliability of learning Gaussian graphical models from microbial abundances

I explored the reliability of the microbial networks inferred with [SpiecEASI](https://doi.org/10.1371/journal.pcbi.1004226) and a novel Bayesian method based on [BDgraph](https://cran.r-project.org/web/packages/BDgraph/index.html). You can find out [my slides hosted in Github pages](https://currocam.github.io/microbial-network-inference/). This project was part of my MSc. in Bioinformatics at Aarhus University.

## Reproducibility

All code is provided (directory code and src). You can easily reproduce my results using a Makefile. 

To reproduce the enviroment: 

```bash
make
```

To run the scripts:

```bash
make plan
make run
```

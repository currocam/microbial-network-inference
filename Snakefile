rule all:
    input:
        expand(
            "steps/{dir2}/.SUCCESS",
            dir2=[
                "fits",
                "random_errors",
                "credibility_confidence_intervals",
                "network_properties_bma",
                "network_properties_stability",
                "fits_combined",
            ],
        ),
        expand(
            "figures/{dir2}/.SUCCESS",
            dir2=[
                "01-precision-recall",
                "02-regression",
                "03-credibility-confidence",
                "04-network_properties_bma",
            ],
        ),


rule fit_simulated_data:
    input:
        script="code/01-fit_simulated_data.R",
        infiles="src/train.R",
    threads: 8
    output:
        expand(
            "steps/fits/{graph}_{n}.Rds",
            graph=["hub", "cluster"],
            n=range(600, 611),
        ),
        expand(
            "steps/fits/random_{n}.Rds",
            n=range(600, 631),
        ),
        touch("steps/fits/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule fit_simulated_data_combn:
    input:
        script="code/06-fit_combined.R",
        infiles="src/train.R",
    threads: 8
    output:
        expand(
            "steps/fits_combined/random_{n}.Rds",
            n=range(600, 611),
        ),
        touch("steps/fits_combined/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule random_errors_network_properties:
    input:
        infiles="src/network_metrics.R",
        script="code/02-random_errors_network_properties.R",
    threads: 8
    output:
        expand(
            "steps/random_errors/{graph}_{n}.Rds",
            graph=["random", "hub", "cluster"],
            n=range(100, 116),
        ),
        touch("steps/random_errors/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule confidence_credibility_intervals:
    input:
        infiles=["src/network_metrics.R", "src/train.R"],
        script="code/03-credibility_confidence_intervals.R",
    output:
        expand(
            "steps/credibility_confidence_intervals/{graph}_{n}.Rds",
            graph=["random", "cluster"],
            n=range(100, 105),
        ),
        touch("steps/credibility_confidence_intervals/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule network_properties_bma:
    input:
        infiles=["src/network_metrics.R", "src/train.R"],
        script="code/04-network_properties_bma.R",
    output:
        expand(
            "steps/network_properties_bma/{graph}_{n}.Rds",
            graph=["random", "cluster"],
            n=range(100, 105),
        ),
        touch("steps/network_properties_bma/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule network_properties_stability:
    input:
        infiles=["src/network_metrics.R", "src/train.R"],
        script="code/05-network_properties_stability.R",
    output:
        expand(
            "steps/network_properties_stability/{graph}_{n}.Rds",
            graph=["random", "cluster"],
            n=range(100, 105),
        ),
        touch("steps/network_properties_stability/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule fig_precision_recall:
    input:
        input=["steps/fits/.SUCCESS", "figures/theme.R"],
        script="figures/01-precision-recall/plot.R",
    output:
        "figures/01-precision-recall/f1-normal.pdf",
        "figures/01-precision-recall/f1-counts.pdf",
        "figures/01-precision-recall/prec_recall-normal.pdf",
        "figures/01-precision-recall/prec_recall-counts.pdf",
        touch("figures/01-precision-recall/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule fig_regression:
    input:
        input=["steps/fits/.SUCCESS", "figures/theme.R", "src/network_metrics.R"],
        script=expand(
            "figures/02-regression/{script}.R",
            script=[
                "modularity",
                "distances",
                "adjusted_mutual_information",
                "hub_score",
            ],
        ),
    output:
        "figures/02-regression/modularity_boxplot.pdf",
        "figures/02-regression/distances_spearman.pdf",
        "figures/02-regression/hub_score.pdf",
        "figures/02-regression/cluster_ami.pdf",
        touch("figures/02-regression/.SUCCESS"),
    run:
        for script in input.script:
            shell("Rscript --vanilla {script}")


rule fig_confidence_credibility_intervals:
    input:
        input=["steps/credibility_confidence_intervals/.SUCCESS", "figures/theme.R"],
        script="figures/03-credibility-confidence/plots.R",
    output:
        "figures/03-credibility-confidence/cluster_ami.pdf",
        "figures/03-credibility-confidence/hub_score.pdf",
        "figures/03-credibility-confidence/modularity.pdf",
        "figures/03-credibility-confidence/distances_spearman.pdf",
        touch("figures/03-credibility-confidence/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"


rule fig_network_properties_bma:
    input:
        input=["steps/network_properties_bma/.SUCCESS", "figures/theme.R"],
        script="figures/04-network_properties_bma/plots.R",
    output:
        "figures/04-network_properties_bma/cluster_ami.pdf",
        "figures/04-network_properties_bma/hub_score.pdf",
        "figures/04-network_properties_bma/modularity.pdf",
        "figures/04-network_properties_bma/distances_spearman.pdf",
        touch("figures/04-network_properties_bma/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"

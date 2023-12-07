rule all:
    input:
        expand(
            "steps/{dir2}/.SUCCESS",
            dir2=["fits", "random_errors"],
        ),
        expand(
            "figures/{dir2}/.SUCCESS",
            dir2=["01-f1_scores"],
        ),


rule fit_simulated_data:
    input:
        "code/01-fit_simulated_data.R",
    threads: 8
    output:
        expand(
            "steps/fits/{graph}_{n}.csv",
            graph=["hub", "cluster"],
            n=range(600, 611),
        ),
        expand(
            "steps/fits/random_{n}.png",
            n=range(600, 631),
        ),
        touch("steps/fits/.SUCCESS"),
    shell:
        "Rscript --vanilla {input}"


rule random_errors_network_properties:
    input:
        "code/02-random_errors_network_properties.R",
    threads: 8
    output:
        expand(
            "steps/random_errors/{graph}_{n}.csv",
            graph=["random", "hub", "cluster"],
            n=range(100, 116),
        ),
        touch("steps/random_errors/.SUCCESS"),
    shell:
        "Rscript --vanilla {input}"


rule fig_precision_recall:
    input:
        input=["steps/fits/.SUCCESS", "figures/theme.R"],
        script="figures/01-precision-recall/plot.R",
    output:
        "figures/01-f1_scores/f1-normal.pdf",
        "figures/01-f1_scores/f1-counts.pdf",
        "figures/01-f1_scores/prec_recall-normal.pdf",
        "figures/01-f1_scores/prec_recall-counts.pdf",
        touch("figures/01-f1_scores/.SUCCESS"),
    shell:
        "Rscript --vanilla {input.script}"

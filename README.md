
# The Strong Screening Rule for SLOPE

This repository contains the code necessary to reproduce the results in
the NIPS paper The Strong Screening Rule for SLOPE.

## renv

The code in this repository uses the [renv](https://CRAN.R-project.org/package=renv)
package to maintain reproducibility. To install and active the renv repository
for this project, please run [`prepare_simulations.R`](prepare_simulations.R).

## Figures and Tables

Run [`figures.R`](figures.R) and [`tables.R`](tables.R) to reproduce the figures and table 
from the paper. The [figures folder](/figures) already, however,
contains the figures used in the paper.

## Simulations

Run one of the `sim_*.R` files to reproduce
a given simulation. The results from the simulations will be saved to the
[results folder](/results) in this repository. This folder is already populated with the 
results from the paper.

## Data

The [data-raw folder](/data-raw) contains the code required to obtain the data files used
in the experiments. These data-sets are, for the most part, however, already
present in the [data folder](/data) in this repository.


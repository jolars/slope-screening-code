
# The Strong Screening Rule for SLOPE

This repository contains the code necessary to reproduce the results in
the NIPS paper The Strong Screening Rule for SLOPE.

## renv

The code in this repository uses the [renv](https://CRAN.R-project.org/package=renv)
package to maintain reproducibility. Please install the package and run

```r
renv::activate()
```

before attempting to run any of the scripts in this repository.

## Figures and Tables

Run the code`figures.R` and `tables.R` to reproduce the figures and table from
the paper. The </figures> folder contains the figures used in the paper.

## Simulations

To reproduce the simulations, first run <prepare_simulations.R> to source all the
necessary bits of code. Then run one of the `sim_*.R` files to reproduce
a given simulation. The results from the simulations will be output to the
</results> folder in this repository. This folder is already populated with the results 
from th e

## Data

The </data-raw> folder contains the code required to obtain the data files used
in the experiments. These data-sets are, for the most part, however, already
present in the </data> folder of this repository.


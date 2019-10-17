# pineplot

The **pineplot** package can be used to generate pine plots, which are stacked triangular heat maps that are generated using the visualization capabilities of **ggplot2** and rotation from **grid**. Visualizing symmetric matrices in this way has the following benefits:

- Removes redundancy by removing half of the symmetric heat map
- Allows for visualizing more heatmaps in the same page, as it saves space
- Increases the margin space to allow for more annotation around axes
- Since each heat map is a **ggplot2** object, the heatmaps can be easily manipulated after creation before construction of a pine plot

The paper will be available soon.

<!-- ![pineplot](vignettes/images/example.jpg) -->

## Getting Started

The R package **devtools** can be used to install this package. If you do not have **devtools**, it can be installed and loaded using the following commands.
```
install.packages("devtools")
library(devtools)
```
Next, install **pineplot** and load the package.

```
install_github("klovens/pineplot", build_vignettes =TRUE)
library(pineplot)
```
Examples and code demonstrating some of the different uses for **pineplot** are available in the vignettes.

```
vignette("pineplot")
vignette("cleveland_heart_disease")
vignette("liver")
```

### Prerequisites
This package requires that ggplot2 (>= 3.0.0), grid, gridExtra (>= 2.3), reshape2 (>= 1.4.3), and gtable are installed. We also suggest knitr, magick, rmarkdown, and testthat (>= 2.1.0) in order to run all examples and tests.

## Authors

* [**Katie Ovens**](https://github.com/klovens) - *Initial work* 
* [**Daniel Hogan**](https://github.com/djhogan) - *Initial work* 
* [**Farhad Maleki**](https://github.com/FarhadMaleki) - *Initial work*

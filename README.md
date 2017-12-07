# Description

This is a collection of utilities that I find useful. Among other things:

- Loading the package provides access to `lmSummary()`, which is a version of `summary.lm()` but provides F statistics and $R^2_p$ for each term in an `lm` object.
- Provides the `loadStuff()` function, which takes a character vector of packages. If `require(pkg)` returns `FALSE` then the package is installed.
- Includes a set of functions for transforming between probabilities, odds, and logits.
- Includes the `makePdf()` function which makes sending graphics output to a pdf device much quicker.
- Includes a version of the vioplot function that removes missing values by default, adds dashed lines representing means, and other goodies.

# Installation

1. If devtools is not installed, install it: 

```
install.packages('devtools'); library(devtools)
```

2. Install this package using the devtools github installation tool:

```
install_github('Cmell/CMUtils')
```
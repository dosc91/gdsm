# gdsm <img src='https://dominicschmitz.com/wp-content/uploads/2022/03/Zeichenflaeche-1-1.png' align="right" height="138" />

<!-- badges: start -->
![](https://img.shields.io/badge/version-0.2-FFA70B.svg)
![](https://img.shields.io/github/last-commit/dosc91/gdsm)
<!-- badges: end -->

`gdsm` is a package created for the ["Grundlagen der Distributionellen Semantik"](https://div-ling.org/de/veranstaltungen/grundlagen-der-distributionellen-semantik/) workshop. 

It includes some handy functions and data sets, but most importantly installs all necessary packages and contains exercise files.

Check out the [references](https://dosc91.github.io/gdsm/reference/index.html) for more detailed information.


# How to Install

The preferred way to install this package is through devtools:

```r
# if devtools has not been installed yet, please install it first
# install.packages("devtools")

# then, install the gdsm package
devtools::install_github("dosc91/gdsm", dependencies=TRUE)
```

You will be asked to update dependencies during installation. Updates can usually be skipped. However, please note that if for one or more packages an `NA` is given, these packages need to be installed.

# References

Please cite the package as follows:

Schmitz, D., & Schneider, V. (2022). gdsm: General functions for Distributional SeMantics. R package version 0.2. URL: https://github.com/dosc91/gdsm




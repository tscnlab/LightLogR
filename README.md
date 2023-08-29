
<!-- README.md is generated from README.Rmd. Please edit that file -->

> **Please note that LightLogR is work in progress! If you are
> interested in the project and want to know more, please give us a
> [message](mailto:johannes.zauner@tum.de).**

# LightLogR <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tscnlab/LightLogR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tscnlab/LightLogR/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

Personalized light exposure data is progressively gaining importance in
various sectors, including research, occupational affairs, and fitness
tracking. Data are collected through a proliferating selection of
wearable loggers and dosimeters, varying in size, shape, functionality,
and output format. Despite or maybe because of numerous use cases, the
field lacks a unified framework for collecting, validating, and
analyzing the accumulated data. This issue increases the time and
expertise necessary to handle such data and also compromises the
FAIRness (Findability, Accessibility, Interoperability, Reusability) of
the results, especially in meta-analyses.

**LightLogR** is a package under development as part of the
[*MeLiDos*](https://www.melidos.eu) project to address these issues.
MeLiDos is a joint, [EURAMET](https://www.euramet.org)-funded project
involving sixteen partners across Europe, aimed at developing a
metrology and a standard workflow for wearable light logger data and
optical radiation dosimeters. Its primary contributions towards
fostering FAIR data include the development of a common file format,
robust metadata descriptors, and an accompanying open-source software
ecosystem. **LightLogR** aims to provide tools for:

- Generation of data and metadata files

- Conversion of popular file formats

- Validation of light logging data

- Verification of crucial metadata

- Calculation of common parameters

- Semi-automated analysis and visualization (both command-line and
  GUI-based)

- Integration of data into a unified database for cross-study analyses

**LightLogR** is developed by the [*Translational Sensory & Circadian
Neuroscience*](https://www.tscnlab.org) lab, a joint group from the
[Technical University of Munich](https://www.tum.de/en/) and the [Max
Planck Institute for Biological
Cybernetics](https://www.mpg.de/152075/biological-cybernetics).

## Installation

You can install the development version of LightLogR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tscnlab/LightLogR")
```

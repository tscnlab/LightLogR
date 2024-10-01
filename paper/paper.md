---
title: 'LightLogR: Reproducible analysis of personal light exposure data'
tags:
- R
- light
- light exposure
- luminous exposure
- personal light exposure
- melanopic
- ipRGC
- time series analysis
- wearable devices
- wearable sensors
- wearables
- "rstats-package"
date: "01 October 2024"
output: pdf_document
authors:
- name: Johannes Zauner
  orcid: "0000-0003-2171-4566"
  corresponding: true
  affiliation: 1, 2
- name: Steffen Hartmeyer
  orcid: "0000-0002-2813-2668"
  affiliation: 3
- name: Manuel Spitschan
  orcid: "0000-0002-8572-9268"
  affiliation: 1, 2, 4, 5
bibliography: paper.bib
editor_options:
  markdown:
    wrap: 72
affiliations:
- name: TUM School of Medicine and Health, Department Health and Sports Sciences,
    Chronobiology & Health, Technical University of Munich, Munich, Germany
  index: 1
  ror: 02kkvpp62
- name: Max Planck Institute for Biological Cybernetics, Max Planck Research Group
    Translational Sensory & Circadian Neuroscience, Tübingen, Germany
  ror: 026nmvv73
  index: 2
- name: Laboratory of Integrated Performance in Design, École Polytechnique Fédérale
    de Lausanne, Lausanne, Switzerland
  ror: 02s376052
  index: 3
- name: "TUM Institute for Advanced Study (TUM-IAS), Technical University of Munich,
    Munich, Germany"
  index: 4
- name: TUMCREATE Ltd., Singapore, Singapore
  index: 5
---

# Summary

The effects of light on human health and well-being are best studied
with real-world and personal light exposure, measured through wearable
devices. More research groups incorporate these kinds of data in their
studies, and important connections between light and health outcomes are
drawn and their relevance gauged. Yet with few or missing standards,
guidelines, and frameworks, setting up measurements, analysing the data,
and comparing outcomes between studies is challenging, especially
considering the significantly more complex time series data from
wearables than single, spot measurements in past laboratory studies. In
this paper, we introduce one building block to facilitate these efforts
in the form of an open-source, permissively licenced software package
for R statistical software: `LightLogR`. As part of a developing
software ecosystem, `LightLogR` is built with the challenges of current
and future datasets in mind. The package standardizes many tasks when
importing and processing personal light exposure data, provides deep and
quick insights into the datasets through summary and visualization
tools, and incorporates all major metrics used in the relevant
literature, all while embracing the inherently hierarchical,
participant-based data structure.

# Statement of need

Personalized luminous exposure data is progressively gaining importance
in various sectors, including research, occupational affairs, and
fitness tracking. Data are collected through a proliferating selection
of wearable loggers and dosimeters, varying in size, shape,
functionality, and output format. Despite or maybe because of numerous
use cases, the field lacks a unified framework for collecting,
validating, and analyzing the accumulated data. This issue increases the
time and expertise necessary to handle such data and also compromises
the FAIRness (Findability, Accessibility, Interoperability, Reusability)
of the results, especially in meta-analyses.

`LightLogR` was designed to be used by researchers who deal with
personal light exposure data collected from wearable devices. These data
are of interest for various disciplines, including epidemiology,
chronobiology, sleep research, and even lighting design. The package is
intended to streamline the process of importing, processing, and
analysing these data in a reproducible and transparent manner. Key
features include:

-   a growing list of supported devices with pre-defined import
    functions tailored to their data structure (17 at the time of
    writing, see \autoref{tab:one})

-   preprocessing functions to combine different time series, aggregate
    and filter data, and find and deal with implicitly missing data

-   visualization functions to quickly explore the data. These function
    are based on the popular `ggplot2` [@ggplot2] plotting package
    and are designed to be easily customizable to construct
    publication-ready figures (see, e.g., \autoref{fig:one}).

-   a large and growing set of metrics that cover most if not all major
    approaches found in the literature (at the time of writing 61
    metrics across 17 metric families, see \autoref{tab:two})),
    accessible via a consistent function interface.

![Light logger data can powerfully convey insights into personal light
exposure and health-related outcomes. `LightLogR` facilitates the import
and combination of different data sources into one coherent data
structure, as seen here by combining environmental daylight availability
and personal light exposure with data from a sleep diary. The
visualization functions in the package further allow tweaking to produce
publication-ready results.
\label{fig:one}](Day.png){width="80%"}

| Device Name | Manufacturer |
|----|----|
| Actiwatch Spectrum | Philips Respironics |
| ActLumus | Condor Instruments |
| ActTrust | Condor Instruments |
| melanopiQ Circadian Eye (Prototype) | Max-Planck-Institute for Biological Cybernetics |
| DeLux | Intelligent Automation Inc |
| GENEActiv (with GGIR preprocessing) | Activeinsights |
| Kronowise | Kronohealth |
| Lido | University of Lucerne |
| LightWatcher | Object-Tracker |
| LIMO | ENTPE |
| LYS Button | LYS Technologies |
| Motion Watch 8 | CamNtech |
| XL-500 BLE | NanoLambda |
| OcuWEAR | Ocutune |
| Speccy | Monash University |
| SpectraWear | University of Manchester |
| VEET | Meta Reality Labs |

: devices supported in version 0.4.1 \label{tab:one}

| Metric Family                        | Submetrics | Note                 | Documentation                                                                                     |
|------------------|----------------|-----------------|---------------------|
| Barroso                              | 7                 |                      | `barroso_lighting_metrics()`                                                                      |
| Bright-dark period                   | 4x2               | bright / dark        | `bright_dark_period()`                                                                            |
| Centroid of light exposure           | 1                 |                      | `centroidLE()`                                                                                    |
| Disparity index                      | 1                 |                      | `disparity_index()`                                                                               |
| Duration above threshold             | 3                 | above, below, within | `duration_above_threshold()`                                                                      |
| Exponential moving average (EMA)     | 1                 |                      | `exponential_moving_average()`                                                                    |
| Frequency crossing threshold         | 1                 |                      | `frequency_crossing_threshold()`                                                                  |
| Intradaily Variance (IV)             | 1                 |                      | `intradaily_variability()`                                                                        |
| Interdaily Stability (IS)            | 1                 |                      | `interdaily_stability()`                                                                          |
| Midpoint CE (Cumulative Exposure)    | 1                 |                      | `midpointCE()`                                                                                    |
| nvRC (Non-visual circadian response) | 4                 |                      | `nvRC()`, `nvRC_circadianDisturbance()`, `nvRC_circadianBias()`, `nvRC_relativeAmplitudeError()` |
| nvRD (Non-visual direct response)    | 2                 |                      | `nvRD()`, `nvRD_cumulative_response()`                                                           |
| Period above threshold               | 3                 | above, below, within | `period_above_threshold()`                                                                        |
| Pulses above threshold               | 7x3               | above, below, within | `pulses_above_threshold()`                                                                        |
| Threshold for duration               | 2                 | above, below         | `threshold_for_duration()`                                                                        |
| Timing above threshold               | 3                 | above, below, within | `timing_above_threshold()`                                                                        |
| **Total:**                           |                   |                      |                                                                                                   |
| **17 families**                      | **61 metrics**    |                      |                                                                                                   |

: metrics available in version 0.4.1 \label{tab:two}

LightLogR is already being used in several research projects and
scientific publications across the scientific community, such as:

-   cohort study to collect light exposure data across different
    geolocations [@Guidolin2024]
-   cohort study to collect year-long datasets of various types of
    environmental and behavioral data [@biller2024]
-   power analysis method for personal light exposure
    [@zauner2023power],
-   intervention study on the effects of light on bipolar disorder (data
    collection in progress),
-   intervention study on sex and seasonal changes in human melatonin
    suppression and alerting response to moderate light (publication in
    progress),
-   intervention study on exposure to bright light during afternoon to
    early evening on later evening melatonin release in adolescents
    (**note: Preprint this week, Rafael Lazar will send DOI**),
-   observational study on the wearing compliance of personal light
    exposure [@stefani2024],
-   observational study on the differences in light exposure and light
    exposure related behavior between Malaysia and Switzerland
    (preregistration in progress).

# Funding Statement

The develoment of `LightLogR` is funded through MeLiDos, a joint,
EURAMET-funded project involving sixteen partners across Europe, aimed
at developing a metrology and a standard workflow for wearable light
logger data and optical radiation dosimeters. Its primary contributions
towards fostering FAIR data include the development of a common file
format, robust metadata descriptors, and an accompanying open-source
software ecosystem.

The project (22NRM05 MeLiDos) [@SPITSCHAN2024114909] has received
funding from the European Partnership on Metrology, co-financed from the
European Union’s Horizon Europe Research and Innovation Programme and by
the Participating States. Views and opinions expressed are however those
of the author(s) only and do not necessarily reflect those of the
European Union or EURAMET. Neither the European Union nor the granting
authority can be held responsible for them.

# Acknowledgements

We thank Carolina Guidolin and Anna Biller from the TSCN unit for
testing the software during development and providing feature ideas.

# References
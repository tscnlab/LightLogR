---
title: 'LightLogR: Reproducible analysis of personal light exposure data'
tags:
- R
- light
- light exposure
- luminous exposure
- personal light exposure
- melanopsin
- ipRGCs (intrinsic photosensitive retinal ganglion cells)
- circadian rhythm
- chronobiology
- photoperiod
- occupational exposure
- time series analysis
- wearable devices
- wearable sensors
- wearables
- "rstats-package"
date: "07 October 2024"
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
- name: Technical University of Munich, TUM School of Medicine and Health, Department Health and Sports Sciences, Chronobiology & Health, Munich, Germany
  index: 1
  ror: 02kkvpp62
- name: Max Planck Institute for Biological Cybernetics, Max Planck Research Group Translational Sensory & Circadian Neuroscience, Tübingen, Germany
  ror: 026nmvv73
  index: 2
- name: École Polytechnique Fédérale de Lausanne (EPFL), School of Architecture, Civil and Environmental Engineering (ENAC), Laboratory of Integrated Performance in Design (LIPID), Lausanne, Switzerland
  ror: 02s376052
  index: 3
- name: TUM Institute for Advanced Study (TUM-IAS), Technical University of Munich, Garching, Germany
  index: 4
- name: TUMCREATE Ltd., Singapore, Singapore
  index: 5
---

# Summary

Light plays an important role in human health and well-being, which necessitates the study of the effects of personal light exposure in real-world settings, measured by means of wearable devices. A growing number of studies incorporate these kinds of data to assess associations between light and health outcomes. Yet with few or missing standards, guidelines, and frameworks, setting up measurements, analysing the data, and comparing outcomes between studies is challenging, especially considering the significantly more complex time series data from wearable light loggers compared to controlled stimuli used in laboratory studies. In this paper, we introduce `LightLogR`, a novel resource to facilitate these research efforts in the form of an open-source, GPL-3.0-licenced software package for the statistical software R. As part of a developing software ecosystem, `LightLogR` is built with common challenges of current and future datasets in mind. The package standardizes many tasks for importing and processing personal light exposure data, provides quick as well as detailed insights into the datasets through summary and visualization tools, and incorporates major metrics commonly used in the field (61 metrics across 17 metric families), while embracing an inherently hierarchical, participant-based data structure.

![LightLogR logo \label{fig:one}](logo.png){width="25%"}

# Statement of need

Personalized luminous exposure data is progressively gaining importance across various domains, including research, occupational affairs, and lifestyle tracking. Data are collected through a proliferating selection of wearable light loggers and dosimeters, varying in size, shape, functionality, and output format [@hartmeyer2023]. Despite or potentially because of numerous use cases, the field still lacks a unified framework for collecting, validating, and analyzing the accumulated data [@hartmeyer2023; @spitschan2022].  This issue increases the time and expertise necessary to handle such data and also compromises the FAIRness (findability, accessibility, interoperability, reusability) [@wilkinson2016] of the results, especially for meta-analyses [@devries2024].

`LightLogR` (\autoref{fig:one}) was designed to be used by researchers who deal with personal light exposure data collected from wearable devices. These data are of interest for various disciplines, including chronobiology, sleep research, vision science and epidemiology, as well as for post-occupancy evaluations in  architecture and lighting design. The package is intended to streamline the process of importing,  processing, and analysing these data in a reproducible and transparent manner. The package is available on GitHub (@repo) and CRAN (@cran), has a dedicated website for documentation and tutorials (@documentation), and releases are archived on Zenodo (@zenodo).

`LightLogR`'s key features include:

-   a growing list of supported devices with pre-defined import functions tailored to their data structure (17 at the time of writing, see \autoref{tab:one}),

-   preprocessing functions to combine different time series, aggregate and filter data, and find and deal with implicitly missing data,

-   visualization functions to quickly explore the data. These function are based on the popular `ggplot2` [@wickham2016] plotting package and are designed to be easily customizable to construct publication-ready figures (see, \autoref{fig:two}),

-   a large and growing set of metrics that cover most if not all major approaches found in the literature (at the time of writing 61  metrics across 17 metric families, see \autoref{tab:two})), accessible via a consistent function interface.

![Light logger data can powerfully convey insights into personal light exposure and health-related outcomes. `LightLogR` facilitates the import and combination of different data sources into a coherent data structure, as seen here by combining environmental daylight availability and personal light exposure with data from a sleep diary. The visualization functions in the package further allow customization to produce publication-ready figures. This figure was created with the 'gg_day()' function. The creation process is part of a tutorial [@tutorial] on several key functions in the package. \label{fig:two}](Day.png){width="80%"}

| Device Name | Manufacturer |
|----|----|
| Actiwatch Spectrum | Philips Respironics |
| ActLumus | Condor Instruments |
| ActTrust | Condor Instruments |
| DeLux | Intelligent Automation Inc. |
| GENEActiv[^1] | Activeinsights |
| Kronowise | Kronohealth |
| Lido | Lucerne University of Applied Sciences and Arts |
| LightWatcher | Object-Tracker |
| LIMO | École nationale des travaux publics de l'État (ENTPE) |
| LYS Button | LYS Technologies |
| Motion Watch 8 | CamNtech |
| melanopiQ Circadian Eye | Max Planck Institute for Biological Cybernetics |
| XL-500 BLE | NanoLambda |
| OcuWEAR | Ocutune |
| Speccy | Monash University Malaysia |
| SpectraWear | University of Manchester |
| VEET | Meta Reality Labs |

[^1]: Available after processing of the data using `GGIR` [@migueles2019].

: Devices supported for import in version 0.4.1 \label{tab:one}

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
| nvRC (non-visual circadian response) | 4                 |                      | `nvRC()`, `nvRC_circadianDisturbance()`, `nvRC_circadianBias()`, `nvRC_relativeAmplitudeError()` |
| nvRD (non-visual direct response)    | 2                 |                      | `nvRD()`, `nvRD_cumulative_response()`                                                           |
| Period above threshold               | 3                 | above, below, within | `period_above_threshold()`                                                                        |
| Pulses above threshold               | 7x3               | above, below, within | `pulses_above_threshold()`                                                                        |
| Threshold for duration               | 2                 | above, below         | `threshold_for_duration()`                                                                        |
| Timing above threshold (TAT)         | 3                 | above, below, within | `timing_above_threshold()`                                                                        |
| **Total:**                           |                   |                      |                                                                                                   |
| **17 families**                      | **61 metrics**    |                      |                                                                                                   |

: metrics available in version 0.4.1 \label{tab:two}

LightLogR is already being used in several research projects across scientific domains, including:

-   an ongoing cohort study to collect light exposure data across different geolocations [@guidolin2024],
-   an ongoing cohort study to collect year-long datasets of various types of environmental and behavioral data [@biller2024],
-   a novel power analysis method for personal light exposure data [@zauner2023],
-   an intervention study on the effects of light on bipolar disorder [@roguski2024],
-   an intervention study on exposure to bright light during afternoon to early evening on later evening melatonin release in adolescents [@lazar2024],
-   an observational study on the wearing compliance of personal light exposure [@stefani2024],
-   an observational study on the differences in light exposure and light exposure related behavior between Malaysia and Switzerland  (preregistration in progress),
-   an intervention study on sex and seasonal changes in human melatonin suppression and alerting response to moderate light (publication in progress),
-   an observational study on light exposure, sleep, and circadian rhythms in hospital shift workers (publication in progress).

# Funding Statement

The development of `LightLogR` is funded by MeLiDos, a joint, EURAMET-funded project involving sixteen partners across Europe, aimed at developing a metrology and a standard workflow for wearable light logger data and optical radiation dosimeters [@spitschan2024]. Its primary contributions towards fostering FAIR data include the development of a common file format, robust metadata descriptors, and an accompanying open-source software ecosystem.

The project (22NRM05 MeLiDos) has received funding from the European Partnership on Metrology, co-financed from the European Union’s Horizon Europe Research and Innovation Programme and by the Participating States. Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or EURAMET. Neither the European Union nor the granting authority can be held responsible for them.

# Acknowledgements

We thank Carolina Guidolin (Max Planck Institute for Biological Cybernetics) and Dr. Anna Magdalena Biller (Technical University of Munich) for testing the software during development and providing feature ideas, and the entire Translational Sensory & Circadian Neuroscience Unit (MPS/TUM/TUMCREATE) for its support.

# References

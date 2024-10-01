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
  - rstats-package

authors:
  - name: Johannes Zauner
    orcid: 0000-0003-2171-4566
    corresponding: true
    affiliation: "1, 2"
  - name: Steffen Hartmeyer
    orcid: 0000-0002-2813-2668
    affiliation: 3
  - name: Manuel Spitschan
    orcid: 0000-0002-8572-9268
    affiliation: "1, 2, 4, 5"
affiliations:
 - name: TUM School of Medicine and Health, Department Health and Sports Sciences, Chronobiology & Health, Technical University of Munich, Munich, Germany
   index: 1
   ror: 02kkvpp62
 - name: Max Planck Institute for Biological Cybernetics, Max Planck Research Group Translational Sensory & Circadian Neuroscience, Tübingen, Germany
   ror: 026nmvv73
   index: 2
 - name: Laboratory of Integrated Performance in Design, École Polytechnique Fédérale de Lausanne, Lausanne, Switzerland
   ror: 02s376052
   index: 3
 - name: TUM Institute for Advanced Study (TUM-IAS), Technical University of Munich, Munich, Germany
   index: 4
 - name: TUMCREATE Ltd., Singapore, Singapore
   index: 5
date: 01 October 2024
bibliography: paper.bib

---

# Summary

The forces on stars, galaxies, and dark matter under external gravitational
fields lead to the dynamical evolution of structures in the universe. The orbits
of these bodies are therefore key to understanding the formation, history, and
future state of galaxies. The field of "galactic dynamics," which aims to model
the gravitating components of galaxies to study their structure and evolution,
is now well-established, commonly taught, and frequently used in astronomy.
Aside from toy problems and demonstrations, the majority of problems require
efficient numerical tools, many of which require the same base code (e.g., for
performing numerical orbit integration).

# Statement of need

`Gala` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package [@astropy] (`astropy.units` and
`astropy.coordinates`).

`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
[@gaia] by students and experts alike.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements

We thank Carolina Guidolin and Anna Biller from the TSCN unit for testing the software during development and providing feature ideas.

# References
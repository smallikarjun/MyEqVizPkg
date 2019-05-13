# MyEqVizPkg
Visualize NOAA earthquake data

# Travis Build Status: 
[![Travis build status](https://travis-ci.org/smallikarjun/MyEqVizPkg.svg?branch=master)](https://travis-ci.org/smallikarjun/MyEqVizPkg)

The goal of MyEqVizPkg R Package is to simplify the workflow while exploring the NOAA significant earthquakes dataset.

More specificaly, this package implements:

- Two function for data cleaning: eq_clean_data and eq_location_clean
- A new ggplot2 geom for plotting time serie data: geom_timeline
- A new ggplot2 geom that adds labels to geom_timeline : geom_timelinelabel
- A function that maps the epicenters (LATITUDE/LONGITUDE) and annotates each point in the dataset: eq_map
- A function that creates an HTML label that can be used as the annotation text in the leaflet map: eq_create_label

# Installation
You can install this package from github using the following command:

devtools::install_github('smallikarjun/MyEqVizPkg', build_vignettes = TRUE)

library(MyEqVizPkg)

# Documentation
A detailled documentation can be found in the package vignette.

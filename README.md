# CORDEX NetCDF Time-Series Analysis

A web-based **R-Shiny application** for extracting point time series from CORDEX climate model NetCDF datasets.


## Live App

Try it online here:  
[Launch CORDEX NetCDF Time-Series Analysis](https://waterhy-shiny.shinyapps.io/cordex_shiny/)

## Features

- Interactive map-based point selection
- Bidirectional coordinate input
- Nearest neighbor extraction
- Inverse Distance Weighting (IDW) interpolation (4 nearest neighbors, power = 2)
- Multi-file comparison
- Metadata display
- CSV export of extracted time series

## Methods

- **Nearest Neighbor**: selects the grid cell minimizing squared Euclidean distance.  
- **Inverse Distance Weighting (IDW)**: uses the four closest grid cells weighted by inverse squared distance (power = 2). Distances are computed in geographic coordinate space.

## Intended Use

Designed for interactive extraction and analysis of point time series from CORDEX NetCDF datasets on rotated grids.

## Citation

If you use this app, please cite it as:  

Lyra, A. (2026). *CORDEX NetCDF Time-Series Analysis*. Zenodo. [https://doi.org/10.5281/zenodo.18684890](https://doi.org/10.5281/zenodo.18684890)

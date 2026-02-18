# CORDEX NetCDF Time-Series Extractor

A web-based Shiny application for extracting point time series from CORDEX climate model NetCDF datasets.

## Features

- Interactive map-based point selection
- Bidirectional coordinate input
- Nearest neighbor extraction
- Inverse Distance Weighting (IDW) interpolation (4 nearest neighbors, power = 2)
- Multi-file comparison
- Metadata display
- CSV export of extracted time series

## Methods

Nearest Neighbor:
Selects the grid cell minimizing squared Euclidean distance.

Inverse Distance Weighting (IDW):
Uses the four closest grid cells weighted by inverse squared distance (power = 2).

Distances are computed in geographic coordinate space.

## Intended Use

Designed for CORDEX regional climate simulations on rotated grids.

## Author

Aikaterini Lyra  
Laboratory of Hydrology and Aquatic Systems Analysis, Department of Civil Engineering, School of Engineering, University of Thessaly, 38334 Volos, Greece  
klyra@uth.gr

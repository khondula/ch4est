[![DOI](https://zenodo.org/badge/277689447.svg)](https://zenodo.org/badge/latestdoi/277689447)

Code and data for forested wetlands methane model used in: *Effects of Using High Resolution Satellite-based Inundation Time Series to Estimate Methane Fluxes from Forested Wetlands* by Kelly Hondula, Ben DeVries, Nate Jones, Margaret Palmer

## Guide to files 

* `00-planet-download.Rmd` - used to download images, UDM, and metadata from Planet API using functions `R/request_%%%_url.R`and table of image IDs in `data/PlanetScope-images-inventory`
* `01-process-images.Rmd` - used to apply UDM mask and calculate additional bands with colorspace
* `02-prep-training.Rmd` - creating water and non-water training classes from NWI, NLCD, topo depressions
* `03-run-models-RF.Rmd` - train and apply models using function in `R/run_rf_model-v7.R`, save classified raster and extract values for each polygon
* `04-make-inundation-time-series.Rmd` - used to create daily time series from classified pixels
* `05-rf-vs-field.Rmd` - used to compare model predictions and field based time series
* `ch4-model-%%.R` - methane models applied to inundation time series 

## Packages and data sources

* [Vanderhoof and Lang 2017](https://doi.org/10.5066/F70C4T8F) topographic depressions
* NLCD 2016 land cover
* National Wetlands Inventory polygons
* Greensboro watershed boundary
* GSOD weather data
* Holgerson and Raymond 2016 Methane data
* tidyverse packages
* colorspace for calculating color metrics
* raster, sf, velox, fasterize, leaflet, stars for spatial data
* h2o, ranger, caret, rsample for modeling
* zoo rollapply for time series rolling median
* LakeMetabolizer for parameters in methane models


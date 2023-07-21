# Phenology of _P. virginiensis_

## Summary

This repository includes code for investigations of variation of flight times 
in the West Virginia White (_Pieris virginiensis_) and associated host plant 
species over time. The work relies on data hosted by the Global Biodiversity 
Information Facility ([GBIF](https://gbif.org)) and 
[Daymet](https://daymet.ornl.gov).

## Dependencies

The work uses the following third-party R packages:

+ broom
+ dplyr
+ ggplot2
+ ggpubr
+ ggridges
+ ks
+ lmtest
+ lubridate
+ terra
+ tidyr

## Structure

+ archive: early versions of scripts for exploratory data analysis; preserved 
for now, but not being maintained
+ data: observational data for _P. virginiensis_ and associated host plant 
species and climate data (specifically temperature data)
+ functions: includes functions repeatedly used across different scripts; 
currently just the one function for downloading data from GBIF.
+ output: graphical and tabular output; most files destined for the output 
folder are not under version control
+ scripts: R scripts for data analysis and visualization
  + gdd-archive: Folder containing early scripts that used growing degree days 
  as a predictor variable.
  + analysis-linear-model-tmid.R: Linear regression analysis for yearly change 
  in Julian day of observations, includes plotting statistical model results; 
  includes midpoint temperature as possible predictor
  + analysis-polynomial-model. R: DEPRECATED Host plant polynomial model
  + data-01-download.R: Download data from gbif and do QA/QC as necessary
  + data-02-prepare.R: Prepare data for analyses and plotting; includes time 
  filtering (2000-2023) and geographic restriction to _P. virginiensis_' 
  approximate range
  + data-03-weather.R: Download temperature data (tmin and tmax) for each 
  observation produced by data-02-prepare.R and calculate midpoint temperature
  (tmid)
  + figure-obs.R: Figure with insect and host plant observations; restricted to
  two _Cardamine_ hosts
  + figure-tmid-ridges.R: Ridge plots of observations for midpoint temperature
  bins
+ templates: DEPRECATED RMarkdown templates that were part of early exploratory 
data analysis

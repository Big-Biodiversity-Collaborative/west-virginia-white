# Phenology of _P. virginiensis_

## Summary

This repository includes code for investigations of variation of flight times 
in the West Virginia White (_Pieris virginiensis_) over time. The work relies 
on data hosted by the Global Biodiversity Information Facility 
([GBIF](https://gbif.org)).

## Dependencies

The work uses the following third-party R packages:

+ broom
+ dplyr
+ ggplot2
+ ggpubr
+ ks
+ lmtest
+ lubridate
+ terra
+ tidyr

## Structure

+ archive: early versions of scripts for exploratory data analysis; preserved 
for now, but not being maintained
+ data: observational data for _P. virginiensis_ and associated host plant 
species and climate data (specifically growing degree days data)
+ functions: includes functions repeatedly used across different scripts; 
currently just the one function for downloading data from GBIF.
+ output: graphical and tabular output; most files destined for the output 
folder are not under version control
+ scripts: R scripts for data analysis and visualization
  + analysis-linear-model.R: Linear regression analysis for yearly change in
  julian day of observations
  + analysis-polynomial-model. R: DEPRECATED Host plant polynomial model
  + data-download.R: Download data from gbif and do QA/QC as necessary
  + data-prepare.R: Prepare data for analyses and plotting; includes time 
  filtering (2000-2020) and geographic restriction to _P. virginiensis_' range
  + figure-gdd-diff.R: DEPRECATED Figure showing change in growing degree days
  between 1961-1990 and 1990-2020
  + figure-gdd-obs.R: Figure with growing degree days, insect and host plant
  observations
  + figure-gdd-ridges.R: Ridge plots of observations for growing degree day 
  bins
  + figure-gdd-vs-gdd-diff.R: Plot of gdd and delta gdd for observational data
  + figure-range-maps.R: Approximate range maps for insect and hosts
+ templates: DEPRECATED RMarkdown templates that were part of early exploratory 
data analysis
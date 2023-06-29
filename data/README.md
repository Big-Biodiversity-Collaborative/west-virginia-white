# README for data folder

## Observational data

### Download and initial cleaning

Observations downloaded from Global Biodiversity Information Facility (GBIF) 
for five named entities:

+ _Arabis laevigata_ (a synonym for _B. laevigata_)
+ _Borodina laevigata_ (which includes observations categorized as 
_Boechera laevigata_, a j.s. of _Borodina laevigata_)
+ _Cardamine concatenata_
+ _Cardamine diphylla_
+ _Pieris virginiensis_

Data stored in \*-raw.csv files are unprocessed, while data in \*-clean.csv 
files have been filtered to only include observations that meet the following 
criteria:

+ Observations include year information
+ Observations occur in or after 1960
+ Observations are not of eggs or larvae (_P. virginiensis_ only)
+ Observations are not "problematic" (see script for details)
+ Observations are between degrees 15 and 70 latitude and -99 and -45 degrees 
longitude
+ Additionally, only a single observation for each species for a specific date 
and geographic coordinates is retained (i.e. data are deduplicated).

Downloading and filtering occurs in scripts/data-download.R all raw and clean 
files are included in the archives gbif-raw.zip and gbif-clean.zip, 
respectively.

### Filtering

Given the focus of the current work is _P. virginiensis_, additional filters 
were applied to all data before analysis. Filtering is performed by 
scripts/data-prepare.R and filtered observations of _all_ species is in 
filtered-obs.csv. Filtering restricts observations to:

+ those occurring 2000-2022
+ those occurring within a 95% density envelope of _P. virginiensis_ 
observations
+ those occurring before the first day of summer of the year

## Climate data

Growing degree day data are from https://climatena.ca/spatialData (manual 
download) for two time periods 1961-1990 and 1991-2020.

Mean temperature (degrees Celsius) for the month of an observation and the 
preceding month are stored in temperature-obs.csv. The file has two columns: 
the unique identifier for the observation (gbifID) and the average temperature 
over those two months (avgt). File is created by scripts/data-weather.R, which 
downloads data from https://www.rcc-acis.org/docs_webservices.html and does the
conversion from Fahrenheit to Celsius.

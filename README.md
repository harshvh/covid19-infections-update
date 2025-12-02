# covid19-infections-update
Analysis and replication code for estimating COVID-19 infections in Brazil, India, United Kingdom, and United States over a period of one year

Publication link: https://www.medrxiv.org/content/10.1101/2025.11.21.25338950v1 

Directory structure

**`0-config.R`** : configuration file that sets data directories, sources base functions, and loads
required libraries
**`0-base-functions
`** : folder containing code files with functions used in the analysis
* `0-all-functions.R`
: R script containing all functions used in the analysis except the prior definitions
* `0-1-prior-functions.R`
: folder containing R scripts defining prior distributions for different
countries and waves

**`1-data
`** : folder containing data files

**`2-analysis
`** : folder containing analysis scripts.
* `1-obtain-priors-country.R`
: obtain priors for each country
* `2-est-expected-cases-monthly.R`
: run month-wise estimates for expected cases in each country
* `3-summarize-results.R`
: compiles and summarizes monthly estimated case counts for each country

**`3-figure-scripts
`** : folder containing figure scripts.

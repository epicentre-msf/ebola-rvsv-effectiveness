# Effectiveness of rVSV-ZEBOV Ebola vaccination during the 10th Ebola virus disease epidemic in the Democratic Republic of the Congo: a retrospective observational analysis

In this retrospective observational analysis we estimated the effectiveness of the rVSV-ZEBOV vaccine against Ebola virus disease during the 10th outbreak in the Democratic Republic of the Congo.

Code is spread across three folders, `scripts/`, `Rmd/` and `R/`, broadly for methods, results and background code, respectively.

## R

The folder `R/` contains any custom R functions called throughout the analysis: to load and clean data; to impute missing data; to sample imputed-matched populations; to fit `brms` models; and other miscellaneous functions.

## scripts

The folder `scripts/` contains all scripts to prepare and obtain the vaccine effectiveness estimates. These are numbered in the order in which they should be run, and should only be run if the results should be updated.

-   `01_impute_missing_delays.R` imputes missing vaccination-onset delays and other missing data. Output is an RDS file that contains all the imputed data sets.
-   `11_sample_matched_pop.R` sampled matched populations (matched by sex, age group, health zone and month of symptom onset) from the raw and imputed data. Output is an RDS file **per analysis** that contains the imputed-matched population samples.
-   `21_fit_models.R` fits the Bayesian logistic regression models. Output is an RDS file **per analysis** that contains a list of fitted `brms` models.
-   `22_summarise_models.R` reshapes the fitted `brms` models by pooling all posterior samples across the fits from each imputed-matched data set (iteration). Output is an RDS file **per analysis** that contains a data frame of posterior samples by iteration (`iter`).

## Rmd

The folder `Rmd/` contains Rmd files to generate results used in the manuscript. Values or figures within the reports will only change if the underlying data (population data or model samples) change. Files are numbered according to the approximate stage of the analysis that they correspond to.

-   `01_imputation_details.Rmd` compares the observed and imputed vaccination-onset delays and looks at the variation in estimated vaccine effectiveness between each imputed data set.
-   `11_matching_details.Rmd` explores the number and demographics of matched/unmatched cases and controls.
-   `12_population.Rmd` summarises the demographics (sex, age group, health zone and month of symptom onset) and EVD exposures of cases and non-cases in the raw, imputed and imputed-matched data.
-   `21_main_results.Rmd` summarises the main result: estimated vaccine effectiveness against EVD infection when vaccinated at least 10 days before symptom onset among individuals reporting contact with an EVD case.
-   `22_supp_results.Rmd` summarises the results of several supplementary and sensitivity analyses.

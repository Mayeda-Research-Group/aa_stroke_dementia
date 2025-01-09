# Effect of Incident Stroke on Dementia Risk Over 10 Years in a Cohort of Asian American and Non-Latino White Older Adults in California

This repo contains R scripts (`01_R`) and select output (`covbal_plots`) for the paper named Effect of incident stroke on the risk of dementia over 10 years of follow-up in a cohort of Asian American and non-Latino White older adults in California. 

Main scripts include: 

* Scripts 0.x consolidate datasets from various sources.
* Script 1 constructs the time-to-event dataset in a long format for analysis.
* Scripts 2.x performs multiple imputation for some demographic and health-related variables in the analytic dataset. 
* Scripts 3.x summarise descriptive information and explore options for constructing weighting models.
* Scripts 4.x conducts main analysis.
* Scripts 5.x cleans analysis results and prepare tables and figures.
* Script 6 performs a simple quantitative bias analysis, looking at impact of outcome misclassification.
* Script 7 provides some additional clarification during manuscript preparation.

Auxiliary scripts include: 

* `function_tbl1_prep.R` contains helper functions for preparing Table 1.
* `helper_functions.R` contains functions for main analysis.
* `tv_covbal_functions.R` contains modified functions from the confoundr R package.

The folder `covbal_plots` contains covariate balance plots before and after applying various weights. 

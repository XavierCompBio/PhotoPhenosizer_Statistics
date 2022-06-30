# Photo Phenosizer Statistical Pipeline
R scripts for the statistical processing of Photo Phenosizer outputs

# Instructions
1. Pick the corresponding R script that matches the experiment type:
* PP.R: Script used in the PhotoPhenosizer Paper. Compares two different treatment groups.
* One_Metric.R: Compares three different treatment groups and produce data for one cell dimension at a time.
* Combine_CSV.R: Combines all csv of same treatment group into one csv file

2. Open the script using Rstudio and alter the various placeholders such as file path to csv files, labels of treatment groups, etc.
3. Run the script and the directory should contain an density plot that showcases the metrics

R scripts are meant to be a guide. One can edit the script to accomodate one's need for statistical purposes.

*Currently work in progress before initial release, however the scripts work for experimental purposes*

# Phdproject

The files included in this repository are part of my PhD project, with data collection completed in July 2023 and data cleansing and analysis conducted thereafter between August 2023 and December 2023.  


The project consisted of collecting 3-weeks of intensitive and high frequency data from 37 study/intervention participants, from their mobile app (Ecological Momentary Assessment), Garmin fitness tracker (daily steps and sleep), and pre-study and post-study surveys.  Survey data was extracted from Qualtrics server, resulting in 3 Excel files (i.e., baseline survey, post-study survey, exit questionnaire).  Raw mobile app data, consisting of 44 csv files, were extracted from the Ilumivu servers.  Raw data from 47 files were merged into a long format.  The full dataset consisted of 587 records, representing 16 days of data captured (i.e., baseline surveys and Garmin steps and sleep (Day 0), intervention phase (Day 1-14), and post-study surveys (Day 15)).  

Included in this repository are the following files:
1) Example of data loading, merging, and cleansing in R, to prepare the initial dataset for further cleansing and analysis. (see file Thesis_Chapter7_data_cleansing.R)
2) Example of additional data cleansing to prepare data for statistical analysis.  Descriptive statistics are shown for steps count data, including visualisations (see file Thesis_Chapter7_final_data_cleansing_descriptivestats_Rmd)  Note:  I used an R Markdown file type.
3) Using machine learning in R to identify predictors to health behaviour change. (see file Thesis_Chapter8_ML_R_code.Rmd)
4) Using machine learning in Python to identify predictors to health behaviour change. (see file Thesis_Python_feature_selection_Dec23.ipynb)


